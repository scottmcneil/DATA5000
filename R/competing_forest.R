library(dplyr)
library(tidyr)
library(lubridate)
library(survival)
library(DBI)
library(randomForestSRC)

###### GET DATA FROM DB ######

conn <- dbConnect(RMySQL::MySQL(), user="root", password="", dbname="crunchbase", host="127.0.0.1", port=3306)
companies <- DBI::dbReadTable(conn = conn, name = 'cb_objects')
rounds <- DBI::dbReadTable(conn = conn, name = 'cb_funding_rounds')
acquisitions <- DBI::dbReadTable(conn = conn, name = 'cb_acquisitions')
ipos <- DBI::dbReadTable(conn = conn, name = 'crunchbase.cb_ipos')
investment <- DBI::dbReadTable(conn = conn, name = 'crunchbase.cb_investments')

###### SEPARATE OUT TRAINING AND TEST SETS ######

train_size <- 0.7
set.seed(42)

sample_companies <- rounds %>%
  mutate(date = ymd(funded_at)) %>%
  group_by(object_id) %>%
  summarise(first_fund = min(date)) %>%
  filter(year(first_fund) >= 1998) %>%
  inner_join(companies, by =c('object_id' = 'id')) %>%
  ungroup() %>%
  select(object_id, region, category_code, country_code) %>%
  sample_frac() %>%
  mutate(dummy = 1,
         dummy = cumsum(dummy),
         group = dummy < train_size*ceiling(max(dummy)))

###### BUILD EVENT/DATE DATA ######

date_rounds <- rounds %>%
  mutate(date = ymd(funded_at),
         raised_amount_usd = if_else(is.na(raised_amount_usd), 0, raised_amount_usd),
         dummy = 1,
         event = 'fund') %>%
  arrange(object_id, date) %>%
  group_by(object_id) %>%
  mutate(funding_to_date = cumsum(raised_amount_usd) - raised_amount_usd,
         rounds_to_date = cumsum(dummy) - dummy) %>%
  filter(rounds_to_date > 0) %>%
  ungroup() %>%
  select(object_id, date, event, raised_amount_usd, funding_to_date, rounds_to_date)

date_acquisitions <- acquisitions %>%
  mutate(date = ymd(acquired_at),
         event = 'acq') %>%
  rename(object_id = acquired_object_id) %>%
  group_by(object_id) %>%
  filter(min_rank(acquired_at) == 1) %>%
  ungroup() %>%
  select(object_id, date, event)

date_ipos <- ipos %>%
  mutate(date = ymd(public_at),
         event = 'ipo') %>%
  group_by(object_id) %>%
  filter(min_rank(public_at) == 1) %>%
  ungroup() %>%
  select(object_id, date, event)

###### BUILD FEATURES ######

regions <- sample_companies %>%
  group_by(region) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(region_rollup = ifelse(n > 1000, region, 'other') %>% as.factor()) %>%
  select(-n)

categories <- sample_companies %>%
  group_by(category_code) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(category_rollup = ifelse(n > 1000, category_code, 'other') %>% as.factor()) %>%
  select(-n)

###### COMBINE EVENTS ######

censor_date <- ymd('2014-01-01')
event_fact <- data.frame(event_lead = as.character(c('cens','fund','acq','ipo')),
                         event_fact = factor(0:3, labels = c('cens','fund','acq','ipo')))

all_events <- date_rounds %>%
  union_all(date_acquisitions) %>%
  union_all(date_ipos) %>%
  inner_join(sample_companies, by = 'object_id') %>%
  arrange(object_id, date) %>%
  group_by(object_id) %>%
  ### THIS IS WHERE THE EXTRA FEATURES GET ADDED
  mutate(funding_to_date = if_else(event != 'fund', lag(funding_to_date), funding_to_date),
         rounds_to_date = if_else(event != 'fund', lag(rounds_to_date), rounds_to_date),
         time = (lead(date, default = censor_date) - date)/91.25,
         time = as.numeric(time),
         event_lead = lead(event, default = 'cens'),
         raised_tot_ratio = ifelse(is.na(lag(raised_amount_usd)), NA,raised_amount_usd/funding_to_date),
         raised_prev_ratio = raised_amount_usd/lag(raised_amount_usd),
         last_interval_time = lag(time, n = 2),
         time_since_first_fund = cumsum(time) - time,
         SF = if_else(region == 'SF Bay', 1, 0),
         NYC = if_else(region == 'New York', 1, 0),
         US = if_else(country_code == 'US', 1, 0),
         bioclean = if_else(category_code == 'biotech' | category_code == 'cleantech', 1, 0),
         hardware = if_else(category_code == 'hardware', 1, 0)) %>%
  ungroup() %>%
  distinct(object_id, event, time, .keep_all = TRUE) %>%
  filter(!is.na(funding_to_date),
         event == 'fund',
         year(date) >= 1998) %>%
  left_join(event_fact, by = 'event_lead') %>%
  mutate(event_int = as.integer(event_fact) - 1)

# Pull out just training set
train_events <- all_events %>%
  filter(group == T)

###### FIT COMPETING RANDOM FOREST ######

rffit <- rfsrc(Surv(time, event_int) ~ funding_to_date + rounds_to_date + SF + NYC + US + bioclean + hardware +
                 raised_amount_usd + raised_tot_ratio + raised_prev_ratio,
               train_events, nsplit = 3, ntree = 1000)

