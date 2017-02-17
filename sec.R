
library(tm)
library(topicmodels)
content_corpus1<-Corpus(VectorSource(read.csv("techcrunch_posts.csv",
              stringsAsFactors = FALSE)$content),readerControl =list( language="en"))

spacing<- content_transformer(function(x, pattern) { 
  
  return (gsub(pattern, " ", x))})

content_corpus2<-tm_map(content_corpus1,spacing,"[^0-9A-Za-z]")
content_corpus2 <-tm_map(content_corpus2,content_transformer(tolower))
new_stopwords <- c("can", "say","one","way","use","and","this","go","re","ve",
                   "they","around","that","with","you","actual",
                   "let","for","just","percent","the","there",
                   "also","howev","tell","will",
                   "much","need","take","tend","even",
                   "like","particular","rather","said",
                   "get","well","make","ask","come","end",
                   "first","two","help","often","may",
                   "might","see","someth","thing","point",
                   "post","look","right","now","think","'ve",
                   "'re","anoth","put","set","new","good",
                   "want","sure","kind","larg","yes,","day","etc","month",
                   "quit","sinc","attempt","lack","seen","awar",
                   "littl","ever","moreov","though","found","abl",
                   "enough","far","earli","away","achiev","draw",
                   "last","never","brief","bit","entir","brief",
                   "great","lot","aaa","aaaaaaaaaa")
mod_content_text1 <- tm_map(content_corpus3, removeWords, new_stopwords)
mod_content_text1<-tm_map(mod_content_text1,content_transformer(gsub),pattern="advertis",replacement="ad")
mod_content_text1<-tm_map(mod_content_text1,content_transformer(gsub),pattern="websit",replacement="web")
mod_content_text1<-tm_map(mod_content_text1,content_transformer(gsub),pattern="site",replacement="web")
mod_content_text1<-tm_map(mod_content_text1,content_transformer(gsub),pattern="research",replacement="search")



mod_content_text1 <- tm_map(mod_content_text1, removeWords, stopwords("english"))
mod_content_text1<-tm_map(mod_content_text1,stemDocument)
mod_content_text1<-tm_map(mod_content_text1,removeNumbers)
mod_content_text1<-tm_map(mod_content_text1,removePunctuation)
mod_content_text1<-tm_map(mod_content_text1,stripWhitespace)


#Caculating  terms frequency(term must be between 3 to 12 characters) and minfreq=1
content_term_matrix1<-DocumentTermMatrix(mod_content_text1,control = list(minDocFreq=1, wordLengths=c(2,12)))
#the total number  of words in each document
total_words_inDoc <-slam::row_sums(content_term_matrix1,na.rm=TRUE)
#Counting the total occurences of terms in the docs
content_term_freq<-slam::col_sums(content_term_matrix1,na.rm=TRUE)
write.csv(content_term_freq,"content_term_freq.csv")
#Remove zero entry documents
content_term_matrix2<-content_term_matrix1[total_words_inDoc>0,]
#content_term_freq<-slam::col_sums(content_term_matrix1,na.rm=T)
write.csv(content_term_freq,"doc_term_freq.csv")
LDA_modeling <-LDA(content_term_matrix2,5)
LDA_modeling_head_terms<-as.matrix(terms(LDA_modeling,10))



