install.packages("tidyverse")
install.packages("tidytext")
install.packages("syuzhet")
library(tidyverse) # metapackage with lots of helpful functions
library(tidytext) # tidy implimentation of NLP methods
library(syuzhet)
news <- read_csv("C:/os/fake.csv")
news$type<-gsub("bs","fake",news$type)                 
news$type<-gsub("conspiracy","fake",news$type)          
#while others are real
news$type<-gsub("bias","real",news$type)              
news$type<-gsub("satire","real",news$type)
news$type<-gsub("hate","real",news$type)
news$type<-gsub("junksci","real",news$type)
news$type<-gsub("state","real",news$type)
news %>% group_by(type) %>% summarise(count=n())
news$exc <- sapply(news$text, function(x) length(unlist(strsplit(as.character(x), "\\!+")))) #count exclamation
news$que <- sapply(news$text, function(x) length(unlist(strsplit(as.character(x), "\\?+")))) #count question marks
news %>% group_by(type) %>% summarise(exclamations=sum(exc))
news %>% group_by(type) %>% summarise(QuestionMarks=sum(que))
boxplot(exc ~ type,news,ylim=c(0,20),ylab="",col=c("red","orange"))
boxplot(que ~ type,news,ylim=c(0,20),col=c("red","orange")) 
#function for finding words in each text
terms<- function(fake, text_column, group_column){
  
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  # get the count of each word in each review
  words <- news %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>%
    ungroup()
  
  # get the number of words per text
  #total_words <- words %>%
  #group_by(!!group_column) %>%
  #summarize(total = sum(n))
  
  # combine the two dataframes we just made
  
  return (words)
}
df<-terms(news,text,type) 
boxplot(n ~ type,df,log="y",xlab="type",ylab="number of words",col=c("green","pink"))
sentiment<-get_nrc_sentiment(news$text)
sentiment
df1<-sentiment[c(9,10)]
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
} 
df1$negative<-normalize(df1$negative)
df1$positive<-normalize(df1$positive) 
news<-cbind(news,df1) 
neg_sd<-news %>% group_by(type) %>% summarise(neg_sd=sd(negative))
pos_sd<-news %>% group_by(type) %>% summarise(pos_sd=sd(positive))
neg_med<-news %>% group_by(type) %>% summarise(neg_med=median(negative))
pos_med<-news %>% group_by(type) %>% summarise(pos_med=median(positive)) 
dfr2<-data.frame(neg_sd)
dfr1<-data.frame(pos_sd)
dfr3<-data.frame(neg_med)
dfr4<-data.frame(pos_med) 
t1<-merge(dfr1,dfr2)
t2<-t(t1)
t2
t3<-merge(dfr4,dfr3)
t4<-t(t3)
t4
