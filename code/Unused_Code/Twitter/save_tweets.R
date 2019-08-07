#save tweets to a file

library(twitteR)
library(tm)
library(syuzhet)
library(ggplot2)
library(descr)
library(magrittr)
library(stringr)
library(wordcloud)
library(SnowballC)

token <- read.csv("~/token.csv",header = FALSE,stringsAsFactors = FALSE)

consumer_key <- token$V1[1]
consumer_secret <- token$V1[2]
access_token <- token$V1[3]
access_secret <- token$V1[4]

setup_twitter_oauth(consumer_key = consumer_key,
                    consumer_secret =  consumer_secret,
                    access_token =  access_token,
                    access_secret =  access_secret)


#show number of searches used
searches <- getCurRateLimitInfo(resources = "search")
print(searches)

tag <- readline(prompt = "Twitter Search: ")
number <- as.numeric(readline(prompt = "Number of tweets: "))
while(is.na(number)){
  number <- as.numeric(readline(prompt = "Please enter a numeric value: "))
}

dat <- tag %>% searchTwitter(n = number,lang = "en")


users <- sapply(dat, function(x) x$getScreenName())
tagtext <- sapply(dat, function(x) x$getText())

tagtext_corpus <- Corpus(VectorSource(tagtext))

#remove twitter specific stuff
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("@\\w+ *", "", s)))
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("#\\w+ *", "", s)))

tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(x) str_replace_all(x, "[\n]" , " ")))


tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(tolower)) 
tagtext_corpus <- tm_map(tagtext_corpus, removePunctuation)
tagtext_corpus <- tm_map(tagtext_corpus, function(x)removeWords(x,stopwords()))
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("htt\\w+ *", "", s)))
# 

tagtext_cleaned <-data.frame(text=unlist(sapply(tagtext_corpus, `[`, "content")), 
                                         stringsAsFactors=F)
#fileConn<-file("corpus.txt")
cat(tagtext_cleaned$text,file = "",append = TRUE,sep = "\n")
#close(fileConn)
