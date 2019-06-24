library(twitteR)
library(tm)
library(syuzhet)
library(ggplot2)
library(descr)
library(magrittr)
library(stringr)
library(wordcloud)
library(SnowballC)
setwd("~/")
token <-
    read.csv("~/token.csv",
             header = FALSE,
             stringsAsFactors = FALSE)

consumer_key <- token$V1[1]
consumer_secret <- token$V1[2]
access_token <- token$V1[3]
access_secret <- token$V1[4]

setup_twitter_oauth(consumer_key = consumer_key,
                    consumer_secret =  consumer_secret,
                    access_token =  access_token,
                    access_secret =  access_secret)


trans <- searchTwitteR(searchString = "transgender+army",
                       n = 3200,
                       lang = "en")

trans.df <- twListToDF(trans)


users <- trans.df$screenName
tweets <- trans.df$text

tagtext_corpus <- Corpus(VectorSource(tweets))

#remove twitter specific stuff
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("@\\w+ *", "", s)))
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("#\\w+ *", "", s)))

tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(x) str_replace_all(x, "[\n]" , " ")))


tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(tolower)) 
tagtext_corpus <- tm_map(tagtext_corpus, removePunctuation)
tagtext_corpus <- tm_map(tagtext_corpus, function(x)removeWords(x,stopwords()))
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("htt\\w+ *", "", s)))
# 
dtm <- DocumentTermMatrix(tagtext_corpus)
tdm <- TermDocumentMatrix(tagtext_corpus)

freq <- colSums(as.matrix(dtm))

set.seed(142)   
wordcloud(names(freq), freq, max.words=100)


length(freq)  

ord <- order(freq)

freq[head(ord)]
freq[tail(ord)]

wf <- data.frame(word=names(freq), freq=freq)  

plot <- ggplot(subset(wf,freq>(length(freq))/15), aes(word, freq))
plot <- plot + geom_bar(stat="identity") + xlab("")
plot <- plot +theme(axis.text.x=element_text(angle=0, hjust=1)) + coord_flip()
print(plot)

dat.chr <- toUTF8(tweets)

sentiments <- get_nrc_sentiment(dat.chr)

sumSents <- as.data.frame(colSums(sentiments))

sumSents$Names <- row.names(sumSents)

colnames(sumSents) <- c("Sums","Type")

plot <- ggplot(sumSents) + geom_bar(mapping = aes(x = Type,y = Sums),stat = "identity") + xlab(label = "") +coord_flip()
print(plot)

