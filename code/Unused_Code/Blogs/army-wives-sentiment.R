#2019-06-27
#Sentiment & text analysis of Army wives blog
#original source: https://www.armywifenetwork.com/category/experience/

#install.packages("jsonlite")
#install.packages("tidyverse")
#tidyverse has ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
#install.packages("tidytext")
#install.packages("tm")
#install.packages("qdap")
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
#install.packages("SentimentAnalysis")
#install.packages("sentimentr")
#install.packages("lexicon")


library(jsonlite)
library(tidyverse)
library(tidytext)
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)
#library(SentimentAnalysis)
library(sentimentr)
library(magrittr)
library(dplyr)
library(lexicon)

setwd("~/ari_social_media/data/working/Blogs")
#read in data, remove all carriage returns from the "entry" column (body of blog text)
data <- fromJSON("armywivesdata.json")
data[,c("entry")]=stringr::str_replace_all(data[,c("entry")], "[\r\n]" , " ")
data[,c("entry")]=stringr::str_trim(data[,c("entry")])
entries <- tibble(text = data$entry)
entries2 <- tibble(text = data$entry, tag = data$tags)

#separate every single word into a different row
#going from 1141 observations of blog posts to 294048 words from those posts
entries <- entries %>%
  unnest_tokens(word, text)

#removing stop words (like "and", "if", etc.)
entries <- entries %>%
  anti_join(stop_words)


#give us a count of the top 50 words, removing contractions that aren't included in stop_words
top50_words <- freq_terms(entries, 50, at.least=4, stopwords = c("didnt", "dont", "youre", "cant", "thats"))
top50_words

#count of top 200 words
top200_words <- freq_terms(entries, 200, at.least=4, stopwords = c("didnt", "dont", "youre", "cant", "thats"))
top200_words

#count of top 100 words
top100_words <- freq_terms(entries, 100, at.least=4, stopwords = c("didnt", "dont", "youre", "cant", "thats"))
top100_words

#create word cloud
set.seed(1234)
wordcloud(words = top200_words$WORD, freq = top200_words$FREQ, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "RdYlBu"))


#join bing library of words and their associated sentiments
bing <- get_sentiments("bing")
entries_bing <- entries %>%
  anti_join(stop_words) %>%
  inner_join(bing)

#create separate lists of positive and negative words from posts
entries_bing_pos <- entries_bing %>%
  filter(sentiment == "positive")
top200_pos <- freq_terms(entries_bing_pos$word, 200, at.least=4, stopwords = c("didnt", "dont", "youre", "cant", "thats"))

entries_bing_neg <- entries_bing %>%
  filter(sentiment == "negative")
top200_neg <- freq_terms(entries_bing_neg$word, 200, at.least=4, stopwords = c("didnt", "dont", "youre", "cant", "thats"))


#create word cloud of top 200 negative and positive words
#positive words
set.seed(1234)
wordcloud(words = top200_pos$WORD, freq = top200_pos$FREQ, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "RdYlBu"))

#negative words
set.seed(1234)
wordcloud(words = top200_neg$WORD, freq = top200_pos$FREQ, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "RdYlBu"))



#sentiment analysis of tagged blog posts
entries2 <- tibble(text = data$entry, tag = data$tags)
#entries2 <- entries2 %>%
#  unnest_tokens(word, text) %>%
#  anti_join(stop_words)

#of the PCS tagged posts, how positive/neutral/negative are they?
pcs <- entries2[grepl("PCS", entries2$tag),]
sentiment <- analyzeSentiment(pcs$text)
pcs_sentiment <- convertToDirection(sentiment$SentimentQDAP)
plot(pcs_sentiment)

#of the Army life tagged posts, how positive/neutral/negative are they?
army_life <- entries2[grepl("Army life", entries2$tag),]
sentiment <- analyzeSentiment(army_life$text)
army_life_sentiment <- convertToDirection(sentiment$SentimentQDAP)
plot(army_life_sentiment)

#of the Army life tagged posts, how positive/neutral/negative are they?
army_life <- entries2[grepl("Army life", entries2$tag),]
sentiment <- analyzeSentiment(army_life$text)
army_life_sentiment <- convertToDirection(sentiment$SentimentQDAP)
plot(army_life_sentiment)

#of the deployment tagged posts, how positive/neutral/negative are they?
deployment <- entries2[grepl("deployment", entries2$tag),]
sentiment <- analyzeSentiment(deployment$text)
deployment_sentiment <- convertToDirection(sentiment$SentimentQDAP)
plot(deployment_sentiment)

#of the advice tagged posts, how positive/neutral/negative are they?
advice <- entries2[grepl("advice", entries2$tag),]
sentiment <- analyzeSentiment(advice$text)
advice_sentiment <- convertToDirection(sentiment$SentimentQDAP)
plot(advice_sentiment)

mental_health <- entries2[grepl("mental health", entries2$tag),]
sentiment <- analyzeSentiment(mental_health$text)
mental_health_sentiment <- convertToDirection(sentiment$SentimentQDAP)
plot(mental_health_sentiment)

#attempt to use sentimentr package instead
#This code takes the first post, splits it into a vector of its sentences, then finds the sentiment of each sentence.
example_post <- get_sentences(entries2[1,1])
example_post_sentiment <- sentiment(example_post)
mean(example_post_sentiment$sentiment)
terms <- extract_sentiment_terms(example_post)


#testing the highlight function in sentimentr with the first blog post in entries data table
a <- entries[1,]
a %>%
  mutate(review = get_sentences(text)) %$%
  sentiment_by(review) %>%
  highlight()


#adding in another dataset of words to add to sentimentr as a dictionary
setwd("~/ari_social_media/data/working/Blogs")
sentiwords <- read.table("SentiWordNet_3.0.0.txt", fill = TRUE)
