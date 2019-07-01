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

library(jsonlite)
library(tidyverse)
library(tidytext)
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)

setwd("~/ari_social_media/data/working/Blogs")
#read in data, remove all carriage returns from the "entry" column (body of blog text)
data <- fromJSON("armywivesdata.json")
data[,c("entry")]=stringr::str_replace_all(data[,c("entry")], "[\r\n]" , " ")
data[,c("entry")]=stringr::str_trim(data[,c("entry")])
entries <- tibble(text = data$entry)

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

#count of top 100 words
top100_words <- freq_terms(entries, 100, at.least=4, stopwords = c("didnt", "dont", "youre", "cant", "thats"))
top100_words

#create word cloud
set.seed(1234)
wordcloud(words = top200_words$WORD, freq = top200_words$FREQ, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "RdYlBu"))
