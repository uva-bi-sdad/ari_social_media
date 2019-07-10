#2019-06-26
#Importing json file from Army Wives Blog
#original source: https://www.armywifenetwork.com/category/experience/

library(rjson)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(lubridate)
library(ggplot2)
library(jsonlite)
library(stringr)
#library(wordcloud)
#library(SentimentAnalysis)

setwd("~/git/ari_social_media/")

json_file <- fromJSON("data/working/community-embeddedness/armywivesdata.json")
ls(json_file)
myvector <- json_file$entry
newcol <- str_replace_all(myvector, "[\r\n]" , "")
newcol
