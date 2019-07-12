#get users who use a certain hastag
install.packages("twitteR")
install.packages("tm")
install.packages("syuzhet")
install.packages("ggplot2")
install.packages("descr")
install.packages("magrittr")
install.packages("rtweet")
install.packages("tidytext")

library(twitteR)
library(tm)
library(syuzhet)
library(ggplot2)
library(descr)
library(magrittr)

#read token from file
token <- read.csv("~/token.csv",header = FALSE,stringsAsFactors = FALSE)

consumer_key <- token$V1[1]
consumer_secret <- token$V1[2]
access_token <- token$V1[3]
access_secret <- token$V1[4]




#connect to twitter api

setup_twitter_oauth(consumer_key = consumer_key,
                    consumer_secret =  consumer_secret,
                    access_token =  access_token,
                    access_secret =  access_secret)

print(getCurRateLimitInfo(resources = "search"))

tag <- readline(prompt = "Twitter Search: ")
number <- as.numeric(readline(prompt = "Number of tweets: "))
while(is.na(number)){
    number <- as.numeric(readline(prompt = "Please enter a numeric value: "))
}

dat <- tag %>% searchTwitter(n = number,lang = "en")
dat <- strip_retweets(dat, strip_manual = TRUE, strip_mt = TRUE)

dat.df <- twListToDF(dat)

users <- dat.df$screenName

sapply(X = users, FUN = cat,file = "taguser.txt", sep = "\n",append = TRUE)
