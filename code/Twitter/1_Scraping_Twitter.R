## Using Twitter's API to gather data

#Libraries
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


#show number of searches used
searches <- getCurRateLimitInfo(resources = "search")
print(searches)

#get search term from user
tag <- readline(prompt = "Twitter Search: ")
number <- as.numeric(readline(prompt = "Number of tweets: "))
while(is.na(number)){
    number <- as.numeric(readline(prompt = "Please enter a numeric value: "))
}

#search twitter for it
dat <- tag %>% searchTwitter(n = number,lang = "en")


dat.df <- twListToDF(dat)

save(dat.df,file = paste("~/sdal/projects/dod_social_media/Twitter/Output/",
                         as.character(Sys.Date()),"_",tag,".RData",sep = ""))