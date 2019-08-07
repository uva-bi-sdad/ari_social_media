# Get live data from twitter as tweets are tweeted
library(twitteR)
library(tm)
library(syuzhet)
library(ggplot2)
library(descr)
library(magrittr)
library(stringr)
library(wordcloud)
library(SnowballC)
library(streamR)
library(RCurl)
library(rjson)
library(RJSONIO)
library(ROAuth)


setwd(dir = "~/sdal/projects/dod_social_media/twitter_social_media/")
load(file = "r_data/twitter.Rdata")


# setup OAuth for streaming if you haven't already
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

consumerKey <- consumer_key # From dev.twitter.com
consumerSecret <- consumer_secret # From dev.twitter.com

# Create OAuth object
my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

# Save it to a file so you don't need to do it anymore
save(my_oauth, file = "my_oauth.Rdata")

# Load it
load(file = "r_data/my_oauth.RData")


filterStream(file.name = "text/livedata.json",track = c("army", "US Army"),language = "en",tweets = 15,oauth = my_oauth) #follow = getid
tweets <- readTweets(tweets = "livedata.json")
tweets <- parseTweets(tweets = tweets)
