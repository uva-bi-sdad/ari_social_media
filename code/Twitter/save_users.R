library(twitteR)
library(tm)
library(syuzhet)
library(ggplot2)
library(descr)
library(magrittr)
library(stringr)
library(wordcloud)
library(SnowballC)
library(lubridate)

# Reads in OAuth credentials, make your own using my wiki entry and save them to
# your home folder
token <-
    read.csv("~/token.csv",
             header = FALSE,
             stringsAsFactors = FALSE)

consumer_key <- token$V1[1]
consumer_secret <- token$V1[2]
access_token <- token$V1[3]
access_secret <- token$V1[4]

# Connect to twitter
setup_twitter_oauth(
    consumer_key = consumer_key,
    consumer_secret =  consumer_secret,
    access_token =  access_token,
    access_secret =  access_secret
)

# Find a popular user and get all their followers
host <- getUser(user = "GoArmy")
peeps <- host$getFollowers()

# Select only people who say they are in the army
getarmy <- function(us) {
    if (grepl(pattern = "army", x = us$description) |
        grepl(pattern = "veteran", x = us$description) |
        grepl(pattern = "army wife", x = us$description)) {
        return(us)
    } else{
        
    }
}

# Select from followers of @UsArmy those who say they are in the army
armus <- lapply(X = peeps, FUN = getarmy)

# Removes the nulls induced by my method
armus2 <- Filter(Negate(is.null), armus)

# Create empty data frame
timelines <- data.frame(stringsAsFactors = FALSE)

# A function that goes through a list of users and returns up to their last
# 3200 tweets, waiting upon rate limit and skipping private accounts
getTimelines <- function(list) {
    # Connect to twitter, it likes to reconnect each time
    setup_twitter_oauth(
        consumer_key = consumer_key,
        consumer_secret =  consumer_secret,
        access_token =  access_token,
        access_secret =  access_secret
    )
    # Show how many are left
    print(length(list))
    # Either get a list of tweets from a user, skip a private user, or wait
    # for the rate limit
    temp <- tryCatch(
        expr = {
            # Get user
            userTimeline(
                user = list[[1]]$screenName,
                n = 3200,
                includeRts = F,
                excludeReplies = T
            )
        },
        error = function(cond,list. = list) {
            # Wait
            time_ <- Sys.time()
            attr(time_, "tzone") <- "UTC"
            show <- getCurRateLimitInfo()
            wait <- 10 + as.numeric(as.interval(x = show$reset[39] - time_, start = time_))
            print(paste("Waiting for",wait,"seconds."))
            Sys.sleep(wait)
            getTimelines(list = list.[-1])
        },
        warning = function(war,list. = list) {
            # Wait
            time_ <- Sys.time()
            attr(time_, "tzone") <- "UTC"
            show <- getCurRateLimitInfo()
            wait <- 10 + as.numeric(as.interval(x = show$reset[39] - time_, start = time_))
            print(paste("Waiting for",wait,"seconds."))
            Sys.sleep(wait)
            getTimelines(list = list.[-1])
        }
    )
    # Add user's tweets to the list
    if(length(temp) > 0){
        timelines <<- rbind(timelines, twListToDF(temp))
    }
    # Save them to a file just in case
    save(timelines, file = "tweet_objects.RData")
    # If it can go to a next value, it will, otherwise, it ends
    if (length(list) > 1) {
        Recall(list = list[-1])
    } else{
        return()
    }
}

getTimelines(armus3)
save(timelines, file = "tweet_objects1.RData")
