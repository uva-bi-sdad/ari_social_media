library(twitteR)
library(tm)
library(syuzhet)
library(ggplot2)
library(descr)
library(magrittr)
library(stringr)
library(wordcloud)
library(SnowballC)

setwd("~/sdal/projects/dod_social_media/Twitter")
load("r_data/twitter.RData")

setup_twitter_oauth(consumer_key = consumer_key,
                    consumer_secret =  consumer_secret,
                    access_token =  access_token,
                    access_secret =  access_secret)

us_users <- readLines(con = file("text/users.txt"))

dat <- lapply(us_users,getUser)

text <- c()
pb <- txtProgressBar(min = 1,max = length(users_b),char = "/",style = 3)
for(index in 1:length(users_b)){
    
    setTxtProgressBar(pb, index)
    
    
    if(grepl(pattern = "army",x = users_b[[index]]$description)|grepl(pattern = "Army",x = users_b[[index]]$description) | grepl(pattern = "veteran",x = users_b[[index]]$description)){
        text <- append(text,users_b[[index]]$screenName)
    }
    #cat(text,sep = "\n", file = "usersvet.txt", append = TRUE)
    
    
}
close(pb)

for(user in armus2){
    temp <- 
}



getDesc <- function(user){
    return(user$description)
}