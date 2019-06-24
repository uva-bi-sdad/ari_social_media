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

### There is a method to do this
# #method to turn search response into a dataframe
# stat_to_DF <- function(stat){
#   #makes empty dataframe
#   stat.df <- data.frame(matrix(data = NA, nrow = length(stat),ncol = 1),stringsAsFactors = FALSE)
#   #gives a descriptive name
#   colnames(stat.df) <- c("Text")
#   
#   #creates an empty vector to fill from status object
#   vec <- c()
#   
#   #puts all the tweets into a vector
#   for(index in 1:length(stat)){
#     vec <- append(vec,stat[[index]]$text)
#   }
#   #puts the vector into the data frame
#   stat.df$Text <- vec
#  
#   return(stat.df)
# }




#get search term from user
tag <- readline(prompt = "Twitter Search: ")
number <- as.numeric(readline(prompt = "Number of tweets: "))
while(is.na(number)){
  number <- as.numeric(readline(prompt = "Please enter a numeric value: "))
}

#search twitter for it
dat <- tag %>% searchTwitter(n = number,lang = "en")


dat.df <- twListToDF(dat)




dat.chr <- toUTF8(dat.df$text)

sentiments <- get_nrc_sentiment(dat.chr)

sumSents <- as.data.frame(colSums(sentiments))

sumSents$Names <- row.names(sumSents)

colnames(sumSents) <- c("Sums","Type")

plot <- ggplot(sumSents) + geom_bar(mapping = aes(x = Type,y = Sums),stat = "identity") + xlab(label = tag) +coord_flip()
print(plot)
#t <- get_sentiment_chart(tagtosearch =  "#wwjd")






