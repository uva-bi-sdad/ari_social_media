library(twitteR)
library(tm)
library(syuzhet)
library(ggplot2)
library(descr)
library(magrittr)
library(stringr)
library(wordcloud)
library(SnowballC)

token <- read.csv("~/token.csv",header = FALSE,stringsAsFactors = FALSE)

consumer_key <- token$V1[1]
consumer_secret <- token$V1[2]
access_token <- token$V1[3]
access_secret <- token$V1[4]

setup_twitter_oauth(consumer_key = consumer_key,
                    consumer_secret =  consumer_secret,
                    access_token =  access_token,
                    access_secret =  access_secret)

ar6701 <- searchTwitter(searchString = "AR 670-1",n = 3200,lang = 'en')
ar6701.df <- twListToDF(ar6701)

users_a <- ar6701.df$screenName
cat(users_a,file = "uniformusers.txt",sep = "\n")

users_b <- lapply(X = users_a,FUN = getUser)
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

getDesc <- function(user){
    return(user$description)
}

text1 <- lapply(X = users_b,FUN = getDesc)
for(i in 1:27){
    cat(text1[[i]],"------>",users_a[i],"\n **************",file = "descriptions.txt",sep = "\n",append = TRUE)
}

text <- append(text,users_a[2])

cat(text,file = "uniformarmy.txt",sep = "\n")
