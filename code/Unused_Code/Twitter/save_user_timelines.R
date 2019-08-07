library(twitteR)
library(tm)
library(syuzhet)
library(ggplot2)
library(descr)
library(magrittr)
library(stringr)
library(wordcloud)
library(SnowballC)
setwd("~/")
token <-
    read.csv("~/token.csv",
             header = FALSE,
             stringsAsFactors = FALSE)

consumer_key <- token$V1[1]
consumer_secret <- token$V1[2]
access_token <- token$V1[3]
access_secret <- token$V1[4]

setup_twitter_oauth(consumer_key = consumer_key,
                    consumer_secret =  consumer_secret,
                    access_token =  access_token,
                    access_secret =  access_secret)

ucon <- file("~/sdal/projects/dod_social_media/Twitter/text/uniformarmy.txt")

usernames <- readLines(ucon)
close(ucon)

#userlist <- lapply(X = usernames,FUN = getUser)

usernames <- gsub(pattern = "@",replacement = "",x = usernames)
setwd(dir = "~/sdal/projects/dod_social_media/Twitter/")
#timeline <- userTimeline(usernames[1])
vec <- usernames
writetofile <- function(usernames){
    setup_twitter_oauth(consumer_key = consumer_key,
                        consumer_secret =  consumer_secret,
                        access_token =  access_token,
                        access_secret =  access_secret)
    Sys.sleep(time = 2)
    text <- ""
    name <- usernames[1]
    timeline <<- try(userTimeline(user = name,
                                  n = 3200),silent=TRUE)
    if(inherits(timeline,'try-error')){ 
        
    }else{
        timeline <<- strip_retweets(timeline, strip_manual = TRUE, strip_mt = TRUE)
        
        Sys.sleep(time = 2)
        pb <- txtProgressBar(min = 1,max = length(timeline),char = "/",style = 3)
        for(index in 1:length(timeline)){
            
            setTxtProgressBar(pb, index)
            
            text <- timeline[[index]]$text
            
            cat(text,sep = "\n", file = "Output/texts4.txt", append = TRUE)
            
            
        }
    }
    
    
    
    close(pb)
    if(length(usernames) == 1){
        return()
    }
    record <<- usernames[-1]
    Recall(usernames = usernames[-1])
}
vec <- record
writetofile(usernames = vec)
tweets <- lapply(X = timeline, FUN = twListToDF)



timeline[[4]]$text
length(timeline)

# tweettext <- sapply(X = tweets, FUN = gettext)
#
# tweettext <- tweettext[1,]
#
# tweettext <- t(tweettext)

texts <- lapply(X = tweettext, FUN = cbind)

#test <- userTimeline(usernames[2])



getList <- function(name){
    
    host <- getUser(name)
    
    goodlist <- host$getFollowers()
    
    sorted <- goodlist[rev(order(sapply(goodlist,FUN = followersCount)))]
    text <- c()
    pb <- txtProgressBar(min = 1,max = length(sorted),char = "/",style = 3)
    for(index in 1:length(sorted)){
        
        setTxtProgressBar(pb, index)
        
        
        if(grepl(pattern = "army",x = sorted[[index]]$description) | grepl(pattern = "veteran",x = sorted[[index]]$description)){
            text <- append(text,sorted[[index]]$screenName)
        }
        #cat(text,sep = "\n", file = "usersvet.txt", append = TRUE)
        
        
    }
    close(pb)
    return(text)
}

vec <- getList(name = "LTGRickLynch") # your user here
sorted[[1]]


for (i in 1:length(usernames)) {
    print(i)
    userTimeline(usernames[i])
}

tagtext <- readLines(con = file("texts2.txt"))

tagtext_corpus <- Corpus(VectorSource(tagtext))

#remove twitter specific stuff
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("@\\w+ *", "", s)))
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("#\\w+ *", "", s)))

tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(x) str_replace_all(x, "[\n]" , " ")))


tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(tolower)) 
tagtext_corpus <- tm_map(tagtext_corpus, removePunctuation)
tagtext_corpus <- tm_map(tagtext_corpus, function(x)removeWords(x,stopwords()))
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("htt\\w+ *", "", s)))
# 
dtm <- DocumentTermMatrix(tagtext_corpus)
tdm <- TermDocumentMatrix(tagtext_corpus)

freq <- colSums(as.matrix(dtm))

set.seed(142)   
wordcloud(names(freq), freq, max.words=100) 


length(freq)  

ord <- order(freq)

freq[head(ord)]
freq[tail(ord)]

wf <- data.frame(word=names(freq), freq=freq)  

plot <- ggplot(subset(wf,freq>(length(freq))/15), aes(word, freq))
plot <- plot + geom_bar(stat="identity") + xlab(tag)
plot <- plot +theme(axis.text.x=element_text(angle=0, hjust=1)) + coord_flip()
print(plot)

dat.chr <- toUTF8(tagtext)

sentiments <- get_nrc_sentiment(dat.chr)

sumSents <- as.data.frame(colSums(sentiments))

sumSents$Names <- row.names(sumSents)

colnames(sumSents) <- c("Sums","Type")

plot <- ggplot(sumSents) + geom_bar(mapping = aes(x = Type,y = Sums),stat = "identity") + xlab(label = tag) +coord_flip()
print(plot)


