#better twitter analysis
#Searches twitter by userprompted hashtag
library(twitteR)
library(tm)
library(syuzhet)
library(ggplot2)
library(descr)
library(magrittr)
library(stringr)
library(wordcloud)
library(SnowballC)

# Reads in an OAuth token, make your own
# There are instructions on the wiki
token <- read.csv("~/token.csv",header = FALSE,stringsAsFactors = FALSE)

consumer_key <- token$V1[1]
consumer_secret <- token$V1[2]
access_token <- token$V1[3]
access_secret <- token$V1[4]

setup_twitter_oauth(consumer_key = consumer_key,
                    consumer_secret =  consumer_secret,
                    access_token =  access_token,
                    access_secret =  access_secret)


# Shows number of searches used
searches <- getCurRateLimitInfo(resources = "search")
print(searches)

# Reads in the hashtag and the number of tweets requested, up to 3200
# That's an API limit, requesting more won't break it, but it also won't work
tag <- readline(prompt = "Twitter Search: ")
number <- as.numeric(readline(prompt = "Number of tweets: "))
while(is.na(number)){
  number <- as.numeric(readline(prompt = "Please enter a numeric value: "))
}

# Searches twitter for the given tag using twitteR's function
dat <- tag %>% searchTwitter(n = number,lang = "en")

# Takes the list of tweets returned from searchTwitter and returns a dataframe
dat.df <- twListToDF(dat)

# Save the data to a file
save(dat.df,file = paste("r_data/",gsub(pattern = "#",replacement = "",x = tag),"_",as.character(Sys.Date()),sep = ""))

# Gets the texts and begins to prepare them for analysis
tagtext <- dat.df$text

# Creates a Corpus with each tweet as a document
tagtext_corpus <- Corpus(VectorSource(tagtext))

# Remove usernames and hashtags from text used for sentiment analysis
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("@\\w+ *", "", s)))
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("#\\w+ *", "", s)))

# Takes multiline tweets and puts them on one line
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(x) str_replace_all(x, "[\n]" , " ")))

#Remove hyperlinks from tweets
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(function(s) gsub("htt\\w+ *", "", s)))

# Makes tweets lowercase
tagtext_corpus <- tm_map(tagtext_corpus, content_transformer(tolower)) 
# Removes punctuation
tagtext_corpus <- tm_map(tagtext_corpus, removePunctuation)
# Removes stop words
tagtext_corpus <- tm_map(tagtext_corpus, function(x)removeWords(x,stopwords()))

# Makes the two main objects of text mining,
# the document term matrix and the term document matrix
dtm <- DocumentTermMatrix(tagtext_corpus)
tdm <- TermDocumentMatrix(tagtext_corpus)

# Shows word frequency
freq <- colSums(as.matrix(dtm))

#Creates a reproducible word cloud
set.seed(142)   
wordcloud(names(freq), freq, max.words=100) 

# Gives number of unique words
length(freq)  

# Creates an order for the words from most frequent to least
ord <- order(freq,decreasing = T)

# Most Frequent
freq[head(ord)]
#Least Frequent
freq[tail(ord)]

# Makes a data frame for ggplot
wf <- data.frame(word=names(freq), freq=freq)  

# Plots frequencies of the top couple words
plot <- ggplot(subset(wf,freq>(length(freq))/30), aes(word, freq))
plot <- plot + geom_bar(stat="identity") + xlab(tag)
plot <- plot +theme(axis.text.x=element_text(angle=0, hjust=1)) + coord_flip()
print(plot)

# Sets the encoding of the tweets 
dat.chr <- toUTF8(dat.df$text)

# Calculates sentiment scores for tweets as a whole
sentiments <- get_nrc_sentiment(dat.chr)

# Sums it up to plot
sumSents <- as.data.frame(colSums(sentiments))

# Makes labels
sumSents$Names <- row.names(sumSents)

# Descriptive names
colnames(sumSents) <- c("Sums","Type")

# Makes a barplot 
plot <- ggplot(sumSents) + geom_bar(mapping = aes(x = Type,y = Sums),stat = "identity") + xlab(label = tag) +coord_flip()
print(plot)


