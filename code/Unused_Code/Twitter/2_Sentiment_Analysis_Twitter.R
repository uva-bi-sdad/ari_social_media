library(tm)      # Text mining
library(syuzhet) # Sentiment Analysis

load(file = file.choose())
# Load any saved data frame here and set text equal to a vector of all
# the tweets
text <- dat.df$text

# Creates a corpus of all the tweets, with each tweet as a document
text_corpus <- Corpus(VectorSource(text))

# Remove usernames and hashtags from text used for sentiment analysis
text_corpus <- tm_map(text_corpus,
                      content_transformer(function(s) gsub("@\\w+ *",
                                                           "",
                                                           s)))
text_corpus <- tm_map(text_corpus,
                      content_transformer(function(s) gsub("#\\w+ *",
                                                           "",
                                                           s)))

# Takes multiline tweets and puts them on one line
text_corpus <- tm_map(text_corpus,
                      content_transformer(function(x) str_replace_all(x,
                                                                      "[\n]" ,
                                                                      " ")))

#Remove hyperlinks from tweets
text_corpus <- tm_map(text_corpus,
                      content_transformer(function(s) gsub("htt\\w+ *",
                                                           "",
                                                           s)))

# Makes tweets lowercase
text_corpus <- tm_map(text_corpus, content_transformer(tolower)) 
# Removes punctuation
text_corpus <- tm_map(text_corpus, removePunctuation)
# Removes stop words
text_corpus <- tm_map(text_corpus, function(x)removeWords(x,
                                                          stopwords()))

dtm <- DocumentTermMatrix(text_corpus)
tdm <- TermDocumentMatrix(text_corpus)

# Shows word frequency
freq <- colSums(as.matrix(dtm))

#Creates a reproducible word cloud
set.seed(142)   
wordcloud(words = names(freq),
          freq = freq,
          max.words=100) 

# Gives number of unique words
length(freq)  

# Creates an order for the words from most frequent to least
ord <- order(freq,
             decreasing = T)

# Most Frequent
freq[head(ord)]
#Least Frequent
freq[tail(ord)]


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
plot <- ggplot(sumSents) + 
    geom_bar(mapping = aes(x = Type,y = Sums),stat = "identity") +
    xlab(label = tag) +
    coord_flip()
print(plot)
