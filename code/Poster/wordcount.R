library(twitteR)
library(tm)
library(syuzhet)
library(ggplot2)
library(ggrepel)
library(descr)
library(magrittr)
library(stringr)
library(wordcloud)
library(SnowballC)
library(lubridate)
library(xlsx)
library(dplyr)

# dict <- read.xlsx(file = "words_uniq_nonstemmed_grp_clean.xlsx",1)
#save(dict,file = "dictionary.RData")
load("~/tweet_objects.RData")
load(file = "~/sdal/projects/dod_social_media/dictionary.RData")

#clean text
df <- timelines
df <- rbind(df,ar6701.df,trans.df)
    #remove hyperlinks
df$text <- gsub(pattern = " ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)",
                replacement = "",
                x = df$text)
    #remove emoji and other special chars except #
df$text <- gsub(pattern = "[^a-zA-Z0-9#-/ ]",
                replacement = "",
                x = df$text)
    #put a space back in before the #
df$text <- gsub(pattern = "#",
                replacement = " #",
                x = df$text)
    #make everything lowercase
df$text <- tolower(df$text)
df$text <- removePunctuation(x = df$text,preserve_intra_word_dashes = T)

df$text <- removeWords(x = df$text,words = stopwords())



tags <- unlist(strsplit(df$text,split = " "))[which(grepl("#",unlist(strsplit(df$text,split = " "))))]
tags.df <- data.frame(table(tags),stringsAsFactors = F)

total_corpus <- Corpus(VectorSource(df$text))

counts <- data.frame()
get_Counts <- function(test,str){
    splits <- unlist(strsplit(x = str,split = " "))
    return(length(splits[which(splits %in% test)]))
}

dict$counts <- sapply(X = dict$words,FUN = get_Counts,str = unlist(strsplit(df$text,split = " ")))
dict$counts[which(dict$counts == 0)] <- NA
dict <- na.omit(dict)



gettags <- function(str){
    tags1 <- unlist(strsplit(x = str,split = " "))
    tags1 <- tags1[which(grepl("#",tags1))]
    tags1 <- paste(tags1,collapse = " ")
}

df$hashtags <- sapply(X = df$text,FUN = gettags)
df$hashtags[which(df$hashtags == "")] <- NA

df$created <- as.Date(df$created)
dates <- unique(df$created)

matrix2 <- matrix1

matrix2 <- matrix2[, colSums(matrix2 != 0) > 50]

sds1 <- c(0)
for(i in 2:length(matrix4)){
    sds1 <- append(sds1,sd(matrix4[,i]))
}
sds1 <- data.frame(sds1)
sds1$words <- colnames(matrix4)

make_timeline <- function(date,word){
    temp <- df %>% filter(created == date)
    return(get_Counts(test = word,str = unlist(strsplit(x = temp$text,split = " "))))
}

healthchart <- data.frame(dates,stringsAsFactors = F)
healthchart$counts <- sapply(X = healthchart$dates,FUN = make_timeline,word = "health")
ggplot(healthchart) + geom_line(mapping = aes(x = dates,y = counts)) +
    scale_x_date(limits = as.Date(c('2014-01-01','2016-07-18')))

womenchart <- data.frame(dates,stringsAsFactors = F)
womenchart$counts <- sapply(X = womenchart$dates,FUN = make_timeline,word = c("women",
                                                                              "woman",
                                                                              "female"))
ggplot(womenchart) + geom_line(mapping = aes(x = dates,y = counts)) + 
    scale_x_date(limits = as.Date(c('2014-01-01','2016-07-18'))) 

schoolchart <- data.frame(dates,stringsAsFactors = F)
schoolchart$counts <- sapply(X = schoolchart$dates,FUN = make_timeline,word = c("school",
                                                                                "college",
                                                                                "education"))
ggplot(schoolchart) + geom_line(mapping = aes(x = dates,y = counts)) + 
    scale_x_date(limits = as.Date(c('2014-01-01','2016-07-18'))) + scale_y_log10()

transchart <- data.frame(dates,stringsAsFactors = F)
transchart$counts <- sapply(X = transchart$dates,FUN = make_timeline,word = "transgender")
ggplot(transchart) + geom_line(mapping = aes(x = dates,y = counts)) + 
    scale_x_date(limits = as.Date(c('2014-01-01','2016-07-18'))) #+ scale_y_log10()

transchart1 <- data.frame(dates,stringsAsFactors = F)
transchart1$counts <- sapply(X = transchart1$dates,FUN = make_timeline,word = c("transgender",
                                                                                "trans",
                                                                                "transsexual",
                                                                                "Transgender",
                                                                                "Trans",
                                                                                "Transsexual"))
transchart1 <- transchart1[order(x = transchart1$dates,decreasing = T),]
transchart1$counts <- transchart1$counts/datefreq$Freq
ggplot(transchart1) + geom_line(mapping = aes(x = dates,y = counts)) + 
    scale_x_date(limits = as.Date(c('2014-01-01','2016-07-18')),breaks = "1 month") + scale_y_log10()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
    

diversitychart <- data.frame(dates,stringsAsFactors = F)
diversitychart$counts <- sapply(X = diversitychart$dates,FUN = make_timeline,word = c("diversity"))
diversitychart <- diversitychart[order(x = diversitychart$dates,decreasing = T),]
diversitychart$counts <- diversitychart$counts/datefreq$Freq
ggplot(diversitychart) + geom_line(mapping = aes(x = dates,y = counts)) + 
    scale_x_date(limits = as.Date(c('2014-01-01','2016-07-18')),breaks = "1 month") + scale_y_log10()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Diversity")

grantchart <- data.frame(dates,stringsAsFactors = F)
grantchart$counts <- sapply(X = grantchart$dates,FUN = make_timeline,word = c("grant"))
grantchart <- grantchart[order(x = grantchart$dates,decreasing = T),]
grantchart$counts <- grantchart$counts/datefreq$Freq
ggplot(grantchart) + geom_line(mapping = aes(x = dates,y = counts)) + 
    scale_x_date(limits = as.Date(c('2014-01-01','2016-07-18')),breaks = "1 month") + scale_y_log10()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("grant")

privacychart <- data.frame(dates,stringsAsFactors = F)
privacychart$counts <- sapply(X = privacychart$dates,FUN = make_timeline,word = c("privacy"))
privacychart <- privacychart[order(x = privacychart$dates,decreasing = T),]
privacychart$counts <- privacychart$counts/datefreq$Freq
ggplot(privacychart) +
    geom_line(mapping = aes(x = dates,y = counts),color = "#00aced") +
    scale_x_date(limits = as.Date(c('2014-01-01','2016-07-18')),date_breaks = "1 month") +
    scale_y_log10()+
    theme(axis.text.x = element_text(angle = 90, vjust = .5,size = 12)) +
    labs(y = "Relative Frequency", x = "Date")

wearchart <- data.frame(dates,stringsAsFactors = F)
wearchart$counts <- sapply(X = wearchart$dates,FUN = make_timeline,word = c("wear",
                                                                                "uniform",
                                                                                "Uniform",
                                                                                "Wear"))
wearchart1 <- wearchart %>% filter(year(dates) >= 2014)
ggplot(wearchart1) + 
    geom_line(mapping = aes(x = dates,
                            y = counts/10000),
              color = "#00aced") +
        scale_y_log10() +
    theme_bw() +
    labs(x = "Date",
         y = "Ratio") +
    theme_bw() + 
    theme(panel.border = element_blank(),
          axis.title = element_text(size = 20),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     size = 12),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    scale_x_date(date_breaks = "1 month")



allfbcomfildic <- read.csv(file = "~/sdal/projects/dod_social_media/Will/allfbcomfildic.csv", stringsAsFactors = FALSE)
filt <- allfbcomfildic %>% filter(pattern == "wear")
wordvec <- sds$pattern
wordvec <- c(wordvec, "assault", "college", "wear", "school", "tax", "rape", "women")
for(i in 1:length(wordvec)){  
    temp <- allfbcomfildic %>% filter(pattern == wordvec[i])
    assign(x = wordvec[i], ggplot(temp) + 
               geom_line(aes(x = as.Date(date), y = ratio), color = "#3b5998") +
               labs(title = as.character(wordvec[i]), x = "Date", y = "Ratio") +
               theme_bw() + 
               theme(panel.border = element_blank(),
                     axis.text.x = element_text(angle = 45, hjust = 1, size = 4)) +
               scale_x_date(date_breaks = "1 month"))
}


ggplot(temp) + 
    geom_line(aes(x = as.Date(date), y = ratio), color = "#3b5998") +
    labs(title = as.character(wordvec[i]), x = "Date", y = "Ratio") +
    theme_bw() + 
    theme(panel.border = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 4)) +
    scale_x_date(date_breaks = "1 month"))

policychart <- data.frame(dates,stringsAsFactors = F)
policychart$counts <- sapply(X = policychart$dates,FUN = make_timeline,word = c("Policy",
                                                                                "policy",
                                                                                "Policies",
                                                                                "policy"))
ggplot(policychart) + geom_line(mapping = aes(x = dates,y = counts)) +
    scale_x_date(limits = as.Date(c('2014-01-01','2016-07-18'))) + scale_y_log10()

vetschart <- data.frame(dates,stringsAsFactors = F)
vetschart$counts <- sapply(X = vetschart$dates,FUN = make_timeline,word = c("veterans",
                                                                            "vet",
                                                                            "vets",
                                                                            "Veteran",
                                                                            "Veterans",
                                                                            "Vets"))
vetschart <- vetschart[order(x = vetschart$dates,decreasing = T),]
vetschart$counts <- vetschart$counts/datefreq$Freq
ggplot(vetschart) + geom_line(mapping = aes(x = dates,y = counts)) +
    scale_x_date(limits = as.Date(c('2014-01-01','2016-07-18'))) + scale_y_log10()

armychart <- data.frame(dates,stringsAsFactors = F)
armychart$counts <- sapply(X = armychart$dates,FUN = make_timeline,word = c("Army",
                                                                            "army",
                                                                            "Army's",
                                                                            "army's",
                                                                            "armies",
                                                                            "Armies"))
armychart <- armychart[order(x = armychart$dates,decreasing = T),]
armychart$counts <- armychart$counts/datefreq$Freq
ggplot(armychart) + geom_line(mapping = aes(x = dates,y = counts)) +
    scale_x_date(limits = as.Date(c('2014-01-01','2016-07-18'))) + scale_y_log10()

matrix1 <- data.frame(dates,stringsAsFactors = F)
for(i in 1:length(dict$words)){
    word1 <- dict$words[i]
    matrix1 <- cbind(matrix1,assign(x = word,value = sapply(dates,FUN = make_timeline, word = word1)))
}
colnames(matrix1) <- c("dates",as.character(dict$words))
save(matrix1, "twitter_social_media/matrix.RData")


matrix3 <- matrix1
for(i in 2:length(matrix3)){
    matrix3[,i] <- matrix3[,i]/datefreq$Freq
}



ggplot(matrix4) + geom_line(mapping = aes(x = dates,y = veteran))
ggplot(matrix4) +
    geom_line(mapping = aes(x = dates,y = male)) +
    geom_line(mapping = aes(x = dates,y = female),color = "blue")
ggplot(matrix4) + 
    geom_line(mapping = aes(x = dates,y = women + woman + female)) +
    labs(x = "Date", y = "Relative Frequency")

ggplot(matrix4) + 
    geom_line(mapping = aes(x = dates,y = assault)) +
    labs(x = "Date", y = "Relative Frequency", title = "Assault") + scale_x_date(breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(matrix4) + 
    geom_line(mapping = aes(x = dates,y = school)) +
    labs(x = "Date", y = "Relative Frequency", title = "School") + scale_x_date(breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(matrix4) + 
    geom_line(mapping = aes(x = dates,y = woman + women + female)) +
    labs(x = "Date", y = "Relative Frequency", title = "Women") + scale_x_date(breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(matrix4) + 
    geom_line(mapping = aes(x = dates,y = college)) +
    labs(x = "Date", y = "Relative Frequency", title = "College") + scale_x_date(breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(matrix4) + 
    geom_line(mapping = aes(x = dates,y = college + school + semester)) +
    labs(x = "Date", y = "Relative Frequency", title = "College and School") + scale_x_date(breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(matrix4) + 
    geom_line(mapping = aes(x = dates,y = tax)) +
    labs(x = "Date", y = "Relative Frequency", title = "Tax") + scale_x_date(breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(matrix4) + 
    geom_line(mapping = aes(x = dates,y = rape)) +
    labs(x = "Date", y = "Relative Frequency", title = "Rape") + scale_x_date(breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(matrix4) + 
    geom_line(mapping = aes(x = dates,y = assault + rape)) +
    labs(x = "Date", y = "Relative Frequency", title = "Assault and Rape") + scale_x_date(breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(matrix4) + 
    geom_line(mapping = aes(x = dates,y = healthcare + insurance)) +
    labs(x = "Date", y = "Relative Frequency", title = "Health Care") + scale_x_date(breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(matrix4) + 
    geom_line(mapping = aes(x = dates,y = wear)) +
    labs(x = "Date", y = "Relative Frequency", title = "Wear") + scale_x_date(breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(radarchart)
library(jsonlite)

hashtag <- "#army"
sentiments <- get_nrc_sentiment(filter(df,grepl(pattern = hashtag,x = df$hashtags,ignore.case = T))$text)

sumSents <- as.data.frame(colSums(sentiments))

sumSents$Names <- row.names(sumSents)

colnames(sumSents) <- c("Sums","Type")

sumSents1 <- sumSents[2:1]

c <- grDevices::col2rgb(c("#5d89b4"))
chartJSRadar(scores = sumSents1,
             labelSize = 60,
             colMatrix = c)


genPoint <- function(sents){
    max <- max(sents$Sums)
    sums <- sents$Sums/max
    circ <- seq(0,359,36)
    names <- colnames(sents)
    
    
}

sentimentstot <- get_nrc_sentiment(df$text)

sumSentstot <- as.data.frame(colSums(sentimentstot))

sumSentstot$Names <- row.names(sumSentstot)

colnames(sumSentstot) <- c("Sums","Type")

#ggplot(sumSents) + geom_bar(mapping = aes(x = Type,weight = Sums)) + ggtitle(hashtag)


sumSentstot1 <- sumSentstot[2:1]

c <- grDevices::col2rgb(c("#00aced"))
c <- cbind(c,c(59,89,152))
as.matrix(data.frame(c(204,24,30),c(59,89,152),c(0,172,237)))
chartJSRadar(scores = sumSentstot1,labelSize = 60,colMatrix = c)



sents <- c()
for(i in 1:nrow(tags.df2)){
    sentimentstemp <- get_nrc_sentiment((filter(df,grepl(pattern = tags.df2$tags[i],x = df$hashtags))$text))
    sumSentstemp <- as.data.frame(colSums(sentimentstemp))
    sumSentstemp$Names <- row.names(sumSentstemp)
    colnames(sumSentstemp) <- c("Sums","Type")
    pos <-  sumSentstemp$Sums[10]
    sents <- append(x = sents,values = pos)
}
tags.df2$pos <- sents

sents <- c()
for(i in 1:nrow(tags.df3)){
    sentimentstemp <- get_nrc_sentiment((filter(df,grepl(pattern = tags.df3$tags[i],x = df$hashtags))$text))
    sumSentstemp <- as.data.frame(colSums(sentimentstemp))
    sumSentstemp$Names <- row.names(sumSentstemp)
    colnames(sumSentstemp) <- c("Sums","Type")
    neg <- sumSentstemp$Sums[9]
    sents <- append(x = sents,values = neg)
}
tags.df3$neg <- sents


sents <- append(sents,rep(0,(nrow(tags.df) - 1140)))

tags.df$posneg <- sents


ggplot(tags.df4) +
    geom_text(mapping = aes(x = posneg,y = Freq,label = tags)) + scale_y_log10()



tags.df5 <- tags.df2[1:100,]




Math.svnthrt <- function(x) {
    sign(x) * abs(x)^(1/7)
}

tags.df5$x = runif(n = 99,min = -1,max = 1)
tags6 <- tags.df5[-which.max(tags.df5$Freq),]
tags.df5 <- tags.df5[-which(tags.df5$tags == "#kilroysconversation"),]

ggplot(tags.df5) +
    geom_text_repel(mapping = aes(x = x,
                                  y = Math.fthrt(posneg)+.5,
                                  label = tags,
                                  size = Freq),color = "#660000",segment.size = 0) + 
    #ylim(c(-125,375)) +
    #scale_y_log10() +
    #geom_jitter(height = .01,width = .01)
    labs(y = "Net Sentiment (Positive - Negative)",x = "",size = "Frequency") + theme(legend.position = "none",axis.text.x = element_blank(),
                                                                axis.ticks = element_blank(),
                                                                axis.title.x=element_blank(),
                                                                panel.background = element_blank()) +
    scale_size(range = c(3,7)) + 
    geom_hline(yintercept = 0)

ggplot(tags.df6) +
    geom_hline(yintercept = 0)+
    geom_text_repel(mapping = aes(x = x,
                                  y = Math.fthrt(posneg) + .4,
                                  label = tags,
                                  size = Freq),color = "#660000",segment.size = 0, force = 15) + 
    #ylim(c(-125,375)) +
    #scale_y_log10() +
    #geom_jitter(height = .01,width = .01)
    labs(y = "Net Sentiment (Positive - Negative)",x = "",size = "Frequency") + theme(legend.position = "none",axis.text.x = element_blank(),
                                                                axis.ticks = element_blank(),
                                                                axis.title.x=element_blank(),
                                                                panel.background = element_blank()) +
    scale_size(range = c(4,9)) 
    
set.seed(5)
ggplot(tags.df5) +
    geom_text_repel(mapping = aes(x = x,
                                  y = y,
                                  label = tags,
                                  size = Freq,
                                  color = Math.svnthrt(posneg)),segment.size = 0, force = 5) + 
    #ylim(c(-125,375)) +
    #scale_y_log10() +
    #geom_jitter(height = .01,width = .01)
    labs(y = "",x = "",size = "Frequency") + theme(axis.text.x = element_blank(),
                                                                                      axis.ticks = element_blank(),
                                                                                      axis.title.x=element_blank(),
                                                                                      panel.background = element_blank(),
                                                                                      axis.text.y = element_blank()) +
    scale_size(range = c(4,9)) +
    scale_color_gradient(low = "#CC0000",high = "#0000AA")


wordcloud()


tags.df4 <- tags.df3[1:100,]

ggplot(tags.df4) +
    geom_text_repel(mapping = aes(x = neg,
                            y = pos,
                            label = tags,
                            size = log10(Freq/10)),segment.size = 0) + 
    #xlim(c(0,2500)) + ylim(c(0,2500)) +
    scale_x_log10() + scale_y_log10() +
    stat_function(fun = function(x){x})+
    labs(y = "Positive",x = "Negative",size = "Frequency") + theme(legend.position = "none")
    

