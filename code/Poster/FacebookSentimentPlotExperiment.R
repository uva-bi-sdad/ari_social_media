#library(devtools)
#install_github("sfeuerriegel/SentimentAnalysis", "sfeuerriegel")
library(SentimentAnalysis)
library(parallel)
library(foreach)
library(doParallel)

unoffarmycom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffarmycommentsA.csv", stringsAsFactors = FALSE)
milfamcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcommentsA.csv", stringsAsFactors = FALSE)
armyfamcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armyfamcommentsA.csv", stringsAsFactors = FALSE)
unoffmilcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffmilcommentsA.csv", stringsAsFactors = FALSE)
armybasescom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armybasescommentsA.csv", stringsAsFactors = FALSE)
offarmycom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/offarmycommentsA.csv", stringsAsFactors = FALSE)

unoffarmycom$Comment <- iconv(unoffarmycom$Comment, "latin1", "ASCII", sub = "")
milfamcom$Comment <- iconv(milfamcom$Comment, "latin1", "ASCII", sub = "")
armyfamcom$Comment <- iconv(armyfamcom$Comment, "latin1", "ASCII", sub = "")
unoffmilcom$Comment <- iconv(unoffmilcom$Comment, "latin1", "ASCII", sub = "")
armybasescom$Comment <- iconv(armybasescom$Comment, "latin1", "ASCII", sub = "")
offarmycom$Comment <- iconv(offarmycom$Comment, "latin1", "ASCII", sub = "")

######################
#unofficial army sentiment table
unoffarmysent <- analyzeSentiment(unoffarmycom$Comment)

ggplot(unoffarmysent) +
  geom_point(aes(x = PositivityGI, y = NegativityGI, color = SentimentGI), size = 3) +
  scale_colour_gradient(limits=c(-1, 1), low="red", high="green") +
  labs(
    title = "Sentiment of 'Unofficial Army' Posts",
    x = "Negativity",
    y = "Positivity"
  )

##################################################
seq <- seq(1, nrow(milfamcom), length.out = 70)
init <- data_frame()
for(i in 2:length(seq)){ 
  temp <- milfamcom[seq[i-1]:seq[i],]
  tempsent <- analyzeSentiment(temp$Comment)
  init <- rbind(init, tempsent)
}

#write.csv(init, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcomsent.csv")

milfamcomsent <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcomsent.csv", stringsAsFactors = FALSE)

ggplot(milfamcomsent, aes(x = PositivityGI, y = NegativityGI)) +
  geom_jitter(aes(color = SentimentGI), size = 3, alpha = .25) +
  scale_colour_gradient(limits=c(-1, 1), low="red", high="green") +
  labs(
    title = "Sentiment of 'Military Family' Posts",
    x = "Negativity",
    y = "Positivity"
  )

#####################################


