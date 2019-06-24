library(Rfacebook)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(qdap)

tokenapp <- #PUT YOUR APP TOKEN HERE

#number of comments to scrape per post
numcl <- 250

#function to get relevant info from list
getinfo <- function(id, list, table, field){ 
  return(list[id][[1]][[table]][field])
} 

#function to make a vector of the same length as the temporary comments table to attach a post id to the comment
makepostid <- function(id, list, table){  
  return(rep(id, getlength(id ,list, table)))
}  

#function to determine the number of comments actually scraped from a post
getlength <- function(id, list, table){
  if(is.null(list[id][[1]][[table]])){
    return(0)
  }
  else(
    return(nrow(list[id][[1]][[table]]))
  )
}

#function to make lists for comments
    #will not stop with error if post is not available
catchGetPost <- function(id, token, numcl){
  tryCatch({
    getPost(id, token, numcl)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#function to make a data frame of information on a post and id of someone who liked the post
makelikesdf <- function(pagedf, token, numcl){
  list <- pagedf$id %>% sapply(getPost, token, numcl)
  postid <- unname(unlist(pagedf$id %>% lapply(makepostid, list, "likes")))
  likerid <- unname(unlist(pagedf$id %>% lapply(getinfo, list, "likes", "from_id")))
  return(data_frame(Post_ID = postid, Liker_ID = likerid))
}

#THIS IS THE FUNCTION TO MAKE THE COMMENTS DATA FRAMES
    #pagedf is the data frame of posts made by getAllPosts in FacebookPageScraping.R
    #token is your Facebook App token
    #numcl is the number of comments you want to scrape per post
makecommentsdf <- function(pagedf, token, numcl){  
  list <- pagedf$id %>% sapply(catchGetPost, token, numcl)
  comments <- unname(unlist(pagedf$id %>% lapply(getinfo, list, "comments","message")))
  commentsid <- unname(unlist(pagedf$id %>% lapply(getinfo, list, "comments", "id")))
  commenterid <- unname(unlist(pagedf$id %>% lapply(getinfo, list, "comments", "from_id")))
  commentlikes <- unname(unlist(pagedf$id %>% lapply(getinfo, list, "comments", "likes_count")))
  postid <- unname(unlist(pagedf$id %>% lapply(makepostid, list, "comments")))
  
  return(data_frame(Post_ID = postid, Comment = comments, Likes = commentlikes, Comment_ID = commentsid, Commenter_ID = commenterid))
} 

#makes a word cloud from the data frame returned by makecommentsdf
    #comdf is the data frame returnedd by makecommentsdf
    #maxwords is the maximum number of words you want to appear in the wordcloud
makewordcloud <- function(comdf, maxwords){
  comdf$Comment <- iconv(comdf$Comment, "latin1", "ASCII", sub = "")
  comCorpus <- Corpus(VectorSource(comdf$Comment))
  comCorpus <- tm_map(comCorpus, PlainTextDocument)
  comCorpus <- tm_map(comCorpus, content_transformer(tolower))
  comCorpus <- tm_map(comCorpus, removePunctuation)
  comCorpus <- tm_map(comCorpus, removeWords, stopwords('english'))
  wordcloud(comCorpus, max.words = maxwords, random.order = FALSE)
}

#makes a word frequency table from the data frame returned by makecommentsdf
    #comdf is the data frame returned by makecommentsdf
    #this function removes punctuation and stopwords
makewordtable <- function(comdf){
  vec <- iconv(comdf$Comment, "latin1", "ASCII", sub = "")
  comcorpus <- Corpus(VectorSource(vec)) %>%
    tm_map(PlainTextDocument) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeWords, stopwords('english'))
  dtm <- DocumentTermMatrix(comcorpus)
  dtm2 <- as.matrix(dtm)
  frequency <- colSums(dtm2)
  frequency <- sort(frequency, decreasing=TRUE)
  freqtable <- data_frame(word = names(frequency), count = frequency)
  return(freqtable)
}

########################################################################
#SCRAPING

#OFFICIAL ARMY
USarmy <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/USarmy2016_07_06.csv", stringsAsFactors = FALSE)
goarmy <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/goarmy2016_07_06.csv", stringsAsFactors = FALSE)
WPUSMA <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/WPUSMA2016_07_06.csv", stringsAsFactors = FALSE)
ACC <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/ACC2016_07_06.csv", stringsAsFactors = FALSE)
USACEHQ <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/USACEHQ2016_07_06.csv", stringsAsFactors = FALSE)
cadd <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/cadd2016_07_06.csv", stringsAsFactors = FALSE)
afsc <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/afsc2016_07_06.csv", stringsAsFactors = FALSE)

offarmy <- rbind(USarmy, goarmy, WPUSMA, ACC, USACEHQ, cadd, afsc)
#write.csv(offarmy, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/offarmycomp.csv")

offarmy <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/offarmycomp.csv", stringsAsFactors = FALSE)
#offarmycom <- makecommentsdf(offarmy, tokenapp, numcl)

offarmycomA <- offarmycom %>% left_join(offarmy, by = c("Post_ID" = "id")) %>%
  dplyr::select(Post_ID, Comment, Likes, Comment_ID, Commenter_ID, created_time)
offarmycomA <- offarmycomA[!duplicated(offarmycomA),]
#write.csv(offarmycomA, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/offarmycommentsA.csv")


#ARMY BASES
usafc <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/usafc2016_07_06.csv", stringsAsFactors = FALSE)
fc <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/fc2016_07_06.csv", stringsAsFactors = FALSE)
usafh <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/usafh2016_07_06.csv", stringsAsFactors = FALSE)
fbf <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/fbf2016_07_06.csv", stringsAsFactors = FALSE)
afl <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/afl2016_07_06.csv", stringsAsFactors = FALSE)
flwm <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/flwm2016_07_06.csv", stringsAsFactors = FALSE)
fkky <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/fkky2016_07_06.csv", stringsAsFactors = FALSE)
fb <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/fb2016_07_06.csv", stringsAsFactors = FALSE)
 
#armybases <- rbind(usafc, fc, usafh, fbf, afl, flwm, fkky, fb)
#write.csv(armybases, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armybasescomp.csv")

armybases <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armybasescomp.csv", stringsAsFactors = FALSE)
#armybasescom <- makecommentsdf(armybases, tokenapp, numcl)

armybasescomA <- armybasescom %>% left_join(armybases, by = c("Post_ID" = "id")) %>%
  dplyr::select(Post_ID, Comment, Likes, Comment_ID, Commenter_ID, created_time)
armybasescomA <- armybasescomA[!duplicated(armybasescomA),]
#write.csv(armybasescomA, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armybasescommentsA.csv")


#UNOFFICIAL ARMY
csasfl <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/csasfl2016_07_06.csv", stringsAsFactors = FALSE)

#unoffarmy <- csasfl
#write.csv(unoffarmy, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffarmycomp.csv")

unoffarmy <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffarmycomp.csv", stringsAsFactors = FALSE)
#unoffarmycom <- makecommentsdf(unoffarmy, tokenapp, numcl)

unoffarmycomA <- unoffarmycom %>% left_join(unoffarmy, by = c("Post_ID" = "id")) %>%
  dplyr::select(Post_ID, Comment, Likes, Comment_ID, Commenter_ID, created_time)
#write.csv(unoffarmycomA, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffarmycommentsA.csv")


#UNOFFICIAL MILITARY
tl <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/tl2016_07_06.csv", stringsAsFactors = FALSE)
dm <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/dm2016_07_06.csv", stringsAsFactors = FALSE)
mdc <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/mdc2016_07_06.csv", stringsAsFactors = FALSE)
mn <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/mn2016_07_06.csv", stringsAsFactors = FALSE)

#unoffmil <- rbind(tl, dm, mdc, mn)
#write.csv(unoffmil, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffmilcomp.csv")

unoffmil <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffmilcomp.csv", stringsAsFactors = FALSE)
#unoffmilcom <- makecommentsdf(unoffmil, tokenapp, numcl)

unoffmilcomA <- unoffmilcom %>% left_join(unoffmil, by = c("Post_ID" = "id")) %>%
  dplyr::select(Post_ID, Comment, Likes, Comment_ID, Commenter_ID, created_time)
unoffmilcomA <- unoffmilcomA[!duplicated(unoffmilcomA),]
#write.csv(unoffmilcomA, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffmilcommentsA.csv")


#ARMY FAMILY
gap <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/gap2016_07_06.csv", stringsAsFactors = FALSE)
usafsf <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/usafsf2016_07_06.csv", stringsAsFactors = FALSE)
FMWR <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/FMWR2016_07_06.csv", stringsAsFactors = FALSE)
ARFP <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/ARFP2016_07_06.csv", stringsAsFactors = FALSE)
ACSF2 <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/ACSF22016_07_06.csv", stringsAsFactors = FALSE)

armyfam <- rbind(gap,usafsf,FMWR,ARFP,ACSF2)
#write.csv(armyfam, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armyfamcomp.csv")

armyfam <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armyfamcomp.csv", stringsAsFactors = FALSE)
#armyfamcom <- makecommentsdf(armyfam, tokenapp, numcl)

armyfamcomA <- armyfamcom %>% left_join(armyfam, by = c("Post_ID" = "id")) %>%
  dplyr::select(Post_ID, Comment, Likes, Comment_ID, Commenter_ID, created_time)
#write.csv(armyfamcomA, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armyfamcommentsA.csv")


#MILITARY FAMILY
mf <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/mf2016_07_06.csv", stringsAsFactors = FALSE)
NMSN <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/NMSN2016_07_06.csv", stringsAsFactors = FALSE)
TAPS <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/TAPS2016_07_06.csv", stringsAsFactors = FALSE)
bsm <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/bsm2016_07_06.csv", stringsAsFactors = FALSE)
wyr <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/wyr2016_07_06.csv", stringsAsFactors = FALSE)
fmw <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/fmw2016_07_06.csv", stringsAsFactors = FALSE)
IhsiM <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/IhsiM2016_07_06.csv", stringsAsFactors = FALSE)
MFSG <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/MFSG2016_07_06.csv", stringsAsFactors = FALSE)
SOV <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/SOV2016_07_06.csv", stringsAsFactors = FALSE)

milfam <- rbind(mf, NMSN, TAPS, bsm, wyr, fmw, IhsiM, MFSG, SOV)
#write.csv(milfam, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcomp.csv")

milfam <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcomp.csv", stringsAsFactors = FALSE)
#milfamcom <- makecommentsdf(milfam, tokenapp, numcl)

milfamcomA <- milfamcom %>% left_join(milfam, by = c("Post_ID" = "id")) %>%
  dplyr::select(Post_ID, Comment, Likes, Comment_ID, Commenter_ID, created_time)
#write.csv(milfamcomA, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcommentsA.csv")
