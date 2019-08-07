# Test of Web scraping
library(rvest)

library(stringr)
library(dplyr)



home <- "https://en.wikipedia.org/wiki/Main_Page"

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

pages <- c()
links <- c()
tree_gen <- function(home){
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  
  
  homepage <- read_html(home)

  print("1 works")
  list <- homepage %>% html_nodes('a')
  print("2 works")
  list <- str_extract(list, url_pattern)
  print("3 works")
  list <- na.omit(list)
  print("4 works")
  list <- as.data.frame(list,stringsasfactors = FALSE)
  print("5 works")
  list <- filter(list,grepl(pattern = "en.wikipedia.org",list) == TRUE)
  print("6 works")
  list$list <- as.character(list$list)
  print("7 works")
  list <- list$list
  print("8 works")
  pages <- append(x = pages,values = list)
  print("9 works")
  Sys.sleep(runif(n = 1,min = 3,max = 15))
  print("10 works")
  
  sapply(X = list,FUN = tree_gen)
    
  
}




