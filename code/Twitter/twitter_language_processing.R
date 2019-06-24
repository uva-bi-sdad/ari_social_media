#language processing
library(lsa)
library(NLP)
library(textcat)
library(tm)


library(openNLP)
library(openNLPdata)
library(openNLPmodels.en)
library(magrittr)
setwd("~/sdal/projects/dod_social_media/twitter_social_media/")
text <- readLines(con = file("text/texts2.txt"))


clean <- gsub(pattern = "@\\w+ *",replacement = "",x = text)

clean <- gsub(pattern = "#\\w+ *",replacement = "",x = clean)

clean <- gsub(pattern = "http\\S+\\s*",replacement = "",x = clean)

clean <- iconv(gsub("\\n", " ", clean), to="ASCII", sub="")
clean.s <- as.String(clean)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()

a2 <- annotate(clean.s, list(sent_token_annotator,word_token_annotator))

words <- AnnotatedPlainTextDocument(s = clean.s,annotations = a2)

words(words) %>% head(10)

person_ann <- Maxent_Entity_Annotator(kind = "person")

test <- textcat(x = text, method = "ALPD")
