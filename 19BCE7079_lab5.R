#NAME   : HARIPRASAD K K
#REG_NO : 19BCE7079
#LAB_NO : LAB-5

library(janeaustenr)
library(dplyr)
library(tm)
library(tidytext)
library(tidyverse)
library(qdapTools)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(textdata)

#Import  Dataset

df=read.csv("updated_books.csv")
head(df)
tail(df)
summary(df)
#(a)Remove "<br>" from each sentence
df$Content=gsub("<br>","",as.character(df$Content))
#cleaning
options(warn=-1)
corpus <- Corpus(VectorSource(df$Content))
head(corpus)
#(b)Perform Tokenization AND (c)Remove Stopwords
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
#(d)Removing "writers" column
df=subset(df,select=-c(writers))

myDtm <- TermDocumentMatrix(corpus) #create a DTM
myDtm
#find associations

associations=findAssocs(myDtm,'identity',0.15)
associations=as.data.frame(associations)
#require(tidytext)
terms=Terms(myDtm) 
head(terms)
ap_td = tidy(myDtm) #convert DTM in a "tidy" form
ap_td
# sentiment analysis using tidy text in bing lexicon

#(e)Using the bing lexicon categorize the columns into positive negative and neutral
df1<- ap_td %>% inner_join(get_sentiments("bing"), by = c(term = "word"))
head(df1)
tail(df1)
     