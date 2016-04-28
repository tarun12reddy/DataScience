library(wordcloud)
library(tm)
library(RColorBrewer)
library(ggplot2)
library(SnowballC)
library(biclust)
library(cluster)
library(igraph)
library(fpc)
library(Rcampdf)

#Reading Data

review_load <- function(data, type){
  path <- file.path(getwd(), data, type)   
  filenames <- dir(path)
  reviews <- Corpus(DirSource(path))   
  return(reviews)
}

#Cleaning Data
review_clean <- function(docs){
  #Remove HTML format
  docs <- llply(docs, function(x){
                        return(gsub("<br /><br />", " ", x[["content"]]))
                })
  docs <- Corpus(VectorSource(docs))
  #Remove Punctuation
  docs <- tm_map(docs, removePunctuation)
  #Removing numbers
  docs <- tm_map(docs, removeNumbers)   
  #LowerCase
  docs <- tm_map(docs, tolower)   
  #StopWords
  docs <- tm_map(docs, removeWords, stopwords("english"))   
  #StemWords
  docs <- tm_map(docs, stemDocument)   
  #WhiteSpaces
  docs <- tm_map(docs, stripWhitespace)
  #Extract PlainText
  docs <- tm_map(docs, PlainTextDocument)   
  return(docs)
}

reviews <- review_load(data = "train", type = "pos")
res <- review_clean(reviews)

# Stage the Data
dtm <- DocumentTermMatrix(res)   
tdm <- TermDocumentMatrix(res)   
freq <- colSums(as.matrix(dtm))   
ord <- order(freq)
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   


#### Classification problem
## Collect Features
#1. From Word Cloud get most frequent words for postive and negative and frequency
#2. Number of Positive Words belonging to different category
#3. Number of Negative Words belonging to different category
#4. Total number of words
#5. Use these features to develop a randomforest approach