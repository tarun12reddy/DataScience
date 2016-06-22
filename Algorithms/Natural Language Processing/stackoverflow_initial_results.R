library(XML)
library(RCurl)
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
library(plyr)
library(caret)
library(randomForest)
library(psych)
library(NbClust)
library(proxy)
library(skmeans)

job_load <- function(path){
  filenames <- dir(path)
  job_details <- Corpus(DirSource(path))   
  return(job_details)
}

job_view <- job_load("unique_stackoverflow_jobs/")

# rmv_words <- c("able", "ability", "across", "analysis", "based", "buy", "can", "candidates", 
#                "chef", "closely", "company", "competitive", "conditions", "continuous", "ensure", "enterprise",
#                "equivalent", "etc", "field", "francisco", "full", "hallway", "health", "including", "industry", 
#                "interview", "like", "make", "money", "needs", "one", "part", "people", "professional",
#                "project", "provide", "quickly", "quiet", "relat", "role", "spring",  "step", "take", "us", "will", 
#                "within", "years", "york" )
rmv_words <- c("francisco")

review_clean <- function(docs){
  docs <- llply(docs, function(x){
    return(x)
  })
  docs <- Corpus(VectorSource(docs))
  for(j in seq(docs)){
    docs[[j]] <- gsub("C#", "Csharp", docs[[j]])
  }
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeNumbers)   
  docs <- tm_map(docs, tolower)   
  docs <- tm_map(docs, removeWords, c(stopwords("english"), rmv_words)) 
  for(j in seq(docs)){
    docs[[j]] <- gsub("applic", "application", docs[[j]])
    docs[[j]] <- gsub("architecture", "architect", docs[[j]])
    docs[[j]] <- gsub("amazonwebservices", "aws", docs[[j]])
    docs[[j]] <- gsub("bachelorâ..s", "bachelors", docs[[j]])
    docs[[j]] <- gsub("environ", "environment", docs[[j]])
    docs[[j]] <- gsub("expertise", "expert", docs[[j]])
    docs[[j]] <- gsub("highly", "high", docs[[j]])
    docs[[j]] <- gsub("scalable", "scale", docs[[j]])
    docs[[j]] <- gsub("written", "write", docs[[j]])
    docs[[j]] <- gsub("front end", "frontend", docs[[j]])
    docs[[j]] <- gsub("amazon web services", "aws", docs[[j]])
    docs[[j]] <- gsub("big data", "bigdata", docs[[j]])
    docs[[j]] <- gsub("web application", "webapplication", docs[[j]])
  }
  docs <- tm_map(docs, stemDocument)   
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, PlainTextDocument)   
  return(docs)
}
job_res <- review_clean(job_view)


review_plots <- function(res){
  dtm <- DocumentTermMatrix(res)   
  freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE) 
  wordcloud(names(freq), freq, max.words = 99)
  wf <- data.frame(word = names(freq), freq = freq) 
  p <- ggplot(subset(wf, freq > 200), aes(word, freq))    
  p <- p + geom_bar(stat="identity")   
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
  p <- p + ggtitle("Frequent Words")
  p
}
review_plots(job_res)


review_format <- function(res){
  dtm <- DocumentTermMatrix(res)   
  dtms <- removeSparseTerms(dtm, 0.9)
  dtms_inspect <- data.frame(inspect(dtms))
  #Final Check for Similar Words
  col_names_1 <- colnames(dtms_inspect)
  col_names_2 <- wordStem(col_names_1)
  final_dtms <- adply(unique(col_names_2), 1, .id = NULL, function(x){
    locs <- which(col_names_2 == x)
    col <- rep(0, dim(dtms_inspect)[1])
    for(loc in locs){
      col <- col + dtms_inspect[ ,loc]
    }
    if(length(locs) == 1){
      col_name <- col_names_1[loc]
    } else {
      col_name <- x
    }
    return(c(col, col_name))
  })
  final_dtms <- t(final_dtms)
  colnames(final_dtms) <- final_dtms[nrow(final_dtms), ]
  final_dtms <- final_dtms[-nrow(final_dtms), ]
  return(final_dtms)
}
job_data <- data.frame(review_format(job_res))
job_data <- apply(job_data, 2, as.numeric)
rownames(job_data) <- list.files("unique_stackoverflow_jobs/")
colnames_jobdata <- data.frame(colnames(job_data))
colnames(colnames_jobdata) <- "Word"
colnames_jobdata$Correct_Word <- NA
colnames_jobdata$Word_Type <- NA
colnames_jobdata <- data.frame(colnames_jobdata)
write.csv(colnames_jobdata, "segment_words.csv", row.names = F, col.names = T, quote = FALSE)