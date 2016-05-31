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

stack_url <- 'https://stackoverflow.com/jobs/feed?searchTerm=big+data'
stack_feed <- getURL(stack_url)
stack_feed_xml <- xmlParse(stack_feed)
stack_feed_urls <- unlist(xpathApply(stack_feed_xml, '//link', xmlValue))

cnt <- 1
for(url in stack_feed_urls){
  job_url <- getURL(url)
  job_html <- htmlParse(job_url)
  job_description <- gsub("[[:space:]]", " ", unlist(xpathApply(job_html, '//ul', xmlValue)))
  job_title <- gsub("[[:space:]]|[[:punct:]]|'|", "", unlist(xpathApply(job_html, '//h1', xmlValue))[1])
  write.table(job_description[7:length(job_description)], paste("jobs/", job_title, "_", cnt, ".txt", sep = ""), 
              col.names = F, row.names = F, quote = FALSE)
  print(cnt)
  cnt <- cnt + 1
}

job_load <- function(path){
  filenames <- dir(path)
  job_details <- Corpus(DirSource(path))   
  return(job_details)
}

job_view <- job_load("jobs/")

rmv_words <- c("able", "ability", "across", "analysis", "based", "buy", "can", "candidates", 
               "chef", "closely", "company", "competitive", "conditions", "continuous", "ensure", "enterprise",
               "equivalent", "etc", "field", "francisco", "full", "hallway", "health", "including", "industry", 
               "interview", "like", "make", "money", "needs", "one", "part", "people", "professional",
               "project", "provide", "quickly", "quiet", "relat", "role", "spring",  "step", "take", "us", "will", 
               "within", "years", "york" )

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
pos_res <- review_clean(job_view)


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
review_plots(pos_res)


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
job_data <- data.frame(review_format(pos_res))
job_data <- apply(job_data, 2, as.numeric)
rownames(job_data) <- list.files("jobs/")

pos_rate <- c("advanced", "best", "big", "complex", "deep", "excellent", "experience", "expert", "great", "lead")
neutral_rate <- c("familiarity", "good", "help", "understand")
skills <- setdiff(colnames(job_data), c(pos_rate, neutral_rate))

titles <- list.files("jobs/")

final_data <- data.frame(matrix(0, nrow = nrow(job_data), ncol = (3*length(skills))))
rownames(final_data) <- list.files("jobs/")
colnames(final_data) <- c(paste(skills,"pos", sep = "_"), paste(skills,"neut", sep = "_"), paste(skills,"neg", sep = "_"))

post_review_clean <- function(docs){
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
  docs <- tm_map(docs, PlainTextDocument) 
  
  for(j in seq(docs)){
    print(j)
    job_descrptn <- as.character(docs[[j]])
    for(line in job_descrptn){
      line_modified <- strsplit(line, split = "  ")
      for(newline in line_modified[[1]]){
        for(skill in skills){
          if(length(grep(skill, newline)) == 1){
            if(gregexpr(paste(pos_rate, collapse  = "|"), newline)[[1]][1] > 0){
              final_data[j, paste(skill, "pos", sep = "_")] <- 1
            } else if(gregexpr(paste(neutral_rate, collapse  = "|"), newline)[[1]][1] > 0){
              final_data[j, paste(skill, "neut", sep = "_")] <- 1
            } else {
              final_data[j, paste(skill, "neg", sep = "_")] <- 1
            }
          } 
        }
      }
    }
  }
  return(final_data)
}
final_job_data <- post_review_clean(job_view)
final_job_data <- final_job_data[ ,apply(final_job_data, 2, function(x){min(x) != max(x)})]

distances <- dist(final_job_data, method="cosine")

# Hierarchical clustering
clusterUsers_Ward <- hclust(distances, method = "ward.D") 
clusterUsers_Average <- hclust(distances, method = "average")

# Plot the dendrogram
plot(clusterUsers_Ward)
plot(clusterUsers_Average)
cluster <- kmeans(final_job_data, 2)

######Contrasting Features
#Cluster1: neutral 
#Cluster2: tools, test, technology, technical, team, system, strong, sql, solutions, software, skills, service, science, 
#require, product, process, problem, practices, platform,  management, java, knowledge, integration,  implement, high, hadoop,
#expert, experience, engine, distributed, develop, design, data, cloud, business, build, automation, agile, analytics,
#Neutral: advanced, android, api, aws, backend, create, daily, 

fit <- skmeans(as.matrix(final_job_data), 3)
clusplot(final_job_data, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)


cluster1 <- rownames(final_job_data)[which(fit$cluster == 1)]
cluster2 <- rownames(final_job_data)[which(fit$cluster == 2)]
cluster3 <- rownames(final_job_data)[which(fit$cluster == 3)]

x <- NULL
for(rows in 1:3){
  x <- rbind(x, apply(final_job_data[which(fit$cluster == rows), ], 2, mean))
}#spherical k-means. You may want to try CLUTO