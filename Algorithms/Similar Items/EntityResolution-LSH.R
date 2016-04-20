library(plyr)
library(textreuse)
library(RCurl)
library(XML)

set.seed(1)
options(scipen = 999)
#In MAC use encoding latin1
movieLines <- readLines("movies.dat")
movies <- adply(movieLines, 1, function(x){
  y <- strsplit(x, split = ":")
  return(c(y[[1]][1], y[[1]][3], y[[1]][5]))
})
movies <- movies[ ,-1]
colnames(movies) <- c("MovieID", "Title", "Genres")
unique_kaggle_movies <- data.frame(sort(unique(movies$Title)))
rownames(unique_kaggle_movies) <- 1:dim(unique_kaggle_movies)[1]
colnames(unique_kaggle_movies) <- "kaggle"
unique_kaggle_movies <- droplevels(unique_kaggle_movies)

theURL <- "http://vincentarelbundock.github.io/Rdatasets/csv/ggplot2/movies.csv"
movie_data <- read.table(file = theURL, header = TRUE, sep = ",")
unique_imdb_movies <- data.frame(unique(paste(movie_data$title, " (", movie_data$year, ")", sep = "")))
rownames(unique_imdb_movies) <- 1:dim(unique_imdb_movies)[1]
colnames(unique_imdb_movies) <- "imdb"
unique_imdb_movies <- droplevels(unique_imdb_movies)

for(row in 1:dim(unique_imdb_movies)[1]){
  write.table(as.character(unique_imdb_movies[row, ]), 
              paste("C:/Users/qaz/Desktop/SourceTree_DataScience/Algorithms/Similar Items/titles/imdb_title_", row, ".txt", sep = ""), col.names = F, row.names = F, quote = FALSE)
}

for(row in 1:dim(unique_kaggle_movies)[1]){
  write.table(as.character(unique_kaggle_movies[row, ]), 
              paste("C:/Users/qaz/Desktop/SourceTree_DataScience/Algorithms/Similar Items/titles/kaggle_title_", row, ".txt", sep = ""), col.names = F, row.names = F, quote = FALSE)
}

minhash <- minhash_generator(n = 240, seed = 1)
dir <- "C:/Users/qaz/Desktop/SourceTree_DataScience/Algorithms/Similar Items/titles"
corpus <- TextReuseCorpus(dir = dir, tokenizer = tokenize_ngrams, n = 3,
                          minhash_func = minhash, keep_tokens = TRUE,
                          progress = FALSE)

buckets <- lsh(corpus, bands = 60, progress = FALSE)
candidates <- lsh_candidates(buckets)
candidates_compare <- lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE)

max_similar_titles <- candidates_compare

title1 <- adply(max_similar_titles$a, 1, function(x){
  return(length(grep("imdb", x)))
})

title2 <- adply(max_similar_titles$b, 1, function(x){
  return(length(grep("kaggle", x)))
})
  
max_similar_titles$title1 <- title1$V1
max_similar_titles$title2 <- title2$V1

final_similar_titles <- max_similar_titles[(max_similar_titles$title1 == 1 & max_similar_titles$title2 == 1) | 
                                           (max_similar_titles$title1 == 0 & max_similar_titles$title2 == 0), ]