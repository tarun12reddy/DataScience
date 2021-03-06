library(plyr)
library(textreuse)
library(RCurl)
library(XML)
library(sets)

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

document_shingles <- alply(as.character(unique_kaggle_movies$kaggle), 1,
                           function(document){
                             shings <- NULL
                             k <- 4
                             for(shingloc in 1:(nchar(document)-k+1)){
                               shings <- c(shings, substr(document, shingloc, shingloc+k-1))
                             }
                             return(shings)
                            })

unique_shingles <- NULL
for(i in 1:length(document_shingles)){
  unique_shingles <- c(unique_shingles, document_shingles[[i]])
}
unique_shingles <- data.frame(sort(unique(unique_shingles)))
rownames(unique_shingles) <- 1:dim(unique_shingles)[1]
colnames(unique_shingles) <- "shingles"
unique_shingles <- droplevels(unique_shingles)

r <- 189
input <- 0:(r-1)
hash_functions <- adply(input, 1, .id = NULL, function(x){return(input[sample(length(input))])})
hash_functions <- t(hash_functions)

signature_matrix <- function(band_shingles, documents, hashes, min_s){
  init_signature <- matrix(Inf, nrow = hashes, ncol = dim(documents)[1])
  final_list <- NULL
  for(rows in band_shingles){
    row_shingle <- as.character(unique_shingles$shingles[rows])
    for(i in 1:dim(documents)[1]){
      doc_shingles <- document_shingles[[i]]
      if(length(which(doc_shingles == row_shingle)) > 0){
        print(i)
        for(hash in 1:hashes){
          if(init_signature[hash, i] > hash_functions[rows, hash]){
            init_signature[hash, i] <- hash_functions[rows, hash]
          }
        }
      }
    }
  }
  for(col1 in 1:dim(init_signature)[2]){
    set1 <- set(init_signature[ ,col1])
    for(col2 in col1:dim(init_signature)[2]){
      set2 <- set(init_signature[ ,col2])
      jaccard_sim <- set_similarity(set1, set2, method = "Jaccard")
      if(jaccard_sim > 0.8){
        final_list <- rbind(final_list, data.frame(c(col1, col2, jaccard_sim)))
      }
    }
  }
  return(final_list)
}
  
  
b <- 100
candidate_pairs <- NULL
for(band in 1:b){
  band_end <- band_init + r - 1
  shrtlstd_shingles <- band_init:band_end
  candidate_pairs <- rbind(candidate_pairs, signature_matrix(band_shingles = shrtlstd_shingles, 
                                                             documents, hashes = 100))
  band_init <- band_end + 1
}
