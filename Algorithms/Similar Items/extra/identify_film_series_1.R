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

movies$NewTitle <- gsub(" V|VI|VII|VIII|IX|, A|", "", movies$Title)
movies$NewTitle <- gsub("[[:punct:]]|[0-9]|Part|The|I", "", movies$NewTitle)
movies$NewTitle <- gsub("\\s+", " ", movies$NewTitle)
new_movies <- movies[order(movies$NewTitle), ]

NewTitle <- adply(new_movies$NewTitle, 1, function(x){
                                          if(substr(x, nchar(x), nchar(x)) == " " |
                                             substr(x, nchar(x), nchar(x)) == "s"){
                                            x <- substr(x, 1, nchar(x)-1)
                                          }
                                          if(substr(x, 1, 1) == " "){
                                            x <- substr(x, 2, nchar(x))
                                          }
                                          return(x)
})

new_movies <- cbind(new_movies[ ,1:3], data.frame(NewTitle$V1))
colnames(new_movies)[4] <- "NewTitle"
new_movies$NewTitle <- as.character(new_movies$NewTitle)
new_movies[new_movies$NewTitle == "", 'NewTitle'] <- new_movies[new_movies$NewTitle == "", 'Title']

new_movies$New_MovieID <- transform(new_movies,id = as.numeric(factor(new_movies$NewTitle)))