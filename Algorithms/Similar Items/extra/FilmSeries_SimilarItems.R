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

req_data <- "plot"

xml_extract <- function(url){
  xml_imdb <- (tryCatch(xmlParse(url),
                        HTTPError = function(e) {
                          cat("HTTP error: ", e$message, "\n")
                        }))
  xml_imdb <- data.frame(xmlToList(xml_imdb))
  return(xml_imdb[req_data, ])
}

movies[784, 'Title'] <- "En compagnie d'Antonin Artaud"
movies[847, 'Title'] <- 'Wuya yu maque'
movies[1046, 'Title'] <- 'Romeo Juliet'
movies[2150, 'Title'] <- 'Juno and the Paycock'
movies[2680, 'Title'] <- 'Allan Quatermain and the Lost City of Gold'

title_plot <- adply(3691:length(movies$Title), 1, function(x){
  y <- strsplit(movies$Title[x], split = "\\(")
  year <- gsub(")", "", y[[1]][length(y[[1]])])
  if(is.na(as.numeric(year))){year <- ""} 
  title <- strsplit(y[[1]], split = ",")
  ftitle <- title[[1]][1]
  if(substr(ftitle, nchar(ftitle), nchar(ftitle)) == " "){
      ftitle <- substr(ftitle, 1, nchar(ftitle)-1)
  }
  ftitle <- gsub("&", "", ftitle)
  ftitle <- gsub("\\s+", " ", ftitle)
  ftitle <- gsub(" ", "+", ftitle)
  url <- paste("http://www.omdbapi.com/?t=",ftitle,"&y=", year, "&plot=full&r=xml", sep="")
  
  xml_data <- xml_extract(url)
  fxml_imdb <- xml_data[req_data, 1]
  fxml_imdb <- as.character(fxml_imdb)
  if(fxml_imdb[1] == 'N/A' | is.na(fxml_imdb[1])){
    url <- paste("http://www.omdbapi.com/?t=",ftitle,"&y=&plot=full&r=xml", sep="")
    xml_data <- xml_extract(url)
    fxml_imdb <- xml_data[req_data, 1]
    fxml_imdb <- as.character(fxml_imdb)
    if(fxml_imdb[1] == 'N/A' | is.na(fxml_imdb[1])){
      if(length(grep("and", url)) == 1){
        url <- gsub("and+", "", url)
        xml_data <- xml_extract(url)
        fxml_imdb <- xml_data[req_data, 1]
        fxml_imdb <- as.character(fxml_imdb)
      }
    }
  }
  if(fxml_imdb[1] != 'N/A' & !is.na(fxml_imdb[1])){
    print(paste(x, fxml_imdb[1], sep = " : "))
    write.table(fxml_imdb[1], paste("C:/Users/qaz/Desktop/SourceTree_DataScience/Algorithms/Similar Items/titles/title_", x, ".txt", sep = ""), 
                col.names = F, row.names = F, quote = FALSE)
  }
  return(fxml_imdb[1])
})

minhash <- minhash_generator(n = 40, seed = 1)
dir <- "C:/Users/qaz/Desktop/SourceTree_DataScience/Algorithms/Similar Items/titles"
corpus <- TextReuseCorpus(dir = dir, tokenizer = tokenize_ngrams, n = 1,
                          minhash_func = minhash, keep_tokens = TRUE,
                          progress = FALSE)

buckets <- lsh(corpus, bands = 20, progress = FALSE)
candidates <- lsh_candidates(buckets)
candidates_compare <- lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE)

candidates_compare_title <- adply(candidates_compare, 1, function(x){
  return(c(movies$Title[as.numeric(strsplit(x$a, split = "_")[[1]][2])],
           movies$Title[as.numeric(strsplit(x$b, split = "_")[[1]][2])]))
})

candidates_compare_title[candidates_compare_title$score == 1, 3:5]
