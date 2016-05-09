###################################################
############# PACKAGES ############################
###################################################
library(MASS)
library(plyr)
library(knitr)
library(caret)
library(sqldf)
###################################################
############# DATASETS ############################
###################################################
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
users <- read.delim("users.dat", header = FALSE, sep = ":", stringsAsFactors = FALSE)
users <- users[ ,seq(1, 9, 2)]
colnames(users) <- c("UserID", "Gender", "Age", "Occupation", "Zipcode")
ratings <- read.csv("training_ratings_for_kaggle_comp.csv", header = TRUE, stringsAsFactors = FALSE)

genres <- c("Action", "Adventure", "Animation", "Children's",
            "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
            "Film-Noir", "Horror", "Musical", "Mystery", "Romance",
            "Sci-Fi", "Thriller", "War", "Western")

#############################################################
############# FEATURE EXTRACTION ############################
#############################################################
movie_genre <- adply(movies[ ,'Genres'], 1, .id = NULL, function(x){
  dummy_genre <- rep(0, length(genres))
  g <- strsplit(x, split = '[|]')
  index_g <- aaply(g[[1]], 1, function(x){
    return(which(genres == x))
  })
  dummy_genre[index_g] <- 1
  return(dummy_genre)
})

colnames(movie_genre) <- paste("genres", genres, sep = "_")
movies <- cbind(movies, movie_genre)
rownames(movie_genre) <- movies$MovieID

query <- "SELECT m.*, u.*, r.rating
          FROM ratings as r
          INNER JOIN movies as m on r.movie = m.MovieID
          INNER JOIN users as u on r.user = u.UserID"

data <- sqldf(query)

data[is.na(data$Gender), 'Gender'] <- 'NA'
data$Age <- as.character(data$Age)
data$Occupation <- as.character(data$Occupation)
data[is.na(data$Age), 'Age'] <- 'NA'
data[is.na(data$Occupation), 'Occupation'] <- 'NA'
data$UserID <- as.character(data$UserID)

#Selecting 400 random users
slctd_users <- sample(unique(data$UserID), 100)
data <- data[ ,c('MovieID', 'UserID', setdiff(colnames(data), c('title_year', 'Zipcode', 'cleanZipcode', 
                                         'cleanZipcode1', 'zip', 'city', 
                                         'total_genres', 'MovieID', 'UserID')))]
data <- data[!is.na(match(data$UserID, slctd_users)), ]
trainIndex <- createDataPartition(data$rating, p = .7, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

knn <- function(training, k_nn, testing, wts, response){
  wt <- c(wts, rep(1, 24))
  train_res <- training[,response]  #get response variable from training data
  test_res <- testing[,response]    
  train_pred <- training[,-which(names(training) %in% response)]  #remove response variable
  test_pred <- testing[,-which(names(testing) %in% response)]
  
  train_row_count <- nrow(train_pred)
  train_col_count <- ncol(train_pred)
  
  num_var <- sapply(train_pred,is.numeric)  #numeric variables
  nom_var <- sapply(train_pred,is.character)  #ordinal variables

  train_pred_nom <- as.matrix(train_pred[,nom_var])
  train_pred_num <- as.matrix(train_pred[,num_var])
  
  test_pred[nom_var] <- lapply(test_pred[nom_var],as.character)
  
  #calculate 'gower' distance matrix
  dist <- apply(test_pred,1,
                  function(tx){
                    dist_num <- abs(sweep(train_pred_num,2,as.numeric(tx[num_var]),"-"))%*%(wt[num_var])
                    dist_nom <- abs(sweep(train_pred_nom,2,tx[nom_var],"!=")+0)%*%(wt[nom_var])    		
                    dist_all <- (dist_num+dist_nom)/sum(wt)
                    return(dist_all)
                  })

  shrtlstd_dist <- apply(dist, 2,
                         function(tx){
                           temp <- tx[order(tx)[1:k_nn]]
                           return(temp)
                         })
  
  pred <- apply(dist, 2, 
                function(tx){
                  temp <- train_res[order(tx)[1:k_nn]]
                  return(temp)
                })

  pred <- t(pred)
  shrtlstd_dist <- t(shrtlstd_dist)
  colnames(pred) <- paste("knn", 1:k_nn, sep = "_")
  colnames(shrtlstd_dist) <- paste("knn", 1:k_nn, sep = "_")
  return(list(predictions = pred, distances = shrtlstd_dist))
}

kernel <- function(pred, dist, type, k_nn){ 
  pred <- data.frame(pred[ ,1:k_nn])
  dist <- data.frame(dist[ ,1:k_nn])
  colnames(pred) <- paste("knn", 1:k_nn, sep = "_")
  colnames(dist) <- paste("knn", 1:k_nn, sep = "_")
  if(type == "Uniform"){
    wt <- data.frame(matrix(1, nrow = dim(pred)[1], ncol = k_nn))
  } 
  if(type == "Epanechsilov"){
    wt <- apply(dist, 2, function(tx){
                          return((3*(1-(tx)^2))/4)
                         })
  }
  if(type == "Guassian"){
    wt <- apply(dist, 2, function(tx){
                          return(exp(-(tx)^2)/sqrt(2*pi))
                         })
  }
  data <- cbind(pred, wt)
  final_pred <- apply(data, 1, 
                      function(tx){
                        return(sum((tx[1:k_nn]*tx[(k_nn+1):(2*k_nn)])/sum(tx[(k_nn+1):(2*k_nn)])))
                      })
  return(final_pred)
}

error <- function(test_data, pred){
  data <- cbind(test_data, data.frame(pred))
  data$pred <- -data$pred
  data <- data[order(data$UserID),]
  data$seq <- unlist(with(data, tapply(pred, UserID, function(x) rank(x, ties.method= "first"))))
  
  data$pred <- -data$pred
  query <- "SELECT d.UserID,
                   d.rating,
                   d.pred,
                   d.seq,
                   g.cnt,
                   g.maxseq
            FROM data as d
            INNER JOIN (SELECT UserID, count(*) as cnt, max(seq) as maxseq
                        FROM data
                        GROUP BY UserID) as g ON g.UserID = d.UserID
            ORDER BY d.UserID, d.pred desc"
  
  data$req_seq <- floor(data$seq*0.5) + 1
  data$rnd_pred <- round(data$pred)
  data <- data[data$req_seq == 1, ]
  return(list(kaggle_score = mean(c(data$rating, data$rnd_pred)), model_error = mean(abs(data$rating - data$rnd_pred))))
}

final_result <- data.frame(matrix(0, nrow = (3*3*50*3), ncol = 6))
colnames(final_result) <- c("knn", "kernel", "movie_wt", "user_wt", "kaggle_score", "model_error")

cnt <- 1
for(movie_wt in c(1, 2, 3)){
  for(user_wt in c(1, 2, 3)){
    knn_result <- knn(training = train, k_nn = 50, testing = test, wts = c(movie_wt, user_wt), response = 'rating')
    for(knn_val in 1:50){
      for(krnl in c("Uniform", "Guassian", "Epanechsilov")){
        kernel_result <- kernel(pred = knn_result$predictions, dist = knn_result$distances, type = krnl, k_nn = knn_val)
        result <- error(test_data = test, pred = kernel_result)
        final_result[cnt, 'knn'] <- knn_val
        final_result[cnt, 'kernel'] <- krnl
        final_result[cnt, 'movie_wt'] <- movie_wt
        final_result[cnt, 'user_wt'] <- user_wt
        final_result[cnt, 'kaggle_score'] <- result$kaggle_score
        final_result[cnt, 'model_error'] <- result$model_error
        cnt <- cnt + 1
      }
    }
  }
}

final_result$movie_user_wt <- paste("movie", final_result$movie_wt, "user", final_result$user_wt, sep = "_")
