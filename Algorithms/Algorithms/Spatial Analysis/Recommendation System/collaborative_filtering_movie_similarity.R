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

ages <- levels(as.factor(users$Age))

occupations <- levels(as.factor(users$Occupation))

genders <- levels(as.factor(users$Gender))

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

users_age <- adply(users[ ,'Age'], 1, .id = NULL, function(x){
  dummy_age <- rep(0, length(ages))
  dummy_age[which(ages == x)] <- 1
  return(dummy_age)
})
colnames(users_age) <- paste("ages", ages, sep = "_")

users_occupation <- adply(users[ ,'Occupation'], 1, .id = NULL, function(x){
  dummy_occupation <- rep(0, length(occupations))
  dummy_occupation[which(occupations == x)] <- 1
  return(dummy_occupation)
})
colnames(users_occupation) <- paste("occupations", occupations, sep = "_")

users_gender <- adply(users[ ,'Gender'], 1, .id = NULL, function(x){
  dummy_gender <- rep(0, length(genders))
  dummy_gender[which(genders == x)] <- 1
  return(dummy_gender)
})
colnames(users_gender) <- paste("genders", genders, sep = "_")

users <- do.call("cbind", list(users, users_gender, users_age, users_occupation))
users <- users[ ,-c(2:5)]
movies <- cbind(movies, movie_genre)
rownames(movie_genre) <- movies$MovieID

query <- "SELECT m.*, u.*, r.rating
FROM ratings as r
INNER JOIN movies as m on r.movie = m.MovieID
INNER JOIN users as u on r.user = u.UserID"

data <- sqldf(query)
data$UserID <- as.character(data$UserID)

###################################################
############# TRAIN AND TEST ######################
###################################################

trainIndex <- createDataPartition(data$UserID, p = .9, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]
finaltest <- read.csv("sample_submission.csv", header = TRUE)
finaltest$movie <- apply(finaltest, 1, function(x){
  return(strsplit(as.character(x[3]), split = "_")[[1]][2])
})  

query <- "SELECT m.*, u.*, f.rating
FROM finaltest as f
INNER JOIN movies as m on f.movie = m.MovieID
INNER JOIN users as u on f.user = u.UserID"

finaltest <- sqldf(query)
finaltest$UserID <- as.character(finaltest$UserID)

###################################################
############# UTILITY MATRIX ######################
###################################################
#Lets choose 100 users and 100 movies randomly from the list
set.seed(1)
shrt_users <- sort(sample(users$UserID, 100))
shrt_movies <- sort(sample(movies$MovieID, 100))
ut_mat <- data.frame(matrix(0, ncol = length(shrt_movies), nrow = length(shrt_users)))
rownames(ut_mat) <- shrt_users
colnames(ut_mat) <- shrt_movies

#Fill the utility matrix with ratings given by userid for a movieid
ut_mat <- adply(ut_mat, 1, .id = NULL, function(x){
  y <- train[train$user == rownames(x), ]
  z <- y[match(colnames(x), y[ ,'MovieID']), 'rating']
  z[is.na(z)] <- 0
  return(z)
})
ut_mat <- ut_mat[ ,-c(1:length(shrt_movies))]
colnames(ut_mat) <- shrt_movies
rownames(ut_mat) <- shrt_users

genres <- gsub("['-]", "", genres)
colnames(movie_genre) <- gsub("['-]", "", colnames(movie_genre))
colnames(train) <- gsub("['-]", "", colnames(train))
colnames(test) <- gsub("['-]", "", colnames(test))
colnames(finaltest) <- gsub("['-]", "", colnames(finaltest))

movie_feature <- movie_genre[!is.na(match(rownames(movie_genre),  shrt_movies)), ]

#####################################################################
############## COLLABORATIVE FILTERING (ITEM SIMILARITY) ############
#####################################################################

#Define a cosine distance between movies based upon item features
### Option 1: Finding Similar movies to one movie. We need to find movies that have more cosine distance between movies they like
#utility_matrix is u x m where u is number of users and m is number of movies
#We need a matrix m x m which has cosine distances between movies

utility_movie_mag <- adply(movie_feature, 1, function(x){
  return(x/sqrt(sum(x^2)))
})
utility_movie_mag[is.na(utility_movie_mag)] <- 0

movie_movie <- as.matrix(utility_movie_mag) %*% as.matrix(t(utility_movie_mag))
colnames(movie_movie) <- shrt_movies
rownames(movie_movie) <- shrt_movies

#For each movie raitng given by a user lets choose nearest 10 users to the user who would have
#given a rating and then take average of those ratings
t_ut_mat <- t(ut_mat)
user_movie_rating <- adply(t_ut_mat, 1, function(x){
  #Every column in this a movie
  movies_dist <- data.frame(movie_movie[setdiff(rownames(movie_movie), rownames(x)), rownames(x)])
  colnames(users_dist) <- 'Distance'
  movies_dist$MovieID <- setdiff(rownames(movie_movie), rownames(x))
  if(length(which(x > 0)) > 0){
    avg_rating_for_movie <- mean(as.numeric(x[which(x > 0)]))
  } else {
    avg_rating_for_movie <- 0
  }
  
  all_movies <- adply(x, 2, function(y){
    #For a given user and movie find the nearest 10 users in user_user who have given a rating
    #for that movie in ut_mat
    all_user_movie_rating <- data.frame(t_ut_mat[setdiff(rownames(movie_movie), rownames(x)), colnames(y)])
    colnames(all_user_movie_rating) <- 'rating'
    all_user_movie_rating$UserID <- setdiff(rownames(user_user), rownames(x))
    query <- "SELECT m.UserID, m.Distance, a.rating
    FROM movies_dist as m
    INNER JOIN all_user_movie_rating as a
    ON m.UserID = a.UserID
    WHERE a.rating != 0
    ORDER BY m.Distance desc"
    nearest_user_movie_ratings <- sqldf(query)
    if(dim(nearest_user_movie_ratings)[1] == 0){
      avg_rating_by_near_users <- 0
    } else if (dim(nearest_user_movie_ratings)[1] <= 10){
      avg_rating_by_near_users <- as.numeric(mean(nearest_user_movie_ratings$rating))
    } else {
      nearest_10_user_movie_ratings <- nearest_user_movie_ratings[1:10, ]
      #Average of the 10 ratings in the value of rating for a given user and movie combination
      avg_rating_by_near_users <- as.numeric(mean(nearest_10_user_movie_ratings$rating))
    }
    return(avg_rating_by_near_users)
  })
  all_movies <- data.frame(t(all_movies[ ,2]))
  colnames(all_movies) <- shrt_movies
  return(all_movies)
})
rownames(user_movie_rating) <- shrt_movies
###################################################
################# PREDICTION ######################
###################################################

test$rating_pred <- adply(test, 1, .id = NULL, function(x){
  row_id <- which(rownames(user_movie_rating) == x$UserID)
  col_id <- which(colnames(user_movie_rating) == x$MovieID)
  if(length(row_id) == 1 & length(col_id) == 1){
    return(user_movie_rating[row_id, col_id])
  } else {
    return(NA)
  }
})

finaltest$rating_pred <- adply(finaltest, 1, .id = NULL, function(x){
  row_id <- which(rownames(user_movie_rating) == x$UserID)
  col_id <- which(colnames(user_movie_rating) == x$MovieID)
  if(length(row_id) == 1 & length(col_id) == 1){
    return(user_movie_rating[row_id, col_id])
  } else {
    return(NA)
  }
})