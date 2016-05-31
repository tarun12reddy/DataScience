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

finaltest[is.na(finaltest$Gender), 'Gender'] <- 'NA'
finaltest$Age <- as.character(finaltest$Age)
finaltest$Occupation <- as.character(finaltest$Occupation)
finaltest[is.na(finaltest$Age), 'Age'] <- 'NA'
finaltest[is.na(finaltest$Occupation), 'Occupation'] <- 'NA'
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
colnames(ut_mat) <- shrt_movies
rownames(ut_mat) <- shrt_users

genres <- gsub("['-]", "", genres)
colnames(movie_genre) <- gsub("['-]", "", colnames(movie_genre))
colnames(train) <- gsub("['-]", "", colnames(train))
colnames(test) <- gsub("['-]", "", colnames(test))
colnames(finaltest) <- gsub("['-]", "", colnames(finaltest))

###################################################
############# ITEM PROFILE ########################
###################################################
#Each Movie has boolean profile based upon actor, director, genre, year, related to user
#User and Movie has utility matrix related to each other
#For each movie there will be features say genre
#User would give ratings across movies. Say average rating given by user across movies 'x'.
query <- "SELECT UserID, avg(rating) as avg_rating
FROM train
GROUP BY UserID"
avg_rating <- sqldf(query)
#Then only some of these movies belong to particular genre.
#For those movies recalcuate aggregate ratings -- this will component score for that feature for that user.
query <- "SELECT t.*, a.avg_rating
FROM train as t 
INNER JOIN avg_rating as a on a.UserID = t.UserID"
fdata <- sqldf(query)

feature_mat <- adply(rownames(ut_mat), 1, .id = NULL, function(x){
  y <- fdata[fdata$UserID == x, ]
  if (dim(y) > 0){
    final_z <- NULL
    
    genre_z <- adply(genres, 1, function(x){
      query <- paste("SELECT UserID, avg(rating - avg_rating)
                     FROM y
                     WHERE ", paste("genres", x, sep = "_"), " > 0 ",
                     "GROUP BY UserID", sep = "")
      z <- sqldf(query)
      if(dim(z)[1] == 0){
        final_z <- 0
      } else {
        final_z <- z[ ,2]
      }
      return(final_z)
    })
    genre_z <- t(genre_z)
    return(as.numeric(genre_z[2, ]))
  } else {
    return(rep(0, length(genres)))
  }
})
colnames(feature_mat) <- genres
rownames(feature_mat) <- shrt_users

user_feature <- feature_mat
movie_feature <- movie_genre[!is.na(match(rownames(movie_genre),  shrt_movies)), ]

#Movie vector will m x f where m indicates m movies and f features
#User vector indicates u x f where u indicates u users and f features
#For every user for every movie we can calculate the cosine distance from this we will get final utility matrix
user_feature_mag <- adply(user_feature, 1, .id = NULL, function(x){
  return(x/sqrt(sum(x^2)))
})
user_feature_mag <- as.matrix(user_feature_mag)
user_feature_mag[is.na(user_feature_mag)] <- 0

movie_feature_mag <- adply(movie_feature, 1, .id = NULL, function(x){
  return(x/sqrt(sum(x^2)))
})
movie_feature_mag <- as.matrix(t(movie_feature_mag))
movie_feature_mag[is.na(movie_feature_mag)] <- 0
#Need to convert all NaN to 0

user_movie <- user_feature_mag %*% movie_feature_mag
colnames(user_movie) <- shrt_movies
rownames(user_movie) <- shrt_users

###################################################
################# PREDICTION ######################
###################################################

test$rating_pred <- adply(test, 1, .id = NULL, function(x){
   item <- user_movie[rownames(user_movie) == x$UserID, ]
   user_ratings <- ut_mat[rownames(ut_mat) == x$UserID, ]
   if(length(item) > 0){
     if(max(user_ratings) > 0){
       movie_user_rated <- user_ratings[as.numeric(user_ratings) > 0]
       movie_user_rated_item <- item[as.numeric(user_ratings) > 0]
       test_movie_user_item <- item[as.numeric(user_ratings) == x$MovieID]
       movie_user_item <- c(test_movie_user_item, movie_user_rated_item)
       movie_user_rated <- c(NA, movie_user_rated) #NA indicates test one yet to be rated
       order_movie_user_item <- order(movie_user_item)
       if(order_movie_user_item[1] == 1){
         #Movie had highest item value for this user
         if(movie_user_rated[order_movie_user_item[2]] == 5){
           #Next biggest rating is 5 hence this movie also gets rating 5
           return(5)
         } else {
           #Next biggest rating is less than 5 hence this movie rating 
           #is one more than that rating 
           return(movie_user_rated[order_movie_user_item[2]] + 1)
         }
       } else if (order_movie_user_item[length(order_movie_user_item)] == 1){
         #Movie had lowest item value for this user
         if(movie_user_rated[order_movie_user_item[length(order_movie_user_item)] - 1] == 1){
           #Next big rating is 1 hence this movie also gets rating 1
           return(1)
         } else {
           #Next big rating is more than 1 hence this movie rating is one less than that rating 
           return(movie_user_rated[order_movie_user_item[length(order_movie_user_item)]] - 1)
         }
       } else {
         movie_user_rated <- movie_user_rated[order_movie_user_item]
         loc_test_movie_user <- which(is.na(movie_user_rated))
         rate_test_movie_user <- (movie_user_rated[loc_test_movie_user + 1] + movie_user_rated[loc_test_movie_user - 1])/2
         return(round(rate_test_movie_user))
       }
     } else {
         return('Noratings in Train')
       }
   } else {
     return('User not in item')
   }
})
