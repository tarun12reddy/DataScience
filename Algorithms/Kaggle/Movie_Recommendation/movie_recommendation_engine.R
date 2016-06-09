###################################################
############# PACKAGES ############################
###################################################
library(MASS)
library(rjson)
library(plyr)
library(knitr)
library(caret)
library(fmsb)
library(e1071)
library(randomForest)
library(caretEnsemble)
library(ROCR)
library(ineq)
library(zipcode)
library(sqldf)
library(ggplot2)

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
title_year <- adply(movies[ ,'Title'], 1 , function(x){
  t <- gsub("[a-zA-Z()/' ']", "", substr(x, nchar(x)-6, nchar(x)))
  if(nchar(t) == 4 & as.numeric(t) <= 2001){
    return(t)
  } else {
    return(NA)
  }
})
title_year <- title_year[ ,-1]

movie_genre <- adply(movies[ ,'Genres'], 1, function(x){
  dummy_genre <- rep(0, length(genres))
  g <- strsplit(x, split = '[|]')
  index_g <- aaply(g[[1]], 1, function(x){
    return(which(genres == x))
  })
  dummy_genre[index_g] <- 1
  return(dummy_genre)
})

movie_genre <- movie_genre[ ,-1]
colnames(movie_genre) <- paste("genres", genres, sep = "_")
movies <- cbind(movies[ ,-3], title_year)
movies <- cbind(movies, movie_genre)
movies$total_genres <- apply(movies, 1, function(x){return(sum(as.numeric(x[4:21])))})
rownames(movie_genre) <- movies$MovieID

data(zipcode)
users$cleanZipcode <- aaply(users$Zipcode, 1, function(x){return(substr(x, 1, 5))})

query <- "SELECT u.*, z.zip, z.city, z.state
          FROM users as u
          LEFT JOIN zipcode as z on u.cleanZipcode = z.zip"

user_zip <- sqldf(query)

query <- "SELECT m.*, u.*, z.zip, z.city, z.state, r.rating
          FROM ratings as r
INNER JOIN movies as m on r.movie = m.MovieID
INNER JOIN users as u on r.user = u.UserID
LEFT JOIN zipcode as z on u.cleanZipcode = z.zip"

data <- sqldf(query)

data$decade <- as.character(1900 + 10*floor((as.numeric(data$title_year) - 1900)/10))
data[is.na(data$title_year), 'title_year'] <- 'NA'
data[is.na(data$Gender), 'Gender'] <- 'NA'
data$Age <- as.character(data$Age)
data$Occupation <- as.character(data$Occupation)
data[is.na(data$Age), 'Age'] <- 'NA'
data[is.na(data$Occupation), 'Occupation'] <- 'NA'
data[is.na(data$state), 'state'] <- 'NA'
data[is.na(data$decade), 'decade'] <- 'NA'
data$UserID <- as.character(data$UserID)

###################################################
############# UTILITY MATRIX ######################
###################################################
#Lets choose 300 users and 300 movies randomly from the list
set.seed(1)
shrt_users <- sort(sample(users$UserID, 300))
shrt_movies <- sort(sample(movies$MovieID, 300))
ut_mat <- data.frame(matrix(0, ncol = length(shrt_movies), nrow = length(shrt_users)))
rownames(ut_mat) <- shrt_users
colnames(ut_mat) <- shrt_movies

#Fill the utility matrix with ratings given by userid for a movieid
ut_mat <- adply(ut_mat, 1, function(x){
                              y <- ratings[ratings$user == rownames(x), ]
                              z <- y[match(colnames(x), y[ ,'movie']), 'rating']
                              z[is.na(z)] <- 0
                              return(z)
                })
ut_mat <- ut_mat[ ,((dim(ut_mat)[2]/2)+1):(dim(ut_mat)[2])]
colnames(ut_mat) <- shrt_movies
rownames(ut_mat) <- shrt_users
  
genres <- gsub("['-]", "", genres)
colnames(movie_genre) <- gsub("['-]", "", colnames(movie_genre))
colnames(data) <- gsub("['-]", "", colnames(data))

###################################################
############# ITEM PROFILE ########################
###################################################
#Each Movie has boolean profile based upon actor, director, genre, year, related to user
#User and Movie has utility matrix related to each other
#For each movie there will be features say genre
#User would give ratings across movies. Say average rating given by user across movies 'x'.
query <- "SELECT UserID, avg(rating) as avg_rating
          FROM data
          GROUP BY UserID"
avg_rating <- sqldf(query)
#Then only some of these movies belong to particular genre.
#For those movies recalcuate aggregate ratings -- this will component score for that feature for that user.
query <- "SELECT d.*, a.avg_rating
          FROM data as d 
          INNER JOIN avg_rating as a on a.UserID = d.UserID"
fdata <- sqldf(query)

feature_mat <- adply(rownames(ut_mat), 1, function(x){
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
feature_mat <- feature_mat[ ,-1]
colnames(feature_mat) <- genres
rownames(feature_mat) <- shrt_users

user_feature <- feature_mat
movie_feature <- movie_genre[!is.na(match(rownames(movie_genre),  shrt_movies)), ]

#Movie vector will m x f where m indicates m movies and f features
#User vector indicates u x f where u indicates u users and f features
#For every user for every movie we can calculate the cosine distance from this we will get final utility matrix
user_feature_mag <- adply(user_feature, 1, function(x){
  return(x/sqrt(sum(x^2)))
})
user_feature_mag <- as.matrix(user_feature_mag[ ,-1])
user_feature_mag[is.na(user_feature_mag)] <- 0

movie_feature_mag <- adply(movie_feature, 1, function(x){
    return(x/sqrt(sum(x^2)))
})
movie_feature_mag <- as.matrix(t(movie_feature_mag[ ,-1]))
movie_feature_mag[is.na(movie_feature_mag)] <- 0
#Need to convert all NaN to 0

user_movie <- user_feature_mag %*% movie_feature_mag
colnames(user_movie) <- shrt_movies
rownames(user_movie) <- shrt_users
 
####################################################################
#############COLLABORATIVE FILTERING - USER SIMILARITY##############
####################################################################

utility_mat <- adply(ut_mat, 1, function(x){
  if (length(which(x > 0)) > 0){
    x[which(x > 0)] <- x[which(x > 0)] - mean(as.numeric(x[which(x > 0)]))
  }
  return(x)
})

### Option 1: Finding Similar users to one user. We need to find users that have
#more cosine distance between movies they like
#utility_matrix is u x m where u is number of users and m is number of movies
#We need a matrix u x u which has cosine distances between users

utility_mat_mag <- adply(utility_mat, 1, function(x){
  return(x/sqrt(sum(x^2)))
})
utility_mat_mag[is.na(utility_mat_mag)] <- 0

user_user <- as.matrix(utility_mat_mag) %*% as.matrix(t(utility_mat_mag))
colnames(user_user) <- shrt_users
rownames(user_user) <- shrt_users

#For each movie raitng given by a user lets choose nearest 10 users to the user who would have
#given a rating and then take average of those ratings

user_movie_rating <- adply(ut_mat, 1, function(x){
  #Every column in this a movie
  users_dist <- data.frame(user_user[setdiff(rownames(user_user), rownames(x)), rownames(x)])
  colnames(users_dist) <- 'Distance'
  users_dist$UserID <- setdiff(rownames(user_user), rownames(x))
  #Average rating given by the User
  if(length(which(x > 0)) > 0){
    avg_rating_by_user <- mean(as.numeric(x[which(x > 0)]))
  } else {
    avg_rating_by_user <- 0
  }
  all_movies <- adply(x, 2, function(y){
    #For a given user and movie find the nearest 10 users in user_user who have given a rating
    #for that movie in ut_mat
      all_user_movie_rating <- data.frame(ut_mat[setdiff(rownames(user_user), rownames(x)), colnames(y)])
      colnames(all_user_movie_rating) <- 'rating'
      all_user_movie_rating$UserID <- setdiff(rownames(user_user), rownames(x))
      query <- "SELECT u.UserID, u.Distance, a.rating
                FROM users_dist as u 
                INNER JOIN all_user_movie_rating as a
                ON u.UserID = a.UserID
                WHERE a.rating != 0
                ORDER BY u.Distance desc"
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
  return(list(by_user = avg_rating_by_user, by_near_users = all_movies))
})

####################################################################
#############COLLABORATIVE FILTERING - ITEM SIMILARITY##############
####################################################################

#Define a cosine distance between movies based upon item featu
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
  return(list(by_movie = avg_rating_for_movie, by_near_users = all_movies))
})

####################################################################
#############DIMENSION REDUCTIONALITY METHOD #######################
####################################################################

#UV Decomposition 
#Option1: Fill the missing movie ratings with average rating for movie
#Option2: Use incremental SVD method


#9 Documentary
#8 No Documentary
#7 Dont like anything
#6 Thriller no Adventure
#5 Average Liking
#4 Dont like Fantasy
#3 Prefer Romance Comedy over Action Thriller Crime
#2 Dont like Animation and Childrens
#1 Dont like Musical
#While finding the nearest rated users we need to search specifically within the cluster