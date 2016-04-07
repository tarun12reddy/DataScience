library(plyr)
library(sqldf)
library(psych)
library(NbClust)

options(scipen = 999)
#In MAC use encoding latin1
movieLines <- readLines("movies.dat")
movies <- adply(movieLines, 1, function(x){
  y <- strsplit(x, split = ":")
  return(c(y[[1]][1], y[[1]][3], y[[1]][5]))
})
movies <- movies[ ,-1]
colnames(movies) <- c("MovieID", "Title", "Genres")

genres <- c("Action", "Adventure", "Animation", "Children's",
            "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
            "Film-Noir", "Horror", "Musical", "Mystery", "Romance",
            "Sci-Fi", "Thriller", "War", "Western")

#############################################################
############# FEATURE EXTRACTION ############################
#############################################################
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

genres <- c("Action", "Adventure", "Animation", "Childrens",
            "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
            "FilmNoir", "Horror", "Musical", "Mystery", "Romance",
            "SciFi", "Thriller", "War", "Western")
colnames(movie_genre) <- paste("genres", genres, sep = "_")
movies <- cbind(movies, movie_genre)
rownames(movie_genre) <- movies$MovieID

ratings <- read.csv("training_ratings_for_kaggle_comp.csv", header = TRUE, stringsAsFactors = FALSE)

query <- paste("SELECT r.user, avg(r.rating * m.genres_", genre,") as avg_", genre, "_rating
          FROM ratings as r
          INNER JOIN movies as m on r.movie = m.MovieID
          WHERE m.genres_", genre, " != 0
          GROUP BY r.user
          ORDER BY r.user", sep = "")

data <- alply(genres, 1, function(genre){
                          query <- paste("SELECT r.user, avg(r.rating * m.genres_", genre,") as avg_", genre, "_rating
                                          FROM ratings as r
                                          INNER JOIN movies as m on r.movie = m.MovieID
                                          WHERE m.genres_", genre, " != 0
                                          GROUP BY r.user
                                          ORDER BY r.user", sep = "")
                          result <- sqldf(query)
                          return(result)
})

user_genre <- ldply(data, function(x){
                      unique_users <- data.frame(unique(ratings$user))
                      colnames(unique_users) <- "user"
                      query <- paste("SELECT u.user, x.*
                                     FROM unique_users as u
                                     LEFT JOIN x as x ON u.user = x.user")
                      result <- sqldf(query)
                      result[is.na(result[ ,3]), 3] <- 0
                      return(data.frame(t(result[ ,3])))
})

user_genre <- t(user_genre)
rownames(user_genre) <- unique(ratings$user)
colnames(user_genre) <- paste("avg", genres, "rating", sep = "_")

#Plot Frequency of ratings across Genres
#Perform PCA
ug_mean <- apply(user_genre, 2, mean)
ug_sd <- apply(user_genre, 2, sd)
for(i in 1:dim(user_genre)[2]){
  user_genre[ ,i] <- (user_genre[ ,i] - ug_mean[i])/ug_sd[i]
}

fit <- principal(user_genre, nfactors = dim(user_genre)[2], rotate="varimax")
fit
fit$loadings

#####################################################################
######################## Clustering ################################
####################################################################

distances <- dist(user_genre)

# Hierarchical clustering
clusterUsers_Ward <- hclust(distances, method = "ward.D") 
clusterUsers_Average <- hclust(distances, method = "average")

# Plot the dendrogram
plot(clusterUsers_Ward)
plot(clusterUsers_Average)

# Cluster Statistics
# Pseudo-F is equivalent to CH Index
clusterUsers_Ward$call <- NULL
ccc_stat <- NbClust(user_genre, distance = "euclidean", min.nc = 2, max.nc = 20, 
        method = "ward.D", index = "ccc")
names(ccc_stat)
plot(ccc_stat$All.index)
#Cluster values for local peaks are 8 and 13

pseudot2_stat <- NbClust(user_genre, distance = "euclidean", min.nc = 2, max.nc = 20, 
                   method = "ward.D", index = "pseudot2")

plot(pseudot2_stat$All.index)
#Cluster values after immediate peak are 3, 5, 9, 12, 14 and 17

pseudof_stat <- NbClust(user_genre, distance = "euclidean", min.nc = 2, max.nc = 20, 
                         method = "ward.D", index = "ch")

plot(pseudof_stat$All.index)
#Cluster values for Relative peaks are 2, 5, 7, 11, 13 and 15

#Clusters we need to try are 5, 9 and 14
cluster_5 <- kmeans(user_genre, 5)
cluster_9 <- kmeans(user_genre, 9)
cluster_14 <- kmeans(user_genre, 14)

#Find stats similar to SAS for K means
cluster_stats <- function(cluster){
  RMS_STD <- sqrt(cluster$withinss/cluster$size)
  cluster_distances <- data.frame(as.matrix(dist(cluster$centers, diag = TRUE, upper = TRUE)))
  nearest_clusters_stats <- alply(cluster_distances, 1, function(x){
    return(list(distance = min(x[x > 0]), cluster = which(x == min(x[x > 0]))))
  })
  nearest_clusters_stats <- ldply(nearest_clusters_stats, function(x){
    return(c(x$distance, x$cluster))
  })
  nearest_clusters_stats <- nearest_clusters_stats[  ,-c(1:dim(nearest_clusters_stats)[1])]
  result <- cbind(data.frame(RMS_STD), nearest_clusters_stats)
  colnames(result) <- c("RMS_STD", "CentroidDistance", "NearestCluster")
  return(result)
}
