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
library(lubridate)
library(psych)

set.seed(1)
data <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
final_test <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)

ggplot(data, aes(FTR)) + geom_bar() + ggtitle('Frequency of Result')

ggplot(data, aes(HomeTeam, fill = FTR)) + geom_bar() + ggtitle('Frequency of Wins per Home Team')

ggplot(data, aes(AwayTeam, fill = FTR)) + geom_bar() + ggtitle('Frequency of Wins per Away Team')

query <- paste("SELECT ", paste('avg(', colnames(data)[6:23], '),', collapse = ""), " HomeTeam, FTR ",
               "FROM data
               GROUP BY HomeTeam, FTR", sep = "")
avg_data <- sqldf(query)
colnames(avg_data) <- c(paste('avg', colnames(data)[6:23], sep = "_"), "HomeTeam", "FTR")
l_avg_data <- alply(avg_data[ ,-c(19, 20)], 2, function(x){
                                                  y <- cbind(x, avg_data[ ,c(19, 20)])
                                                  colnames(y)[1] <- "Avg_Odds"
                                                  return(y)})
n_avg_data <- ldply(l_avg_data, function(x){x})
colnames(n_avg_data)[1] <- "Bid"
n_avg_data[ ,1] <- gsub("avg_", "", n_avg_data[ ,1])
ggplot(n_avg_data, aes(x = HomeTeam, y = Avg_Odds, colour = FTR)) + facet_wrap( ~Bid, nrow = 6, ncol = 3) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
xlab('Home Teams') + ylab('Avg Odds') +
ggtitle('Avg Odds by Bidder across Home Teams')

query <- paste("SELECT ", paste('avg(', colnames(data)[6:23], '),', collapse = ""), " AwayTeam, FTR ",
               "FROM data
               GROUP BY AwayTeam, FTR", sep = "")
avg_data <- sqldf(query)
colnames(avg_data) <- c(paste('avg', colnames(data)[6:23], sep = "_"), "AwayTeam", "FTR")
l_avg_data <- alply(avg_data[ ,-c(19, 20)], 2, function(x){
                                                  y <- cbind(x, avg_data[ ,c(19, 20)])
                                                  colnames(y)[1] <- "Avg_Odds"
                                                  return(y)})
n_avg_data <- ldply(l_avg_data, function(x){x})
colnames(n_avg_data)[1] <- "Bid"
n_avg_data[ ,1] <- gsub("avg_", "", n_avg_data[ ,1])
ggplot(n_avg_data, aes(x = AwayTeam, y = Avg_Odds, colour = FTR)) + facet_wrap( ~Bid, nrow = 6, ncol = 3) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab('Away Teams') + ylab('Avg Odds') +
  ggtitle('Avg Odds by Bidder across Away Teams')

fac_var <- c("HomeTeam", "AwayTeam")

for(var in fac_var){
  data[ ,var] <- as.factor(data[ ,var])
}

final_data <- data

for(bet in colnames(final_data)[6:23]){
  missing_bet <- data[(is.na(final_data[ ,bet])), ]
  for(i in 1:dim(missing_bet)[1]){
    x <- data[data$HomeTeam == missing_bet[i, 'HomeTeam'] & data$AwayTeam == missing_bet[i, 'AwayTeam'], ]
    if(dim(x)[1] == 1){
      x <- data[(data$HomeTeam == missing_bet[i, 'HomeTeam'] | data$AwayTeam == missing_bet[i, 'AwayTeam']) & year(data$Date) == year(missing_bet[i, 'Date']), ]
    }
    x$rate <- 1
    x[x[ ,'FTR'] == missing_bet[i, 'FTR'], 'rate'] <- 2
    final_data[which(is.na(data[ ,bet]))[i], bet] <- sum(x$rate * x[ ,bet], na.rm = TRUE)/sum(x$rate, na.rm = TRUE)
  }
}

trainIndex <- createDataPartition(final_data$FTR, p = .7, list = FALSE)
train <- final_data[trainIndex, ]
test <- final_data[-trainIndex, ]

train <- train[train[ ,'B365H'] < (mean(train[ ,'B365H']) + 1.96 * sd(train[ ,'B365H'])) &
                 train[ ,'B365H'] > (mean(train[ ,'B365H']) - 1.96 * sd(train[ ,'B365H'])) &
                 train[ ,'B365D'] < (mean(train[ ,'B365D']) + 1.96 * sd(train[ ,'B365D'])) &
                 train[ ,'B365D'] > (mean(train[ ,'B365D']) - 1.96 * sd(train[ ,'B365D'])) &
                 train[ ,'B365A'] < (mean(train[ ,'B365A']) + 1.96 * sd(train[ ,'B365A'])) &
                 train[ ,'B365A'] > (mean(train[ ,'B365A']) - 1.96 * sd(train[ ,'B365A'])) &
                 train[ ,'BWH'] < (mean(train[ ,'BWH']) + 1.96 * sd(train[ ,'BWH'])) &
                 train[ ,'BWH'] > (mean(train[ ,'BWH']) - 1.96 * sd(train[ ,'BWH'])) &
                 train[ ,'BWD'] < (mean(train[ ,'BWD']) + 1.96 * sd(train[ ,'BWD'])) &
                 train[ ,'BWD'] > (mean(train[ ,'BWD']) - 1.96 * sd(train[ ,'BWD'])) &
                 train[ ,'BWA'] < (mean(train[ ,'BWA']) + 1.96 * sd(train[ ,'BWA'])) &
                 train[ ,'BWA'] > (mean(train[ ,'BWA']) - 1.96 * sd(train[ ,'BWA'])) &
                 train[ ,'IWH'] < (mean(train[ ,'IWH']) + 1.96 * sd(train[ ,'IWH'])) &
                 train[ ,'IWH'] > (mean(train[ ,'IWH']) - 1.96 * sd(train[ ,'IWH'])) &
                 train[ ,'IWD'] < (mean(train[ ,'IWD']) + 1.96 * sd(train[ ,'IWD'])) &
                 train[ ,'IWD'] > (mean(train[ ,'IWD']) - 1.96 * sd(train[ ,'IWD'])) &
                 train[ ,'IWA'] < (mean(train[ ,'IWA']) + 1.96 * sd(train[ ,'IWA'])) &
                 train[ ,'IWA'] > (mean(train[ ,'IWA']) - 1.96 * sd(train[ ,'IWA'])) &
                 train[ ,'LBH'] < (mean(train[ ,'LBH']) + 1.96 * sd(train[ ,'LBH'])) &
                 train[ ,'LBH'] > (mean(train[ ,'LBH']) - 1.96 * sd(train[ ,'LBH'])) &
                 train[ ,'LBD'] < (mean(train[ ,'LBD']) + 1.96 * sd(train[ ,'LBD'])) &
                 train[ ,'LBD'] > (mean(train[ ,'LBD']) - 1.96 * sd(train[ ,'LBD'])) &
                 train[ ,'LBA'] < (mean(train[ ,'LBA']) + 1.96 * sd(train[ ,'LBA'])) &
                 train[ ,'LBA'] > (mean(train[ ,'LBA']) - 1.96 * sd(train[ ,'LBA'])) &
                 train[ ,'WHH'] < (mean(train[ ,'WHH']) + 1.96 * sd(train[ ,'WHH'])) &
                 train[ ,'WHH'] > (mean(train[ ,'WHH']) - 1.96 * sd(train[ ,'WHH'])) &
                 train[ ,'WHD'] < (mean(train[ ,'WHD']) + 1.96 * sd(train[ ,'WHD'])) &
                 train[ ,'WHD'] > (mean(train[ ,'WHD']) - 1.96 * sd(train[ ,'WHD'])) &
                 train[ ,'WHA'] < (mean(train[ ,'WHA']) + 1.96 * sd(train[ ,'WHA'])) &
                 train[ ,'WHA'] > (mean(train[ ,'WHA']) - 1.96 * sd(train[ ,'WHA'])) &
                 train[ ,'VCH'] < (mean(train[ ,'VCH']) + 1.96 * sd(train[ ,'VCH'])) &
                 train[ ,'VCH'] > (mean(train[ ,'VCH']) - 1.96 * sd(train[ ,'VCH'])) &
                 train[ ,'VCD'] < (mean(train[ ,'VCD']) + 1.96 * sd(train[ ,'VCD'])) &
                 train[ ,'VCD'] > (mean(train[ ,'VCD']) - 1.96 * sd(train[ ,'VCD'])) &
                 train[ ,'VCA'] < (mean(train[ ,'VCA']) + 1.96 * sd(train[ ,'VCA'])) &
                 train[ ,'VCA'] > (mean(train[ ,'VCA']) - 1.96 * sd(train[ ,'VCA'])), ]


train_mean <- apply(train[ ,c(6:23)], 2, mean)
train_sd <- apply(train[ ,c(6:23)], 2, sd)
for(i in 6:23){
  train[ ,i] <- (train[ ,i] - train_mean[i-5])/train_sd[i-5]
  test[ ,i] <- (test[ ,i] - train_mean[i-5])/train_sd[i-5]
}

fit <- principal(train[ ,c(6:23)], nfactors=18, rotate="varimax")
fit$loadings

train[ ,c('StrongHome', 'StrongDrawWeakAway', 'WeakDrawStrongAway')] <- data.matrix(train[ ,c(6:23)]) %*% data.matrix(fit$loadings[ ,c('PC1', 'PC2', 'PC3')])
test[ ,c('StrongHome', 'StrongDrawWeakAway', 'WeakDrawStrongAway')] <- data.matrix(test[ ,c(6:23)]) %*% data.matrix(fit$loadings[ ,c('PC1', 'PC2', 'PC3')])

train$FTR <- as.factor(train$FTR)
train$FTR <- relevel(train$FTR, ref = "H")
rf_model <- randomForest(FTR ~ HomeTeam + AwayTeam + StrongDrawWeakAway + StrongHome + WeakDrawStrongAway, 
                         data = train, importance=TRUE, ntree=2000, trControl = trainControl(method = "cv",number = 5))
varImpPlot(rf_model)


rf_model <- tuneRF(train[ ,c('HomeTeam', 'AwayTeam', 'StrongDrawWeakAway', 'StrongHome', 'WeakDrawStrongAway')], train$FTR, improve = 0.01)