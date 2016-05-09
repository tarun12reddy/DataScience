data <- read.csv("data.csv", stringsAsFactors = TRUE)

#1
#Number of observation in data
num_obs <- dim(data)[1]*dim(data)[2]
num_obs
#Number of rows in data
nrow <- dim(data)[1]
nrow
#Number of columns in data
ncol <- dim(data)[2]
ncol
#Column Types
col_types <- sapply(data, class)
col_types
#Mean
col_means <- colMeans(data[ ,which(col_types == "numeric")])
col_means
#Median
col_median <- apply(data[ ,which(col_types == "numeric")], 2, median)
col_median
#Summary
summary(data)

#2.
#Identifying missing percentage of each column
data_miss_prcnt <- apply(data, 2, function(x){
  return(length(x[is.na(x)])/length(x))
})

#replace them by median
data_replace <- data
data_replace[ ,which(col_types == "numeric")] <- apply(data[ ,which(col_types == "numeric")], 2, function(x){
                                                                  x[is.na(x)] <- median(x, na.rm = TRUE)
                                                                  return(x)
                                                                })
#replace them by mode
data_replace[ ,which(col_types != "numeric")] <- apply(data[ ,which(col_types != "numeric")], 2, function(x){
                                                          y <- x[!is.na(x)] 
                                                          x[is.na(x)] <- names(which(table(y) == max(table(y))))
                                                          return(x)
                                                        })
#3.
# Suppose remove outliers
data_outliers <- data.frame(matrix(TRUE, nrow = dim(data)[1], ncol = dim(data)[2]))
data_outliers[ ,which(col_types == "numeric")]  <- apply(data_replace[ ,which(col_types == "numeric")] , 2, function(x){
    return(x > boxplot.stats(x)$stats[1] & x < boxplot.stats(x)$stats[5])
})
# Capping on the data(based on the percentile cut off)
data_cap <- data_replace
data_cap[ ,which(col_types == "numeric")]  <- apply(data_replace[ ,which(col_types == "numeric")] , 2, function(x){
      y <- x
      if(class(data) == "numeric"){
        x[x < boxplot.stats(y)$stats[1]] <- boxplot.stats(y)$stats[1]
        x[x > boxplot.stats(y)$stats[5]] <- boxplot.stats(y)$stats[5]
      }
      return(x)
    })
    
#4.
#Different data sources merge, tidy concept (In proc transpose), 

#5. Calculate IV value without R package
information_value <- NULL
for(col_name in colnames(data_cap[ ,which(col_types != "numeric")])){
  data_cap[ ,col_name] <- as.factor(data_cap[ ,col_name])
  iv_table <- table(data_cap[ ,col_name], data_cap[ ,'Credit.Card.Approval.Rating'])
  iv_table_NO <- t(t(iv_table[ ,'NO']/sum(iv_table[ ,'NO'])))
  iv_table_YES <- t(t(iv_table[ ,'YES']/sum(iv_table[ ,'YES'])))
  iv_table_tot <- data.frame(iv_table_YES, iv_table_NO)
  iv_table_log <- log(iv_table_tot$iv_table_YES/iv_table_tot$iv_table_NO)
  information_value[col_name] <- sum((iv_table_YES - iv_table_NO)*iv_table_log)
}


#6. 
#Run the univariate regression on each variable, get the correlation value with respective each response variable.
#Only for continous variables we can get correlation
univ_numeric_var <- cor(data_cap[ ,which(col_types == "numeric")])
#For continous and categorical variable we need to use ANOVA
#For both being categorical variables we can use Association Rule

#7.
#Get the second large value in the vector.
size <- dim(univ_numeric_var)[1]
x <- univ_numeric_var[univ_numeric_var != max(univ_numeric_var)]
x <- sort(x, decreasing = TRUE)
index <- which(univ_numeric_var == x[2])[1]
if(index%%size == 0){
  print(paste("Second Maximum Correlation is",  round(x[2], 3), 
              "and is between", 
              rownames(univ_numeric_var)[index%%size + size], " and ", 
              colnames(univ_numeric_var)[index%/%size]), 
        sep = "")
} else{
  print(paste("Second Maximum Correlation is",  round(x[2], 3), 
              "and is between", 
              rownames(univ_numeric_var)[index%%size], "and", 
              colnames(univ_numeric_var)[index%/%size]), 
        sep = "")
}
#8.
#Sum of n natural number without using (n(n+1)/2)
n <- 5
sum(c(1:n))
#9.	
#Generate the histogram
hist(data_cap$Portfolio_Balance, xlab = "Portfolio_Balance", main = "Histogram of Portfolio Balance")
#Barplot
barplot(data_cap$Portfolio_Balance, xlab = "Portfolio_Balance", main = "Barplot of Portfolio Balance")
#Density Plot
plot(density(data_cap$Portfolio_Balance), xlab = "Portfolio_Balance", main = "Density Plot of Portfolio Balance")
#10. 
#Select only numbers from the string, split the string by some special character like "_" and select the third sub-string from each cell.   
data_cap$post_code_num1 <- adply(data_cap$post_code, 1, function(x){
                              return(gsub("[A-Z]", "", strsplit(x, split = " ")[[1]][1]))
})