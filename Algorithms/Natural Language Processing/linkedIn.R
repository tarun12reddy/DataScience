library(XML)
library(RCurl)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(ggplot2)
library(SnowballC)
library(biclust)
library(cluster)
library(igraph)
library(fpc)
library(Rcampdf)
library(plyr)
library(caret)
library(randomForest)

lnkdn_url <- 'https://www.linkedin.com/vsearch/j?orig=JSHP&keywords=big+data&distance=50&locationType=I&countryCode=us&postalCode=02128&trk=two_box_geo_fill'
lnkdn_feed <- getURL(lnkdn_url)
lnkdn_feed_xml <- xmlTreeParse(lnkdn_url)
lnkdn_feed_urls <- unlist(xpathApply(lnkdn_feed_xml, '//link', xmlValue))

stack_url <- 'https://www.linkedin.com/vsearch/j?type=jobs&keywords=big+data'
stack_feed <-  getURI(stack_url, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE, followlocation = TRUE)
stack_feed_html <- htmlTreeParse(stack_feed)
stack_feed_urls <- unlist(xpathApply(stack_feed_xml, '//link', xmlValue))

