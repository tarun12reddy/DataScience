library(h2o)
localH2O <- h2o.init()

pathToData <- "hdfs://172.31.48.200:9000/user/ec2-user/mly-prcp-filled.txt"

airlines.hex <- h2o.importFile(localH2O, path = pathToData, key = "airlines.hex")
