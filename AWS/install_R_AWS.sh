sudo yum install -y R
wget https://download2.rstudio.org/rstudio-server-rhel-0.99.465-x86_64.rpm
sudo yum install -y --nogpgcheck rstudio-server-rhel-0.99.465-x86_64.rpm
sudo adduser ruser
sudo passwd ruser

install.packages("MASS")
install.packages("rjson")
install.packages("plyr")
install.packages("knitr")
install.packages("caret")
install.packages("fmsb")
install.packages("e1071")
install.packages("randomForest")
install.packages("caretEnsemble")
install.packages("ROCR")
install.packages("ineq")
install.packages("zipcode")
install.packages("sqldf")
install.packages("ggplot2")
install.packages("sp")
install.packages("maptools")
install.packages("maps")
install.packages("rgdal")
install.packages("mime")
install.packages("stringi")
install.packages("magrittr")
install.packages("evaluate")
install.packages("digest")
install.packages("formatR")
install.packages("highr")
install.packages("markdown")
install.packages("stringr")
install.packages("Rcpp")