sudo yum install libxml2-devel

wget http://download.osgeo.org/gdal/gdal-1.9.0.tar.gz
tar xvfz gdal-1.9.0.tar.gz
cd gdal-1.9.0
./configure 
make
sudo make install
cd ..

wget http://download.osgeo.org/proj/proj-4.9.2.tar.gz
tar xvfz proj-4.9.2.tar.gz
cd proj-4.9.2
./configure 
make
sudo make install
cd ..

sudo echo /usr/local/lib >> /etc/ld.so.conf
sudo ldconfig

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
install.packages("raster")
install.packages("maptools")
install.packages("maps")
install.packages("rgdal")
install.packages("gpclib")
install.packages("lattice")
install.packages("RColorBrewer")
install.packages("classInt")
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

wget http://www.asdar-book.org/datasets/auckland_mapgen.dat
wget http://www.asdar-book.org/datasets/seamap105_mod.csv
wget http://www.asdar-book.org/datasets/high.RData
wget http://web1.sph.emory.edu/users/lwaller/book/ch2/scotland.dat
wget https://ckannet-storage.commondatastorage.googleapis.com/2014-10-20T04:50:32.087Z/scotland.csv
wget http://www.asdar-book.org/datasets/70042108.zip
unzip 70042108.zip

sudo su
cp -a /home/ec2-user/seamap105_mod.csv /home/ruser
cp -a /home/ec2-user/70042108.tif /home/ruser
exit
