library(mapview)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

#Load the data
sushi<- read.csv("sushi.csv")

# Distribution for Sushi Restaurants by stars given
hist(sushi$stars, main="Distribution of Stars for Sushi Restaurants", 
     xlab="Star Rating", ylab="Frequency",
     cex.lab=1.5, cex.main=1.75)

#Display map of where Restaurants lie
mapview(sushi, xcol = "longitude", ycol = "latitude", crs = 4326, grid = FALSE)

