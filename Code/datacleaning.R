install.packages("jsonlite")
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)

file1 <- '/Users/rachelstuder/Desktop/628 Practicum/Mod 3/yelp_dataset_2022/business.json'
business<-stream_in(textConnection(readLines(file1, n=1000000)), flatten = TRUE)
file2 <- '/Users/rachelstuder/Desktop/628 Practicum/Mod 3/yelp_dataset_2022/review.json'
review<-stream_in(textConnection(readLines(file2, n=100000)), flatten = TRUE)
file3 <- '/Users/rachelstuder/Desktop/628 Practicum/Mod 3/yelp_dataset_2022/tip.json'
tip<-stream_in(textConnection(readLines(file3, n=100000)), flatten = TRUE)
file4 <- '/Users/rachelstuder/Desktop/628 Practicum/Mod 3/yelp_dataset_2022/user.json'
user<-stream_in(textConnection(readLines(file4, n=100000)), flatten = TRUE)


business<- business %>% select(-starts_with("hours")) %>% 
  filter(grepl('Restaurant',categories )) 
sushi1<- business %>% filter(grepl('Sushi', categories))
sushi2<- sushi1 %>% 
  filter(!grepl("Education | Books | Buffets | Caterers | Dance Clubs | Event Planning | Florists | Landmarks & Historical Buildings | Music Venues | Shopping Centers | Pharmacy | Sports Bars | Wine Bars", categories))

sushi2<- sushi2 %>% filter(!state== "AB") %>% mutate(costal_inland = case_when(
  state == "PA" | state == "DE" | state == "NJ" | state == "FL" | state == "LA" | state == "CA" ~ "costal",
   state == "TN" | state == "MO" | state == "IL" | state == "IN" | state == "AZ" | state == "NV" | state == "ID" ~ "inland"
)) %>% mutate(region = case_when(
  state == "CA" | state == "AZ" | state == "NV" | state == "ID" ~ "West",
  state == "MO" | state == "IL" | state == "IN" ~ "Midwest", 
  state == "TN" | state == "FL" | state == "LA" ~ "South",
  state == "PA" | state == "DE" | state == "NJ" ~ "East"
))

write_csv(sushi2, "/Users/rachelstuder/Desktop/628 Practicum/Mod 3//sushi2.csv")

hist(sushi2$stars, main="Distribution of Stars for Sushi Restaurants", xlab="Star Rating", ylab="Frequency")
