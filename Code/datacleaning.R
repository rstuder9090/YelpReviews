library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

#Stream in all json files
file <- 'business.json'
business<-stream_in(textConnection(readLines(file, n=1000000)), flatten = TRUE)

#Filter for Sushi Restaurants. Limit to main business as Sushi Bar.
business<- business %>% select(-starts_with("hours")) %>% 
  filter(grepl('Restaurant',categories )) 
sushi1<- business %>% filter(grepl('Sushi', categories))
sushi2<- sushi1 %>% 
  filter(!grepl("Education | Books | Buffets | Caterers | Dance Clubs | Event Planning | Florists | Landmarks & Historical Buildings | Music Venues | Shopping Centers | Pharmacy | Sports Bars | Wine Bars", categories))

#Create variables for inland/coastal and region
sushi2<- sushi2 %>% filter(!state== "AB") 
sushi2<- sushi2 %>% mutate(coastal_inland = case_when(
  state == "PA" | state == "DE" | state == "NJ" | state == "FL" | state == "LA" | state == "CA" ~ "coastal",
   state == "TN" | state == "MO" | state == "IL" | state == "IN" | state == "AZ" | state == "NV" | state == "ID" ~ "inland"
)) %>% mutate(region = case_when(
  state == "CA" | state == "AZ" | state == "NV" | state == "ID" ~ "West",
  state == "MO" | state == "IL" | state == "IN" ~ "Midwest", 
  state == "TN" | state == "FL" | state == "LA" ~ "South",
  state == "PA" | state == "DE" | state == "NJ" ~ "Northeast"
))

#Write sushi.csv
path<- getwd()
write.csv(sushi2, file.path(path, "sushi.csv"), row.names=FALSE)
