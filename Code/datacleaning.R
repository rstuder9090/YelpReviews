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
business1<- business %>% 
  arrange(state) %>%
  mutate(categories=str_split(categories, ",")) %>%
  unnest(categories) %>%
  mutate(categories=str_trim(categories, side="both"))

types<- business1%>% select(categories) %>% group_by(categories) %>% arrange(categories) %>%  slice(1)
