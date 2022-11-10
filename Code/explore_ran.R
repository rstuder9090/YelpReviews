library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)

file1 <- '../yelp_dataset_2022/yelp_dataset_2022/business.json'
business<-stream_in(textConnection(readLines(file1, n=1000000)), flatten = TRUE)

# file2 <- '/Users/rachelstuder/Desktop/628 Practicum/Mod 3/yelp_dataset_2022/review.json'
# review<-stream_in(textConnection(readLines(file2, n=100000)), flatten = TRUE)
# file3 <- '/Users/rachelstuder/Desktop/628 Practicum/Mod 3/yelp_dataset_2022/tip.json'
# tip<-stream_in(textConnection(readLines(file3, n=100000)), flatten = TRUE)
# file4 <- '/Users/rachelstuder/Desktop/628 Practicum/Mod 3/yelp_dataset_2022/user.json'
# user<-stream_in(textConnection(readLines(file4, n=100000)), flatten = TRUE)

business1<- business %>% select(-starts_with("hours")) %>% 
  filter(grepl('Restaurant',categories )) %>% 
  arrange(state) %>%
  mutate(categories=str_split(categories, ",")) %>%
  unnest(categories) %>%
  mutate(categories=str_trim(categories, side="both"))

#types<- business1 %>% select(categories) %>% group_by(categories) %>% arrange(categories) %>%  slice(1)
sushi<- business1 %>% filter(categories == "Sushi Bars")

# There are only 14 unique states having sushi restaurants.
sushi %>% count(state) %>% group_by(state)

#IL_types <-  IL %>% select(categories) %>% group_by(categories) %>% arrange(categories) %>%  slice(1)
MO_sushi<- business1 %>% filter(state=="MO") %>% filter(categories=="Sushi Bars")

# There are 16 sushi res in IL and 1341 reviews in total. 
# How about select states with more sushi res? like PA or FL
IL_sushi<- business1 %>% filter(state == "IL") %>% filter(categories=="Sushi Bars")
IL_sushi_reviews_num <- sum(IL_sushi$review_count)

# Some of the attributes are almost NAs.
colSums(is.na(IL_sushi))
