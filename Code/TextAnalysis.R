library(jsonlite)
library(tidyverse)
library(textstem) 
library(tm)


sushi <- read.csv("../sushi.csv")

#code to cut large dataframe into smaller pieces

total <- nrow(review)
cuts <- round(total/200000) #200000 reviews per file

for(i in 1:cuts){
  print(i)
  start <- 1+(i-1)*200000
  if(i*200000 > total){
    end <- total
  }
  else{end <- i*200000}
  name <- paste0("review-",i)
  write.csv(review[start:end,], paste0(name,".csv"))
  
}




#code to iterate through review files, filter reviews for sushi places, perform basic cleaning 
sushi_reviews <- stream_in(file("1.json"))
sushi_reviews <- sushi_reviews %>% 
  filter(business_id %in% sushi$business_id)
for(i in 2:35){
  name <- paste0(i,".json")
  temp <- stream_in(file(name))
  
  temp <- temp %>% 
    filter(business_id %in% sushi$business_id)
  
  
  sushi_reviews <- rbind(sushi_reviews,temp)
}



x <- sushi_reviews[ , 8] #grab 8th column which contains the text

x <- sapply(x, function(x)  gsub("[^\x20-\x7E]", "", x))  #remove non-letters
x <- sapply(x, function(x) gsub('([[:punct:]])', '',x))   #remove punctuation 
x <- sapply(x, tolower)                                   #Make lowercase
x <- sapply(x, function(x) removeWords(x,stopwords()))    #Remove Stopwords
x <- sapply(x, function(x) lemmatize_words(x))            #Lemmatize for standard latin root


sushi_reviews[ , 10] <- x #rewrite as new column to keep old data for error checking


write.csv(sushi_reviews,"sushi_reviews.csv")


file3 <- 'tip.json'
tip<-stream_in(file(file3))


sushi_tips <- tip %>% 
  filter(business_id %in% sushi$business_id)



x <- sushi_tips[ , 3] #grab 8th column which contains the text

x <- sapply(x, function(x)  gsub("[^\x20-\x7E]", "", x))  #remove non-letters
x <- sapply(x, function(x) gsub('([[:punct:]])', '',x))   #remove punctuation 
x <- sapply(x, tolower)                                   #Make lowercase
x <- sapply(x, function(x) removeWords(x,stopwords()))    #Remove Stopwords
x <- sapply(x, function(x) lemmatize_words(x))            #Lemmatize for standard latin root


sushi_tips[ , 6] <- x #rewrite as new column to keep old data for error checking


write.csv(sushi_tips,"sushi_tips.csv")


sushi_reviews$isFresh <- ifelse(grepl("fresh", sushi_reviews[,11], fixed=TRUE), 1,0)  #check if review contains fresh

temp <- sushi %>% 
  select(business_id,coastal_inland, region)

sushi_reviews <- sushi_reviews %>% 
left_join(temp, by = c("business_id" = "business_id"))


sushi_reviews %>% 
  group_by(coastal_inland) %>% 
  summarise(sum = sum(isFresh), n = n(), percentage = sum/n)   #calculate number of fresh mentions by coastal_inland 
                                              #and check how many reviews for each

sushi_reviews %>% 
  group_by(region) %>% 
  summarise(sum = sum(isFresh), n = n(), percentage = sum/n)  #calculate number of fresh mentions by region
                                              #and check how many reviews for each

sushi_reviews %>% 
  group_by(business_id) %>%                 #calculate number of fresh mentions by business
  summarise(sum = sum(isFresh),n = n(), percentage = sum/n)  #and check how many reviews for each
                                                                
                                                                  

stars_fresh <- sushi_reviews %>% 
  group_by(stars) %>%                 #calculate number of fresh mentions by business
  summarise(sum = sum(isFresh),n = n(), percentage = sum/n)  #and check how many reviews for each


#stars for fresh distribution
stars_fresh %>% 
  ggplot() + 
  geom_col(aes(x=stars,y=percentage)) + 
  geom_text(aes(x = stars,y=percentage, label = round(percentage,3)), vjust = 2) +
  labs(title = 'Frequency of Fresh Occuring in Reviews')  #over 1 in 4 chance of review mentioning fresh if 5 stars

            
            
            
            
            
            
            
            

