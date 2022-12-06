
library(tidyverse)


sushi_df<- read_csv("sushi.csv")
sushi<- sushi_df %>% select(1:10)

sushi$city[which(sushi$city == "Tuscon" & sushi$state == "FL" )] <- "Tuscon"
sushi$city[which(sushi$city == "Clearwater Beach" & sushi$state == "FL" )] <- "Clearwater"
sushi$city[which(sushi$city == "Land o lakes" & sushi$state == "FL" )] <- "Land O Lakes"
sushi$city[which((sushi$city == "St Petersburg" | sushi$city == "St. Petersburg") & sushi$state=="FL" )] <- "Saint Petersburg"
sushi$city[which((sushi$city == "South Tampa" | sushi$city == "Tampa Bay")& sushi$state=="FL" )] <- "Tampa"
sushi$city[which((sushi$city == "St Louis" | sushi$city == "St. Louis")& sushi$state=="MO" )] <- "Saint Louis"
sushi$city[which(sushi$city == "O Fallon" & sushi$state == "IL" )] <- "O'Fallon"
sushi$city[which(sushi$city == "St Charles" & sushi$state == "MO" )] <- "St. Charles"
sushi$city[which(sushi$city == "Deptford Township" & sushi$state == "NJ" )] <- "Deptford"
sushi$city[which(sushi$city == "Ewing" & sushi$state == "NJ" )] <- "Ewing Township"
sushi$city[which(sushi$city == "Mount Laurel" & sushi$state == "NJ" )] <- "Mount Laurel Township"
sushi$city[which(sushi$city == "Pennsauken" & sushi$state == "NJ" )] <- "Pennsauken Township"
sushi$city[which(sushi$city == "Voorhees" & sushi$state == "NJ" )] <- "Voorhees Township"
sushi$city[which(sushi$city == "MEDIA" & sushi$state == "PA" )] <- "Media"
sushi$city[which(sushi$city == "Warrington Township" & sushi$state == "PA" )] <- "Warrington"
sushi$city[which((sushi$city == "Mt Juliet Louis" | sushi$city == "Mt. Juliet")& sushi$state=="TN" )] <- "Mount Juliet"



compact <- sushi %>% 
  select(-c(6:8))

write.csv(compact, 'app_data/app_business_data.csv')


################################################################################################################


reviews <- read_csv('sushi_reviews.csv')

reviews$stars <- as.integer(reviews$stars)

stars_all_reviews <- reviews %>% 
  select(business_id, stars)
 
write.csv(stars_all_reviews, 'app_data/stars_all_reviews.csv')


################################################################################################################




library(tidyverse)


sushi_df<- read_csv("sushi.csv")
sushi<- sushi_df %>% select(1:10)

sushi$city[which(sushi$city == "Tuscon" & sushi$state == "FL" )] <- "Tuscon"
sushi$city[which(sushi$city == "Clearwater Beach" & sushi$state == "FL" )] <- "Clearwater"
sushi$city[which(sushi$city == "Land o lakes" & sushi$state == "FL" )] <- "Land O Lakes"
sushi$city[which((sushi$city == "St Petersburg" | sushi$city == "St. Petersburg") & sushi$state=="FL" )] <- "Saint Petersburg"
sushi$city[which((sushi$city == "South Tampa" | sushi$city == "Tampa Bay")& sushi$state=="FL" )] <- "Tampa"
sushi$city[which((sushi$city == "St Louis" | sushi$city == "St. Louis")& sushi$state=="MO" )] <- "Saint Louis"
sushi$city[which(sushi$city == "O Fallon" & sushi$state == "IL" )] <- "O'Fallon"
sushi$city[which(sushi$city == "St Charles" & sushi$state == "MO" )] <- "St. Charles"
sushi$city[which(sushi$city == "Deptford Township" & sushi$state == "NJ" )] <- "Deptford"
sushi$city[which(sushi$city == "Ewing" & sushi$state == "NJ" )] <- "Ewing Township"
sushi$city[which(sushi$city == "Mount Laurel" & sushi$state == "NJ" )] <- "Mount Laurel Township"
sushi$city[which(sushi$city == "Pennsauken" & sushi$state == "NJ" )] <- "Pennsauken Township"
sushi$city[which(sushi$city == "Voorhees" & sushi$state == "NJ" )] <- "Voorhees Township"
sushi$city[which(sushi$city == "MEDIA" & sushi$state == "PA" )] <- "Media"
sushi$city[which(sushi$city == "Warrington Township" & sushi$state == "PA" )] <- "Warrington"
sushi$city[which((sushi$city == "Mt Juliet Louis" | sushi$city == "Mt. Juliet")& sushi$state=="TN" )] <- "Mount Juliet"



compact <- sushi %>% 
  select(-c(6:8))

write.csv(compact, 'app_data/app_business_data.csv')


################################################################################################################


reviews <- read_csv('sushi_reviews.csv')

reviews$stars <- as.integer(reviews$stars)

stars_all_reviews <- reviews %>% 
  select(business_id, stars)
 
write.csv(stars_all_reviews, 'app_data/stars_all_reviews.csv')


################################################################################################################


#Put LDA findings altogether in an organized format: create individual analysis for each business then save results into file system.
#Reviews first

library(tidyverse)
library(tidytext)
library(topicmodels)

reviews <- read_csv("sushi_reviews.csv"); colnames(reviews)[1] <- "index"
tips <- read_csv("sushi_tips.csv"); colnames(tips)[1] <- "index"

business <- read_csv("sushi.csv")



extra_stop_words <- c('sushi', 'food', 'rolls', 'place', 'just', 'one')  #from n-gram analysis
test <- cbind(extra_stop_words,rep("CUSTOM", 6))
test <- data.frame(test); colnames(test) <- c('word','lexicon')
stop_words <- rbind(stop_words,test)

standout_reviews <- data.frame(matrix(ncol=4)); colnames(standout_reviews) <- c("business_id","text", "topi", "type")

for(i in business$business_id){
  
  for(k in c('good', 'bad')){
    print(k)
    if(k == 'good'){ 
      temp <- reviews %>% 
        filter(business_id==i & stars > 3)
    } 
    else{
      temp <- reviews %>% 
        filter(business_id==i & stars < 3)
    }
    if(nrow(temp) > 10){
      word_counts <- temp %>%
        unnest_tokens(word,V10) %>%
        anti_join(stop_words) %>% 
        count(index,word)
      
      reviews_dtm <- word_counts %>%
        cast_dtm(index, word, n)
      
      reviews_lda <- LDA(reviews_dtm, k = 2, control = list(seed = 42))
      
      
      topics <- tidy(reviews_lda, matrix = "beta")
      
      memberships <- tidy(reviews_lda, matrix = "gamma")
      prepath <- 'app_data/review-LDA/'
      path <- paste0(i,k, sep='-')
      path <- paste0(path, '.rda')
      path <- paste0(prepath,path)
      
      save(topics, memberships, file = path)
      
      for(t in 1:2){
        location <- memberships %>%
          arrange(document, topic) %>% 
          filter(topic==t) %>% 
          slice_max(gamma, n = 1) %>% 
          select(document)
        
        location <- min(as.numeric(location[[1]]))
        
        temp2 <- temp %>% 
          filter(index == location) %>% 
          select(business_id, text) %>% 
          mutate(topi = t, type = k)
        standout_reviews <- rbind(standout_reviews, temp2)
        
      }#close topics loop
      
    }#close length condition
  }#closes good and bad loop
  
}#closes business_id loop


quote_path <- paste0('app_data/quotes/','quote_reviews')
quote_path <- paste0(quote_path,'.csv')
print(quote_path)
write.csv(standout_reviews, quote_path)


#Same structure applied to tips data 

extra_stop_words <- c('sushi', 'food', 'rolls', 'place', 'just', 'one')  #from n-gram analysis
test <- cbind(extra_stop_words,rep("CUSTOM", 6))
test <- data.frame(test); colnames(test) <- c('word','lexicon')
stop_words <- rbind(stop_words,test)

standout_tips <- data.frame(matrix(ncol=3)); colnames(standout_tips) <- c("business_id","text", "topi")

for(i in business$business_id){
  
  temp <- tips %>% 
    filter(business_id==i)
  
  if(nrow(temp) > 5){
    word_counts <- temp %>%
      unnest_tokens(word,V6) %>%
      anti_join(stop_words) %>% 
      count(index,word)
    
    tips_dtm <- word_counts %>%
      cast_dtm(index, word, n)
    
    tips_lda <- LDA(tips_dtm, k = 2, control = list(seed = 42))
    
    
    topics <- tidy(tips_lda, matrix = "beta")
    
    memberships <- tidy(tips_lda, matrix = "gamma")
    prepath <- 'app_data/tips-LDA/'
    path <- paste0(i, '.rda')
    path <- paste0(prepath,path)
    
    save(topics, memberships, file = path)
    
    for(t in 1:2){
      location <- memberships %>%
        arrange(document, topic) %>% 
        filter(topic==t) %>% 
        slice_max(gamma, n = 1) %>% 
        select(document)
      
      location <- min(as.numeric(location[[1]]))
      
      temp2 <- temp %>% 
        filter(index == location) %>% 
        select(business_id, text) %>% 
        mutate(topi = t)
      standout_tips <- rbind(standout_tips, temp2)
      
    }#close topics loop
    
  }#close length condition
  
}#closes business_id loop


quote_path <- paste0('app_data/quotes/','quote_tips')
quote_path <- paste0(quote_path,'.csv')
print(quote_path)
write.csv(standout_tips, quote_path)


