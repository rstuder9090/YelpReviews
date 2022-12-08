library(tidyverse)

sushi_df<- read_csv("sushi.csv")
sushi<- sushi_df %>% filter(is_open==1) %>%select(1:10)

sushi$city[which(sushi$city == "Tuscon" & sushi$state == "AZ" )] <- "Tucson"
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

#### Duplicate Restaurants
sushi$name[which(sushi$business_id== "Q-IvBYRM1wSGL6wY-l5zEw")]<- "IOU Sushi -Findley Ave"
sushi$name[which(sushi$business_id== "9d0gEJEMVjy8UVlDg-yylw")]<-"Superb Sushi -8th St"
sushi$name[which(sushi$business_id== "NvmCO5xNbN41IRN5mxXUEg")]<-"Willowcreek Grill & Raw Sushi -Vista Ave"
sushi$name[which(sushi$business_id== "JH4S0Cvw25hFHf7m6UBmSw")]<- "iSushi Cafe"
sushi$name[which(sushi$business_id== "RmcZOeOJ8QyfRFGjiPrnkg")]<- "Naked Tchopstix on College Ave"
sushi$name[which(sushi$business_id== "ppqy2ZngsnYbp9e3qXGz-w")]<- "Sushi Club on 10th St"
sushi$name[which(sushi$business_id== "dhhq9NKmFAk7BxR6nuTfvQ")]<- "Sushi On the Rocks -Meridian St"
sushi$name[which(sushi$business_id== "6ZSLVI8PDkczCp9qrYj_tQ")]<- "Watami Sushi All You Can Eat -Pendleton Pike"
sushi$name[which(sushi$business_id== "kDmeq29jdwm-XatR2tCCYQ")]<-"Kanno California Sushi Bar -20th St"
sushi$name[which(sushi$business_id== "4JsMdrc-zs_3ZhTipMmUqw")]<-"Izakaya Little Tokyo Restaurant"
sushi$name[which(sushi$business_id== "JXucCTkmYXql9C8HGPbG4Q")]<-"Koi Sushi & Thai - Main St"
sushi$name[which(sushi$business_id== "qTicP3qlsW6zqLV7P1Uz_g")]<-"The Eastern Peak - Thompson Lane"
sushi$name[which(sushi$business_id==  "qQndnEgepogvQdFxeOUyvg")]<-"KELP Sushi Joint -Waters Ave"
sushi$name[which(sushi$business_id==  "eYNcVdV5bqqg-q23_FU0Vw")]<- "Pokeworks -Fowler Ave"
sushi$name[which(sushi$business_id==  "LmZWlvPJBwj5WG2KHV-v1Q")]<-"Sushi Garden -Broadway Bvld"
sushi$name[which(sushi$business_id==  "lhsQkb5nhf-Kd5OvgB9MNg")]<-"Sachiko Sushi -Valencia Rd"

compact <- sushi %>% filter(!business_id %in% c("-gZPRSuaHJG7PYHAGFmqdQ","yUpc_zF_lw_Fqi0gDTFwjQ", "LqpoPXU-4QijJJKmJCPhNw", "5jXiZ23pb-xf457uNM2yZg", 
                                                "K0mA7srJ-1aFZhSUzMWM_g", "pWpi_cCH8IxwWy7pRmtq2w", "e-4XTNeu31kowN1HRbJ_Gw", "ld1b8y_PHH6Mn_e7T0u04g")) %>%
  select(-c(6:8))

write.csv(compact, 'Shiny/app_business_data.csv')


################################################################################################################


reviews <- read_csv('sushi_reviews.csv')

reviews$stars <- as.integer(reviews$stars)

stars_all_reviews <- reviews %>% 
  select(business_id, stars)
 
write.csv(stars_all_reviews, 'Shiny/stars_all_reviews.csv')


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


# quote_path <- paste0('app_data/quotes/','quote_reviews')
# quote_path <- paste0(quote_path,'.csv')
# print(quote_path)
# write.csv(standout_reviews, quote_path)


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


# quote_path <- paste0('app_data/quotes/','quote_tips')
# quote_path <- paste0(quote_path,'.csv')
# print(quote_path)
# write.csv(standout_tips, quote_path)

######################################################################################################

#Format n-gram files together to account for shiny publish constraint ----------Reviews

filepath <- "review-grams/"
myfiles <- list.files(path = filepath, pattern = "csv", full.names = TRUE)

set_names <- c('Rank', '1-Gram', 'Frequency-1', 'Percent-Total-1','2-Gram', 'Frequency-2', 'Percent-Total-2','3-Gram', 'Frequency-3', 'Percent-Total-3', 'type', 'business_id')

data <- data.frame(matrix(ncol = 12)); colnames(data) <- set_names

for(file in myfiles){
  temp = str_split(file,"/")[[1]][3]
  
  if(str_detect(file,'good')){
    type="good"
    temp = str_split(temp,"-good")
    bus_id = temp[[1]][1]
    }
  else {
    type="bad"
    temp = str_split(temp,"-bad")
    bus_id = temp[[1]][1]}
  
  new_data <- read_csv(file) %>% 
    mutate(type=type, business_id = bus_id)
  colnames(new_data) <- set_names
  
  data <- rbind(data,new_data)
}



bad_review_grams <- data %>% 
  filter(type=="bad")


good_review_grams <- data %>% 
  filter(type=="good")




temp <- gsub("-good-grams.csv", "", myfiles)
temp <- gsub("-bad-grams.csv", "", temp)

temp <- gsub("review-grams/", "", temp)

k <- 1
for(i in 1:nrow(tester)){
  if(tester$Rank[i]==0 ){
    current_id <- temp[k]
    k <- k + 1
    print(k)
  }
  
  tester$business_id[i] <- current_id
  
}


write.csv(bad_review_grams,'Shiny/bad_reviews_grams.csv')

write.csv(good_review_grams,'Shiny/good_reviews_grams.csv')


###################################################################################################### ----------Tips


#Format n-gram files together to account for shiny publish constraint

filepath <- "tips-grams/"
myfiles <- list.files(path = filepath, pattern = "csv", full.names = TRUE)

set_names <- c('Rank', '1-Gram', 'Frequency-1', 'Percent-Total-1','2-Gram', 'Frequency-2', 'Percent-Total-2','3-Gram', 'Frequency-3', 'Percent-Total-3', 'business_id')

data <- data.frame(matrix(ncol = 11)); colnames(data) <- set_names

for(file in myfiles){
  temp = str_split(file,"/")[[1]][3]
  temp = str_split(temp,"-gram")
  bus_id = temp[[1]][1]
  
  new_data <- read_csv(file) %>% 
    mutate(business_id = bus_id)
  colnames(new_data) <- set_names
  
  data <- rbind(data,new_data)
}


tester <- data

tester = tester[-1,]

temp <- gsub("-grams.csv", "", myfiles)
temp <- gsub("tips-grams/", "", temp)

k <- 1
for(i in 1:nrow(tester)){
  if(tester$Rank[i]==0 ){
    current_id <- temp[k]
    k <- k + 1
    print(k)
  }
  
  tester$business_id[i] <- current_id
  
}

tester$Rank[2]

tail(tester)

write.csv(tester,'Shiny/tips_grams.csv')
