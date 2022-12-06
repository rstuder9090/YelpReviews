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



