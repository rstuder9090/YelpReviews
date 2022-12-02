library(jsonlite)
library(tidyverse)
library(stringr)
library(word2vec)

wordvec=function(selected_business,keywords=c("service","rolls","fresh")){
  # read reviews for this particular business
  review=data.frame()
  for (i in 1:10){
    filename=paste0('./Data/sushi_review-',i,'.csv')
    reviewi=read.csv(filename) %>% 
      filter(business_id==selected_business)
    review=rbind(review,reviewi)
  }
  
  # average stars and the number of reviews
  avg_star=mean(review$stars)
  num_reviews=nrow(review)
  
  # histogram for stars of this business
  hist=review %>% 
    ggplot(aes(stars)) +
    geom_histogram(fill="skyblue",binwidth=0.5)
  
  bad_review=review %>% filter(stars<3)
  good_review=review %>% filter(stars>3)
  
  model_good=word2vec(x=good_review$V10,type="skip-gram",dim=15,window=5,iter=20)
  model_bad=word2vec(x=bad_review$V10,type="skip-gram",dim=15,window=5,iter=20)
  # we can change the keywords.
  nn_good=predict(model_good,keywords,type="nearest",top_n=10)
  nn_bad=predict(model_bad,keywords,type="nearest",top_n=10)
  return(c(avg_star,num_reviews,hist,nn_good,nn_bad))
}


#selected_business='vC2qm1y3Au5czBtbhc-DNw'   
# take this business for example. It can be changed based on users' query



