library(jsonlite)
library(tidyverse)
library(stringr)
library(word2vec)
library(tidytext)
library(tm)
library(syuzhet)
wordvec=function(selected_business,keywords=c("service","rolls","fresh")){
  # read reviews for this particular business
  review=data.frame()
  for (i in 1:10){
    filename=paste0('./Data/sushi_review-',i,'.csv')
    reviewi=read.csv(filename) %>% 
      filter(business_id==selected_business)
    review=rbind(review,reviewi)
  }
  # # average stars and the number of reviews
  # avg_star=mean(review$stars)
  # num_reviews=nrow(review)
  # 
  # # histogram for stars of this business
  # hist=review %>% 
  #   ggplot(aes(stars)) +
  #   geom_histogram(fill="skyblue",binwidth=0.5)
  
  bad_review=review %>% filter(stars<3)
  good_review=review %>% filter(stars>3)
  sw=c(stop_words$word,"sushi",'dont','im','restaurant','dish','dishes','dinner')
  model_good=word2vec(x=good_review$V10,type="cbow",dim=15,window=2,iter=5,stopwords=sw)
  model_bad=word2vec(x=bad_review$V10,type="cbow",dim=15,window=2,iter=5,stopwords=sw)
  # we can change the keywords.
  nn_good=predict(model_good,keywords,type="nearest",top_n=5)
  nn_bad=predict(model_bad,keywords,type="nearest",top_n=5)
  
  # sentiment analysis
  text <- iconv(review$V10)
  s <- get_nrc_sentiment(text)
  barplot(
        sort(colSums(prop.table(s[, 1:8]))), 
        horiz = TRUE, 
        col='skyblue',
        cex.names = 0.7, 
        las = 1, 
        main = "Emotions in Reviews", xlab="Percentage"
    )
  
  bing_vector <- get_sentiment(text, method="bing")
  senti_score=mean(bing_vector)
  
  my_list = list(goodword=nn_good,badword=nn_bad,senti_score=senti_score,bing_vector)
  return(my_list)
}


#selected_business='vC2qm1y3Au5czBtbhc-DNw'   
# take this business for example. It can be changed based on users' query
a=wordvec('vC2qm1y3Au5czBtbhc-DNw',c('roll'))


