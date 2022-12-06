library(jsonlite)
library(tidyverse)
library(stringr)
library(word2vec)
library(tidytext)
library(tm)
library(syuzhet)

reviewall=data.frame()
for (i in 1:10){
  filename=paste0('./Data/sushi_review-',i,'.csv')
  reviewi=read.csv(filename)
  #   filter(business_id=='vC2qm1y3Au5czBtbhc-DNw')
  reviewall=rbind(reviewall,reviewi)
}
sw=c(stop_words$word,"sushi",'dont','im','restaurant','dish','dishes','dinner','roles')
sw[167]=NA
sw[865]=NA
# senti_res=data.frame()
for (business in unique(reviewall$business_id)[1:2]){
  # bad_review=reviewall %>% filter(stars<3 & business_id==business_id)
  # good_review=reviewall %>% filter(stars>3 & business_id==business_id)
  # model_good=word2vec(x=good_review$V10,type="cbow",dim=15,window=2,iter=5,stopwords=sw)
  # model_bad=word2vec(x=bad_review$V10,type="cbow",dim=15,window=2,iter=5,stopwords=sw)
  # nn_good=predict(model_good,'rolls',type="nearest",top_n=7)
  # nn_bad=predict(model_bad,'rolls',type="nearest",top_n=7)
  # rollsi=rbind(nn_good$rolls,nn_bad$rolls)
  # rollsi$business_id=rep(business_id,14)
  # rollsi$direction=rep(c('good','bad'),each=7)
  # rolls=rbind(rolls,rollsi)
  review=reviewall %>% filter(business_id==business)
  text <- iconv(review$V10)
  s <- get_nrc_sentiment(text)
  s$score <- get_sentiment(text, method="bing")
  s$business_id <- rep(business_id,nrow(s))
  write.csv(s, paste0('.//Data//sentiment//',business,'.csv'), row.names=FALSE)
}

bad_review=reviewall %>% filter(stars<3)
good_review=reviewall %>% filter(stars>3)
model_good=word2vec(x=good_review$V10,type="cbow",dim=15,window=2,iter=5,stopwords=sw)
model_bad=word2vec(x=bad_review$V10,type="cbow",dim=15,window=2,iter=5,stopwords=sw)
nn_good=predict(model_good,'get',type="nearest",top_n=7)
nn_bad=predict(model_bad,'get',type="nearest",top_n=7)


# sentiment analysis
# review=reviewall %>% filter(business_id=='vC2qm1y3Au5czBtbhc-DNw' )
# text <- iconv(review$V10)

s <- read.csv('Data/sentiment/vC2qm1y3Au5czBtbhc-DNw.csv')
barplot(
      sort(colSums(prop.table(s[, 1:8]))),
      horiz = TRUE,
      col='skyblue',
      cex.names = 0.7,
      las = 1,
      main = "Emotions in Reviews", xlab="Percentage"
  )
# bing_vector <- get_sentiment(text, method="bing")
# senti_score=mean(bing_vector)
  
#selected_business='vC2qm1y3Au5czBtbhc-DNw'   
# take this business for example. It can be changed based on users' query
# a=wordvec('vC2qm1y3Au5czBtbhc-DNw',c('roll'))

s <- read.csv('.//Data//word2vec_roll.csv')
barplot(
  sort(colSums(prop.table(s[, 1:8]))),
  horiz = TRUE,
  col='skyblue',
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Reviews", xlab="Percentage"
)
