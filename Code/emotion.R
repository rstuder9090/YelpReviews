# take business_id=='vC2qm1y3Au5czBtbhc-DNw' for example

s <- read.csv('Data/sentiment/vC2qm1y3Au5czBtbhc-DNw.csv')
# The average difference between the numbers 
# positive words and negative words in each review
sentiment_score=round(mean(s$score),2)

barplot(
  sort(colSums(prop.table(s[, 1:8]))),
  horiz = TRUE,
  col='skyblue',
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Reviews", xlab="Percentage"
)

# example for word2vec
# another target word service by read word2vec_service.csv
# direction can be 'positive' and 'negative'
s <- read.csv('.//Data//word2vec_service.csv')
s %>% filter(direction=='negative') %>% 
  ggplot() + 
  geom_col(aes(x=reorder(keywords,similarity),y=similarity),fill='skyblue')+
  labs(x="Keywords",y='Conditional Probability',
       title="Top 10 Keywords Related to the Target Word 'Service'")