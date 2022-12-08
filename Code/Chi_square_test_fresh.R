# a=c(762, 327, 468)
# b=c(484, 239, 477)
# M <- as.table(rbind(a/sum(a), b/sum(b)))
# dimnames(M) <- list(gender = c("F", "M"),
#                     party = c("Democrat","Independent", "Republican"))
# Xsq <- chisq.test(M)
# Xsq
library(tidyverse)
sushi=read.csv('Data/sushi.csv')
sushi_reviews=data.frame()
for (i in 1:10){
  filename=paste0('Data/sushi_review-',i,'.csv')
  reviewi=read.csv(filename)
  #   filter(business_id=='vC2qm1y3Au5czBtbhc-DNw')
  sushi_reviews=rbind(sushi_reviews,reviewi)
}

sushi_reviews$isFresh <- ifelse(grepl("fresh", sushi_reviews$V10, 
                            fixed=TRUE), 1,0)  #check if review contains fresh
temp <- sushi %>% 
  select(business_id,coastal_inland, region)

sushi_reviews <- sushi_reviews %>% 
  left_join(temp, by = c("business_id" = "business_id"))

sushi_reviews %>% 
  group_by(region,stars) %>% 
  summarise(sum = sum(isFresh), n = n(), percentage = sum/n)

df = sushi_reviews %>%  filter(isFresh==1)
M=table(df$region,df$stars)


total=as.data.frame(M) %>% group_by(Var2) %>% summarise(total=sum(Freq))
total=total$total
N <- as.table(rbind(total, M[1,]))
dimnames(N) <- list(region = c("total", "midwest"),
                     stars = c("1","2", "3",'4','5'))

Xsq <- chisq.test(N)
Xsq