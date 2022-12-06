# take business_id=='vC2qm1y3Au5czBtbhc-DNw' for example

s <- read.csv('Data/sentiment/vC2qm1y3Au5czBtbhc-DNw.csv')
barplot(
  sort(colSums(prop.table(s[, 1:8]))),
  horiz = TRUE,
  col='skyblue',
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Reviews", xlab="Percentage"
)
