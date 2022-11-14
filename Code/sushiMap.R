library(sf)
library(mapview)

sushi <- read.csv("../sushi.csv")

sushi_reviews <- read.csv("sushi_reviews.csv")

locations <- sushi %>% 
  select(business_id, longitude, latitude)


temp <- sushi_reviews %>% 
  group_by(business_id) %>% 
  summarise(sum = sum(isFresh),n = n(), percentage = sum/n)

sushi_reviews <- sushi_reviews %>% 
  left_join(temp, by = c("business_id" = "business_id"))

temp <-  temp %>% 
  left_join(locations, by = c("business_id" = "business_id"))


#Map out the locations 
mapview(locations, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)


sbux_sf <- st_as_sf(locations, coords = c("longitude", "latitude"),  crs = 4326)
mapview(sbux_sf, map.types = "Stamen.Toner") 



temp <- data.frame(temp)

ggplot(temp) +
  geom_jitter(data = temp, mapping = aes(x = longitude, y = latitude, size=percentage, colour = "red", alpha = .01))
  

#coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)