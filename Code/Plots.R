library(mapview)
library(dplyr)
library(readr)
library(tidyr)
library(tigris)
library(sf)

#Load the data
sushi<- read.csv("sushi.csv")

# Distribution for Sushi Restaurants by stars given
hist(sushi$stars, main="Distribution of Stars for Sushi Restaurants", 
     xlab="Star Rating", ylab="Frequency",
     cex.lab=1.5, cex.main=1.75)

# Create data frame for Map creation
regions<- data.frame(state=state.abb, region=as.character(state.region))
regions<- regions %>% mutate(region= case_when(
  state == "DE" ~ "Northeast",
  TRUE~ region
)) %>% mutate(region = case_when(
  region == "North Central" ~"Midwest",
  TRUE ~ region
))

states<- states(cb=TRUE, resolution = "20m")
data_full<- inner_join(states, regions, by = c("STUSPS" = "state")) %>% filter(!STUSPS %in% c("HI", "AK")) %>% select("region", "STUSPS")

#Display map of where Restaurants lie
sushi_locations<- st_as_sf(sushi, coords = c("longitude", "latitude"), crs=4326) %>% st_jitter(factor = 0.005)

mapview(data_full, zcol="region", grid = FALSE, crs=4326, layer.name="Region", zoom=4) +
  mapview(sushi_locations, label=TRUE, cex=3, alpha=0.5, layer.name="Restaurants", zoom=4)
