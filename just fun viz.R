library(tidyverse)
library(maptools)
library(sp)
library(mapdata)
library(rgl)
library(rgdal)
library(ggplot2)
library(rgeos)
precinct <- readOGR("C:/Users/bingo/Desktop/CUA HaXS/Voting_Precinct__2012/Voting_Precinct__2012.shp") 
ward <- readOGR("C:/Users/bingo/Desktop/CUA HaXS/Ward_from_2012/Ward_from_2012.shp")
precinct2 <- fortify(precinct)
ward2 <- fortify(ward)
precinctdat <- as.data.frame(precinct)
map1 <- ggplot() + 
  geom_polygon(data = precinct2, aes(long, lat, group = group), 
               colour = "black", fill = "white") +
  geom_polygon(data = ward2, aes(long, lat, group = group, fill = group),
               colour = "black", alpha = 0.3, size = 1) +
  ggtitle("DC Precincts and Wards",
          subtitle = "Est. 2012") +
  coord_quickmap()+
  theme_minimal()
map1
#opens up possibility of DC chloropleth map (interactive too)
library(ggmap)
library(sf)
DCMAP <- get_map("Washington, DC", zoom = 12)
wardsp <- spTransform(ward, CRS("+proj=longlat +datum=WGS84"))
wardsp2 <- fortify(wardsp)
#Add another column for ward
WARDWITHLABELS <- ward2 %>%
  mutate(Ward = 
           case_when(
             group == 0.1 ~ 8,
             group == 1.1 ~ 6,
             group == 2.1 ~ 7,
             group == 3.1 ~ 2,
             group == 4.1 ~ 1, 
             group == 5.1 ~ 5,
             group == 6.1 ~ 3,
             group == 7.1 ~ 4
           ))
WARDWITHLABELS$Ward <- factor(WARDWITHLABELS$Ward)
# Get centroids to plot labels
ward_Cent <- gCentroid(ward, byid = TRUE)
ward_CentPoints <- as.data.frame(ward_Cent)
Wardlabs <- ward_CentPoints %>%
  cbind(c(8, 6, 7, 2, 1, 5, 3, 4))
colnames(Wardlabs) <- c("x", "y", "WARD")
# Plot the points on the wards
ggplot() +
  geom_polygon(data = WARDWITHLABELS, aes(long, lat, group = group, fill = Ward),
               colour = "black", alpha = 0.3, size = 1) +
  geom_text(data = Wardlabs, aes(x, y, label = WARD), size = 5) +
  ggtitle("DC Wards",
          subtitle = "Est. 2012") +
  theme(legend.position = "none") +
  coord_quickmap()
# revision of map1
ggplot() + 
  geom_polygon(data = precinct2, aes(long, lat, group = group), 
               colour = "black", fill = "white") +
  geom_polygon(data = WARDWITHLABELS, aes(long, lat, group = group, fill = Ward),
               colour = "black", alpha = 0.3, size = 1) +
  ggtitle("DC Precincts and Wards",
          subtitle = "Est. 2012") +
  coord_quickmap()

