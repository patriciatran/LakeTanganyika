# these are packages you will need, but probably already have.
# Don't bother installing if you already have them
#install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))

# some standard map packages.
#install.packages(c("maps", "mapdata"))

# the github version of ggmap, which recently pulled in a small fix I had
# for a bug 
#devtools::install_github("dkahle/ggmap")


#install.packages("maps")
library(maps)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

sort(unique(ggplot2::map_data("world")$region))
sort(unique(ggplot2::map_data("world")$map))

#map('world')
#lakes.map <- map('lakes', add=TRUE, fill=TRUE, col='white', boundary='black')

countries <- c("Burundi","Tanzania","Democratic Republic of the Congo","Malawi","Zambia")
countries.map <- map_data("world", region=countries, exact=FALSE)

# Lakes:
sort(unique(ggplot2::map_data("lakes")$region))
#sort(unique(ggplot2::map_data("lakes")$subregion))
Tanganyika.map <- map_data("lakes", region="Lake Tanganyika", exact=TRUE)
#all.lakes.map <- map_data("lakes", subregion=NA)

#combined.map$group
Tanganyika.map$group <- 11

# Combine Countries + Lakes
combined.map <- bind_rows(countries.map, Tanganyika.map)
factor(combined.map$group)


#combined.map$group[combined.map$region == "Lake Tanganyika"] <- 11
str(combined.map)
combined.map$group <- as.numeric(combined.map$group)

# Replace the group for Tanganyika to be 11 instead of 10
library(dplyr)

region.lab.data <- combined.map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

min.lat.lake <- min(Tanganyika.map$lat)
max.lat.lake<-max(Tanganyika.map$lat)
min.long.lake<-min(Tanganyika.map$long)
max.long.lake<-max(Tanganyika.map$long)

# LAt is y axis
min.lat.lake
max.lat.lake
# Long is x axis
min.long.lake
max.long.lake


# Load the samples lat and long:
samples <- read.csv("~/Box/PhD/Research/Lake-Tanganyika/1_Code/Samples-Tanganyika-PT.csv")

#bc_bbox <- make_bbox(lat = lat, lon = long, data = Tanganyika.map)

#bc_big <- get_map(location = bc_bbox, source = "google", maptype = "terrain")


ggplot(combined.map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  coord_fixed(xlim = c(min.long.lake-1.0, max.long.lake+1.0),  ylim = c(min.lat.lake, max.lat.lake), ratio = 1.3)+
  # Add the samples
  geom_point(data=samples, aes(x=Longitude, y=Latitude))

#bc_bbox <- make_bbox(lat = lat, lon = long, data = Tanganyika.map)
#bc_bbox
#?register_google
#bc_big <- get_map(location = bc_bbox, source = "google", maptype = "terrain")
