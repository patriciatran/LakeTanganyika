data <- read.csv("~/Documents/Github/LakeTanganyika/Figure4-Candidate-Phyla-Map/CP-Coordinates-IMG.csv")

str(data)

data
str(data)


library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

## Lookup the simplified categories to plot:
categories <- read.csv("~/Documents/Github/LakeTanganyika/Figure4-Candidate-Phyla-Map/Ecosystem.Plot.Categories.csv", header=TRUE)

data <- data %>% mutate(matching.column = paste0(Ecosystem, Ecosystem.Category,Ecosystem.Subtype, Ecosystem.Type))


library(dplyr)

data2 <- left_join(data, categories, by="matching.column")
summary(data2)
str(data2)

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

# If we plot it now we can see that's it's the world map, but doesn't have any points on it.
mp

#Now Layer the coordinates on top:

install.packages("devtools") 
devtools::install_github("jakelawlor/PNWColors")
library(PNWColors)
names(pnw_palettes)

length(levels(data2$Ecosystem.Plot))

cols <- c("Engineered" = "#A00E65", 
          "Aquatic Freshwater Lake" = "#04A39F", 
          "Aquatic Freshwater Other" = "#33A2D3",
          "Aquatic Freshwater Sediment" = "#64E0ED",
          "Aquatic Marine"="#19419B",
          "Aquatic Other"="#7F6DCC",
          "Aquatic Sediment"="#24EDD9",
          "Terrestrial"="#53B74E",
          "Host-Associated"="#036D09"
          )

mpworld <- mp+ geom_point(aes(x=data2$Longitude,y=data2$Latitude, colour = factor(data2$Ecosystem.Plot), shape=data2$Phyla), 
                     size=2)+
 # scale_shape_manual(name="Group", values=c(0,1, 2, 15, 16, 17, 18))+
  #ggtitle("Distribution of CP Tanganyikabacteria & CP Ziwabacteria")+
  scale_colour_manual(values = cols)+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_bw()+
  theme(legend.position = "bottom")

mpworld

library(dplyr)

count.data <- data2 %>% group_by(Phyla, MAG) %>% count(Ecosystem.Plot)

for (i in 1:nrow(count.data)){
  if(count.data$Phyla[i] == "CP Tanganyikabacteria"){
    count.data$n[i] <- -(count.data$n[i])
  }
}


count.data

bar.count <- ggplot(count.data, aes(x=Ecosystem.Plot, y=n, fill=MAG))+
         geom_bar(stat="identity", position="identity")+
  theme_bw()+
  scale_fill_grey()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Ecosystem Types")+
  ylab("Count")+
  ylim(c(-200,200))+
  coord_flip()+
  theme(legend.position = "bottom")

bar.count

library(ggpubr)

ggarrange(mpworld, bar.count, ncol=2, labels=c("C","D"), widths=c(3,1))

                                                   