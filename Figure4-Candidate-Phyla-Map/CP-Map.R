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

mpworld <- mp+ geom_point(aes(data2$Longitude,data2$Latitude, colour = factor(data2$Ecosystem.Plot), shape=data2$MAG), 
                     size=3)+
 # scale_shape_manual(name="Group", values=c(0,1, 2, 15, 16, 17, 18))+
  #ggtitle("Distribution of CP Tanganyikabacteria & CP Ziwabacteria")+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_bw()+
  theme(legend.position = "bottom")

mpworld


library(dplyr)

count.data <- data2 %>% group_by(MAG) %>% count(Ecosystem.Plot)
for (i in 1:nrow(count.data)){
  if(count.data$MAG[i] == "M_DeepCastt_65m_m2_071"){
    count.data$n[i] <- -(count.data$n[i])
  }
}


count.data

bar.count <- ggplot(count.data, aes(x=Ecosystem.Plot, y=n, fill=MAG))+
         geom_bar(stat="identity", position="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Ecosystem Types")+
  ylab("Count")+
  coord_flip()+
  theme(legend.position = "bottom")

bar.count

library(ggpubr)

ggarrange(mpworld, bar.count, ncol=2, labels=c("A","B"), widths=c(3,1))
                                                   