samples <- read.csv("~/Desktop/samples.csv", sep="\t")

library(tidyverse)
library(lubridate)
library(ggrepel)
ggplot(samples, aes(x=mdy(Date),y=-Depth, color=Location))+
  geom_point()+
  geom_label_repel(aes(label=ShortenedName, size=30))+
  theme_bw()+
  ylab("Depth (m)")+
  xlab("Sampling Date")+
  facet_grid(.~Location, scales="free")

full.list <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Samples/DataTypes.csv", header=TRUE)
full.list$Date <- ymd(full.list$Date)

unique(full.list)

ggplot(unique(full.list), aes(x=Date,y=Dummy))+
  geom_point(pch=21, color="black", fill="black", size=3, alpha=0.5)+
  facet_grid(Data.Type.Available~Year, scales="free")+
  theme_bw()
