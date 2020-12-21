samples <- read.csv("Paper/Samples/samples.tsv", sep="\t")

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

## Make a fake label:
samples[25,]$Depth = 1200
samples[25,]$Location = "Mahale"
samples[25,]$ShortenedName = "dummy_sample"


## To make a break, we can facet:
samples$group <- ""

for (i in 1:nrow(samples)){
  if (samples$Depth[i] < 500){
    samples$group[i] <- "1"
  }
  else{
    samples$group[i] <- "2"
  }
}

my_theme <- 

plot1.kigoma <- ggplot(samples %>% filter(group == "1" & Location == "Kigoma"), aes(x=mdy(Date),y=-Depth))+
  geom_point(colour="red", size=2)+
  xlim(mdy("7/11/15"), mdy("10/30/15"))+
  ylim(-400,0)+
  geom_label_repel(aes(label=ShortenedName, size=30), color="black", label.size = NA, fill=NA)+
  theme_bw()+
  scale_colour_manual(values = c("red","blue"))+
  ylab("Depth (m)")+
  xlab("Sampling Date")+
  facet_grid(.~Location, space="free_y", scales = "free_x")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_rect(fill = 'white', colour = 'black'),
        strip.text = element_text(size=12),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=15),
        axis.text = element_text(colour = "black"))
plot1.kigoma

plot1.mahale <- ggplot(samples %>% filter(group == "1" & Location == "Mahale"), aes(x=mdy(Date),y=-Depth))+
  geom_point(colour="blue", size=2)+
  ylim(-400,0)+
  xlim(mdy("7/11/15"), mdy("7/22/15"))+
  geom_label_repel(aes(label=ShortenedName, size=30), color="black", label.size=NA, fill=NA)+
  theme_bw()+
  scale_colour_manual(values = c("red","blue"))+
  ylab("Depth (m)")+
  xlab("Sampling Date")+
  facet_grid(.~Location, space="free_y", scales = "free_x")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.background = element_rect(fill = 'white', colour = 'black'),
        strip.text = element_text(size=12),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=15),
        axis.text = element_text(colour = "black"))

plot1.mahale

plot2.kigoma <- ggplot(samples %>% filter(group == "2"), aes(x=mdy(Date),y=-Depth))+
  geom_point(colour="red",size=2)+
  facet_grid(.~Location)+
  ylim(-1100,-1300)+
  xlim(mdy("7/11/15"), mdy("10/30/15"))+
  geom_label_repel(aes(label=ShortenedName, size=30), color="black", label.size = NA, fill=NA)+
  theme_bw()+
  scale_colour_manual(values = c("red","blue"))+
  ylab("Depth (m)")+
  xlab("Sampling Date")+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size=15),
        axis.text = element_text(colour = "black"))


plot2.kigoma

library(patchwork)

(plot1.kigoma + plot1.mahale)/ plot2.kigoma+
  plot_layout(heights = c(2.5, 1))

ggsave("Paper/Samples/Figure1B_Dec2020.PDF", height = 11, width = 8.5, units = "in")

full.list <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Samples/DataTypes.csv", header=TRUE)
full.list$Date <- ymd(full.list$Date)

unique(full.list)

ggplot(unique(full.list), aes(x=Date,y=Dummy))+
  geom_point(pch=21, color="black", fill="black", size=3, alpha=0.5)+
  facet_grid(Data.Type.Available~Year, scales="free")+
  theme_bw()
