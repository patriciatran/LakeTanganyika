cyano_iRep <- read.csv("~/Box/PhD/Research/Lake-Tanganyika/0_Data/Cyano-MAGs/iRep_Cyano.csv")
library(ggplot2)

my_plot <- ggplot(cyano_iRep, aes(x=raw.index.of.replication, y=Depth)) + 
  geom_point(aes(color=Location))+
  facet_grid(. ~ CyanoMAG.name)+
  #coord_flip()+
  scale_y_reverse(lim=c(1250,0))


my_plot

my_plot <- my_plot + facet_grid(. ~ CyanoMAG.name)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        text = element_text(size=15))+
  xlab("Index of Replication (iRep)")+
  ylab("Depth (m)")+
  geom_hline(yintercept=50, linetype="dashed", color = "grey")+
  geom_hline(yintercept=100, linetype="dashed", color = "grey")+
  geom_hline(yintercept=70, linetype="dashed", color = "red")+
  ggtitle("Index of replication of Cyanobacteria MAGs in Lake Tanganyika")

  
cyano.abund <- read.table("~/Box/PhD/Research/Lake-Tanganyika/0_Data/Cyano-MAGs/Abundance-Cyano-iRep.tsv", header=TRUE)
cyano.abund <- cyano.abund[,-1]
library(reshape2)
library(dplyr)
library(tidyverse)

cyano.abund.gathered <- cyano.abund %>% gather(key="MAG", value="Coverage",c(1,2,3))

# Do some plot but for abundances:

my_plot_abund <- ggplot(cyano.abund.gathered, aes(x=Coverage, y=Depth)) + 
  geom_point(aes(color=Location))+
  facet_grid(. ~ MAG)+
  #coord_flip()+
  scale_y_reverse(lim=c(1250,0))

my_plot_abund <- my_plot_abund+
  facet_grid(. ~MAG)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        text = element_text(size=15))+
  xlab("Coverage in metagenome")+
  ylab("Depth (m)")+
  geom_hline(yintercept=50, linetype="dashed", color = "grey")+
  geom_hline(yintercept=100, linetype="dashed", color = "grey")+
  geom_hline(yintercept=70, linetype="dashed", color = "red")+
  ggtitle("Coverage of Cyanobacteria MAGs in Lake Tanganyika")

library(ggpubr)

ggarrange(my_plot, my_plot_abund, labels="AUTO", ncol=1, nrow=2, common.legend = TRUE)

my_plot + geom_point(data=cyano.abund.gathered, aes(x=Coverage/10, y=Depth, shape=Location))+
  facet_grid(.~MAG)+
  scale_x_continuous(sec.axis = sec_axis(~.*10, name="Coverage"))+
  ggtitle("Cyanobacteria MAGs in Lake Tanganyika")


