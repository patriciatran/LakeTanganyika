# NOB vs Annamox Tanganyika MAGs with depth

nob.anammox <- read.table("~/Documents/Github/LakeTanganyika/Figure5D-NOB-Anammox/NOB_Annamox_R.tsv", sep="\t", header=TRUE)

library(tidyverse)
nob.anammox.gathered <- nob.anammox %>% gather(key="MAG", value="Coverage",c(2:61))

# Add a column with guild indo:
guild.info <- read.table("~/Documents/Github/LakeTanganyika/Figure5D-NOB-Anammox/key_taxonomy.tsv", sep="\t", header=TRUE)

nob.anammox.gathered <- left_join(nob.anammox.gathered, guild.info, by="MAG")

colnames(nob.anammox.gathered)[1] <- "Depth"

library(ggplot2)

ggplot(nob.anammox.gathered) + geom_point(aes(x=Coverage, y=Depth, color=Taxonomy))+
  facet_grid(.~Guild)+
  scale_y_reverse(lim=c(1250,0))+
  theme_bw()+
  xlab("Coverage in metagenome")+
  ylab("Depth (m)")+
  geom_hline(yintercept=50, linetype="dashed", color = "grey")+
  geom_hline(yintercept=100, linetype="dashed", color = "grey")+
  geom_hline(yintercept=70, linetype="dashed", color = "red")+
  ggtitle("Distribution of Comammox, NOB and Anammox in Lake Tanganyika")

