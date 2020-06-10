# NOB vs Annamox Tanganyika MAGs with depth

abundance <- read.table("~/Documents/Github/LakeTanganyika/Figure3-RankAbundanceCurve/Abundance-MAGS-2020-02-25.tsv", sep="\t", header=TRUE)
com <- abundance %>% filter(MAG=="M_DeepCast_65m_mx_150"| MAG=="M_DeepCast_50m_m2_151")


library(tidyverse)
nob.anammox.gathered <- nob.anammox %>% gather(key="MAG", value="Coverage",c(2:61))

# Add a column with guild indo:
guild.info <- read.table("~/Documents/Github/LakeTanganyika/Figure5D-NOB-Anammox/key_taxonomy.tsv", sep="\t", header=TRUE)

nob.anammox.gathered <- left_join(nob.anammox.gathered, guild.info, by="MAG")
com <- nob.anammox.gathered %>% filter(Guild=="comammox")

colnames(com)[1] <- "Depth"

ggplot(com) + geom_point(aes(x=Coverage, y=Depth, color=MAG))+
  scale_y_reverse(lim=c(1250,0))+
  theme_bw()+
  xlab("Coverage in metagenome")+
  ylab("Depth (m)")+
  geom_hline(yintercept=50, linetype="dashed", color = "grey")+
  geom_hline(yintercept=100, linetype="dashed", color = "grey")+
  geom_hline(yintercept=70, linetype="dashed", color = "red")+
  ggtitle("Distribution of Comammox in Lake Tanganyika")


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

