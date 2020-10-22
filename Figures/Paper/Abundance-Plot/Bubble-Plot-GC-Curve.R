# Rank Abundance curve
abundance.mags <- read.table("~/Documents/Github/LakeTanganyika/Figures/Paper/Abundance-Plot/Abundance-MAGS-2020-02-25.tsv", sep="\t", header=TRUE)

#filter MIMAGS quals:
library(dplyr)

metabolism <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Metabolism-Summary/metabolism.tsv", header=T, sep="\t")
library(dplyr)
metabolism <- metabolism %>% filter(Completeness_checkm >= 50) %>% filter(Contamination_checkm < 10)

list.Mimags <- as.character(metabolism$MAG)
list.Mimags

library(tidyverse)

abundance.mags <- abundance.mags %>% filter(MAG %in% list.Mimags)

small.abundance.mags <- abundance.mags %>% select(MAG, Taxonomy, Domain)
# Ok all good but we want simpler names
lookup <- read.table("~/Documents/Github/LakeTanganyika/Figures/Paper/Abundance-Plot/Simple_taxo_lookup.tsv", header=TRUE, sep="\t")
lookup$Simplified.Taxonomy

small.abundance.mags <- left_join(small.abundance.mags, lookup, by="Taxonomy")

# Regroup the all Actinobacteria together
small.abundance.mags %>% filter(is.na(Simplified.Taxonomy))


## Bubble plot:
abundance.mags2 <- abundance.mags %>% select(-MAG,-Domain) %>% 
  group_by(Taxonomy)

abundance.mags2 <- left_join(abundance.mags2, lookup, by="Taxonomy")

summary(abundance.mags2)
abundance.mags2 <- abundance.mags2[,-1]

# Get the maximum value
abundance.mags2[, "max"] <- apply(abundance.mags2[, 1:24], 1, max)

#write.csv(abundance.mags2, "abundance.mags.to.normalize", row.names=FALSE)

# Load the data again:
abundance_df <- read.table("~/Documents/Github/LakeTanganyika/Figures/Paper/Abundance-Plot/abundance.relative.by.taxa.2020-10-21.txt", 
                           sep="\t",
                           header=TRUE)

abundance_df <- abundance_df %>% gather("Sample","Percent_Abundance", KigC250:Mahsurface_9)



# Great I would like to make an animation where this changes as a function of date.
sample.lookup.date <- read.table("~/Documents/Github/LakeTanganyika/Figures/Other/Cyano-vs-SQR-genomes/sample_look_up.tsv", sep="\t", header=T)
abundance_df_info <- left_join(abundance_df,sample.lookup.date)

library(lubridate)
abundance_df_info$Date <- ymd(paste(abundance_df_info$Date_year, abundance_df_info$Date_month, abundance_df_info$Date_day, sep="-"))

abundance_df_info %>%
ggplot(aes(x=Taxonomy, y=-Depth))+
  geom_point(aes(fill=Percent_Abundance), colour="black", size=3, pch=21)+
  facet_grid(Location+Date~., scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))+
  coord_cartesian(clip = "off")+
  scale_y_continuous(
    labels = scales::number_format(accuracy =  1))+
  scale_fill_gradient(low = "white", high="black")

abundance_df_info %>% 
  filter(Date != "2015-07-11") %>%
  filter(Date != "2015-07-14") %>%
  filter(Date != "2015-07-16") %>%
  filter(Date != "2015-07-18") %>%
  filter(Date != "2015-07-22") %>%
  ggplot(aes(x=Taxonomy, y=-Depth))+
  geom_point(aes(fill=Percent_Abundance), colour="black", size=3, pch=21)+
  facet_grid(Location+Date~., scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))+
  coord_cartesian(clip = "off")+
  scale_y_continuous(
    labels = scales::number_format(accuracy =  1))+
  scale_fill_gradient(low = "white", high="black")

ggsave("Paper/Abundance-Plot/Abundance.bubble.portrait.pdf", height = 11, width = 8.5, units = "in")

abundance_df_info %>% filter(Location == "Mahale") %>%
  filter(Date != "2015-07-21") %>%
  ggplot(aes(x=Taxonomy, y=-Depth))+
  geom_point(aes(fill=Percent_Abundance), colour="black", size=3, pch=21)+
  facet_grid(Date~., scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))+
  coord_cartesian(clip = "off")+
  scale_y_continuous(
    labels = scales::number_format(accuracy =  1))+
  scale_fill_gradient(low = "white", high="black")+
  ggtitle("Mahale, surface samples")

ggsave("Paper/Abundance-Plot/Abundance.bubble.Mahale.pdf", height = 8.5, width = 11, units = "in")

# Comparison of GC contents:
GC <- read.table("~/Documents/Github/LakeTanganyika/Figures/Paper/Abundance-Plot/GC-Content-LT.csv", sep="\t", header=T)

hist(GC$GC, xlim=c(0,1), n=50,
     main="Histrogram of GC content (%) in Lake Tanganyika MAGs (n=523 MAGs)",
     xlab="GC Content (%)",
     ylab="Frequency")

abundance.mags.GC <- left_join(abundance.mags, GC)


# Select the 100 most abundant MAGS from 1200m to compare to the 100 most abundant MAGS in Baikal:
top100 <- abundance.mags.GC %>% top_n(100, KigC1200)
top100.group <-top100 %>% count(Taxonomy)

top100.group
