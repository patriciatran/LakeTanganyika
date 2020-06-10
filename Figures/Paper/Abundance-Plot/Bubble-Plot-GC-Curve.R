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

abundance.mags2 <- abundance.mags2 %>% group_by(Simplified.Taxonomy) %>% summarise_all(funs(mean))

rownames.abundance <- as.character(abundance.mags2$Simplified.Taxonomy)
abundance.mags2 <- abundance.mags2[,-1]
rownames(abundance.mags2) <- rownames.abundance

library(reshape)
abundance_df = melt(as.matrix(abundance.mags2))
names(abundance_df) = c('Taxonomy', 'Sample', 'NormalizedCoverage')

ggplot(abundance_df, aes(x = Taxonomy, y = Sample, size = NormalizedCoverage, colour=NormalizedCoverage)) + 
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


data2 <- sweep(abundance.mags2,MARGIN=2,FUN="/",STATS=colSums(abundance.mags2))
data3 <- round(data2*100, digits=2)

#abundance.relative <- round((abundance.mags2/colSums(abundance.mags2))*100, digit=2)

levels(abundance_df$Sample)

# Great I would like to make an animation where this changes as a function of date.
sample.lookup.date <- read.table("~/Documents/Github/LakeTanganyika/Figures/Other/Cyano-vs-SQR-genomes/sample_look_up.tsv", sep="\t", header=T)
abundance_df_info <- left_join(abundance_df,sample.lookup.date)

library(lubridate)
abundance_df_info$Date <- ymd(paste(abundance_df_info$Date_year, abundance_df_info$Date_month, abundance_df_info$Date_day, sep="-"))

ggplot(abundance_df_info,aes(x=Taxonomy, y=-Depth, size=NormalizedCoverage, colour=NormalizedCoverage ))+
  geom_point()+
  facet_grid(abundance_df_info$Date, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))+
  coord_cartesian(clip = "off")+
  scale_y_continuous(
    labels = scales::number_format(accuracy =  1))


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
