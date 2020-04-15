# Rank Abundance curve
abundance.mags <- read.table("~/Documents/Github/LakeTanganyika/Figure3-RankAbundanceCurve/Abundance-MAGS-2020-02-25.tsv", sep="\t", header=TRUE)

#filter MIMAGS quals:
library(dplyr)

metabolism <- read.csv("~/Documents/Github/LakeTanganyika/FigureS7-Metabolism-Summary/metabolism.tsv", header=T, sep="\t")
library(dplyr)
metabolism <- metabolism %>% filter(Completeness_checkm >= 50) %>% filter(Contamination_checkm < 10)

list.Mimags <- as.character(metabolism$MAG)
list.Mimags

library(tidyverse)

abundance.mags <- abundance.mags %>% filter(MAG %in% list.Mimags)

small.abundance.mags <- abundance.mags %>% select(MAG, Taxonomy, Domain)
# Ok all good but we want simpler names
lookup <- read.table("~/Documents/Github/LakeTanganyika/Figure3-RankAbundanceCurve/Simple_taxo_lookup.tsv", header=TRUE, sep="\t")
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


 
levels(abundance_df$Sample)
#abundance_df$Sample_f <- factor(abundance_df$Sample, levels(abundance_df$Sample)[c(16, 15, 14, 19, 18, 17, 20, 24, 23, 22, 21, 2, 5, 4, 3, 10, 1, 12, 7, 11, 6, 13, 9, 8)])


#abundance_df$Sample_f

#ggplot(abundance_df, aes(x = Taxonomy, y = Sample_f, size = NormalizedCoverage, colour=NormalizedCoverage)) + 
#  geom_point()+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Great I would like to make an animation where this changes as a function of date.
sample.lookup.date <- read.table("~/Documents/Github/LakeTanganyika/FigureS4-Cyano-vs-SQR-genomes/sample_look_up.tsv", sep="\t", header=T)
abundance_df_info <- left_join(abundance_df,sample.lookup.date)

library(lubridate)
abundance_df_info$Date <- ymd(paste(abundance_df_info$Date_year, abundance_df_info$Date_month, abundance_df_info$Date_day, sep="-"))

abundance_df_info %>% filter(Date=="2015-07-25") %>%
ggplot(aes(x=Taxonomy, y=-Depth, size=NormalizedCoverage, colour=NormalizedCoverage ))+
  geom_point()+
  #facet_grid(abundance_df_info$Date, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))

# ###
# 
# above.abund <- sum(small.abundance.mags$Epi)
# oxy.abund <- sum(small.abundance.mags$Oxy)
# below.abund <- sum(small.abundance.mags$Hypo)
# 
# summary.abundance.mags <- small.abundance.mags %>% group_by(Simplified.Taxonomy) %>% summarize(sumAbove = sum(Epi), meanAbove=mean(Epi), maxAbove=max(Epi), minAbove=min(Epi),
#                                                                                                sumOxy = sum(Oxy), meanOxy=mean(Oxy), maxOxy=max(Oxy), minOxy=min(Oxy),
#                                                                                                sumBelow = sum(Hypo), meanBelow=mean(Hypo), maxBelow=max(Hypo), minBelow=min(Hypo))
# 
# summary.abundance.mags$TotalSum <- 0
# for (i in 1:nrow(summary.abundance.mags)){
#   summary.abundance.mags$TotalSum[i] <- sum(summary.abundance.mags$sumAbove[i], summary.abundance.mags$sumOxy[i], summary.abundance.mags$sumBelow[i])
#   print(i)
# }
# 
# class(summary.abundance.mags)
# # Plot:
# library(ggplot2)
# 
# 
# top_20 <- top_n(x=summary.abundance.mags, n=20, wt=TotalSum)
# 
# ggplot(top_20) + 
#   geom_line(aes(x=seq(1:20),y=100*sumAbove/above.abund, colour='above'))+
#   geom_point(aes(x=seq(1:20),y=100*sumAbove/above.abund,colour='above'))+
#   #geom_errorbar(aes(x=seq(1:20), ymin=minAbove/above.abund, ymax=maxAbove/above.abund), width=.2,
#   #              position=position_dodge(0.05),colour='#5384E0')+
#   geom_line(aes(x=seq(1:20),y=100*sumOxy/oxy.abund,color='oxy'))+
#   geom_point(aes(x=seq(1:20),y=100*sumOxy/oxy.abund,colour='oxy'))+
#   #geom_errorbar(aes(x=seq(1:20), ymin=minOxy/oxy.abund, ymax=maxOxy/oxy.abund), width=.2,
#   #              position=position_dodge(0.05),colour='#255957')+
#   geom_line(aes(x=seq(1:20),y=100*sumBelow/below.abund,colour='below'))+
#   geom_point(aes(x=seq(1:20),y=100*sumBelow/below.abund,colour='below'))+
#   #geom_errorbar(aes(x=seq(1:20), ymin=minBelow/below.abund, ymax=maxBelow/below.abund), width=.2,
#   #              position=position_dodge(0.05),colour='#F7C548')+
#   scale_colour_manual(name = 'Lake Layer', 
#                       values =c('above'='#5384E0','oxy'="#255957", 'below'="#F7C548"), labels = c('Above oxycline','At oxycline','Below oxycline'))+
#   scale_x_continuous(breaks=seq(1:20),labels=top_20$Simplified.Taxonomy)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         text = element_text(size=20),
#         legend.position = c(0.85, 0.80),legend.background = element_rect(color = "black", 
#                                                                          fill = "white", size = 0.2, linetype = "solid"))+
#   xlab("Taxonomic Group (ranked by coverage above the oxycline)")+
#   ylab("Relative abundance \n of that group per layer (%)")
# 
# top_20.filter <- top_20 %>% select(Simplified.Taxonomy, sumAbove, sumOxy, sumBelow)
# 
# class(top_20.filter)
# 
# top_20.filter <- top_20.filter %>% gather(key="LayerSum",value="Coverage", c(2,3,4))
# 
# str(top_20.filter)
# 
# #top_20.filter <- top_20.filter %>% arrange(desc(Coverage))
# 
# order <- top_20 %>% arrange(desc(TotalSum)) 
# order
# order.I.want <- as.factor(order$Simplified.Taxonomy)
# 
# order.I.want
# 
# #top_20.filter$Simplified.Taxonomy_f <- as.factor(top_20.filter$Simplified.Taxonomy)
# 
# 
# # convert to relative abundance:
# top_20.filter$relative.abund <- 0
# 
# for (i in 1:nrow(top_20.filter)){
#   if (top_20.filter$LayerSum[i] == "sumAbove"){
#     top_20.filter$relative.abund[i] <- top_20.filter$Coverage[i]/above.abund
#   }
#   if (top_20.filter$LayerSum[i] == "sumOxy"){
#     top_20.filter$relative.abund[i] <- top_20.filter$Coverage[i]/oxy.abund
#   }
#   else {
#     top_20.filter$relative.abund[i] <- top_20.filter$Coverage[i]/below.abund
#   }
#   print(i)
# }
# 
# str(top_20.filter)
# 
# #lvls <- names(sort(tapply(top_20.filter$Simplified.Taxonomy == "B", data$x, mean)))
# 
# ggplot(top_20.filter, aes(x=factor(Simplified.Taxonomy, levels=order.I.want), y=relative.abund, fill=LayerSum)) + 
#   geom_bar(stat = "identity")+
#   #facet_grid(LayerSum~.)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         text = element_text(size=20),
#         legend.position = c(0.85, 0.85),
#         legend.background = element_rect(color = "black", fill = "white", size = 0.2, linetype = "solid"))+
#   scale_fill_manual(name = ('Layer'), 
#                     values =c('sumAbove'='#5384E0','sumOxy'="#255957", 'sumBelow'="#F7C548"), 
#                     labels = c('Oxic Layer (0-50m)','Sub-oxic Layer (50-100m)','Anoxic Layer (100-1200m)'))+
#   xlab("Taxonomic Group, ranked by total abundance across all layers")+
#   ylab("Relative Abundance (%)")
# 
#   

GC <- read.table("~/Documents/Github/LakeTanganyika/Figure3-RankAbundanceCurve/GC-Content-LT.csv", sep="\t", header=T)

hist(GC$GC, xlim=c(0,1), n=50,
     main="Histrogram of GC content (%) in Lake Tanganyika MAGs (n=523 MAGs)",
     xlab="GC Content (%)",
     ylab="Frequency")

abundance.mags.GC <- left_join(abundance.mags, GC)


# Select the 100 most abundant MAGS from 1200m to compare to the 100 most abundant MAGS in Baikal:
top100 <- abundance.mags.GC %>% top_n(100, KigC1200)
top100.group <-top100 %>% count(Taxonomy)
