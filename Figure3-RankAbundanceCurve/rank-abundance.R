# Rank Abundance curve
abundance.mags <- read.table("~/Box/PhD/Research/Lake-Tanganyika/1_Code/Figure3-RankAbundanceCurve/Abundance-MAGS.tsv", sep="\t", header=TRUE)
library(tidyverse)

small.abundance.mags <- abundance.mags %>% select(MAG, Taxonomy, Domain, Epi, Oxy, Hypo)
# Ok all good but we want simpler names
lookup <- read.table("~/Box/PhD/Research/Lake-Tanganyika/1_Code/Figure3-RankAbundanceCurve/Simple_taxo_lookup.tsv", header=TRUE, sep="\t")

small.abundance.mags <- left_join(small.abundance.mags, lookup, by="Taxonomy")
# Regroup the all Actinobacteria together


above.abund <- sum(small.abundance.mags$Epi)
oxy.abund <- sum(small.abundance.mags$Oxy)
below.abund <- sum(small.abundance.mags$Hypo)

summary.abundance.mags <- small.abundance.mags %>% group_by(Simplified.Taxonomy) %>% summarize(sumAbove = sum(Epi), meanAbove=mean(Epi), maxAbove=max(Epi), minAbove=min(Epi),
                                                                                    sumOxy = sum(Oxy), meanOxy=mean(Oxy), maxOxy=max(Oxy), minOxy=min(Oxy),
                                                                                    sumBelow = sum(Hypo), meanBelow=mean(Hypo), maxBelow=max(Hypo), minBelow=min(Hypo))

# Plot:
library(ggplot2)


top_20 <- arrange(summary.abundance.mags,desc(sumAbove)) %>% top_n(n=20, wt=sumAbove)

ggplot(top_20) + 
  geom_line(aes(x=seq(1:20),y=100*sumAbove/above.abund, colour='above'))+
  geom_point(aes(x=seq(1:20),y=100*sumAbove/above.abund,colour='above'))+
  #geom_errorbar(aes(x=seq(1:20), ymin=minAbove/above.abund, ymax=maxAbove/above.abund), width=.2,
  #              position=position_dodge(0.05),colour='#5384E0')+
  geom_line(aes(x=seq(1:20),y=100*sumOxy/oxy.abund,color='oxy'))+
  geom_point(aes(x=seq(1:20),y=100*sumOxy/oxy.abund,colour='oxy'))+
  #geom_errorbar(aes(x=seq(1:20), ymin=minOxy/oxy.abund, ymax=maxOxy/oxy.abund), width=.2,
  #              position=position_dodge(0.05),colour='#255957')+
  geom_line(aes(x=seq(1:20),y=100*sumBelow/below.abund,colour='below'))+
  geom_point(aes(x=seq(1:20),y=100*sumBelow/below.abund,colour='below'))+
  #geom_errorbar(aes(x=seq(1:20), ymin=minBelow/below.abund, ymax=maxBelow/below.abund), width=.2,
  #              position=position_dodge(0.05),colour='#F7C548')+
  scale_colour_manual(name = 'Lake Layer', 
                      values =c('above'='#5384E0','oxy'="#255957", 'below'="#F7C548"), labels = c('Above oxycline','At oxycline','Below oxycline'))+
  scale_x_continuous(breaks=seq(1:20),labels=top_20$Simplified.Taxonomy)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=20),
        legend.position = c(0.85, 0.80),legend.background = element_rect(color = "black", 
                                                                         fill = "white", size = 0.2, linetype = "solid"))+
  xlab("Taxonomic Group (ranked by coverage above the oxycline)")+
  ylab("Relative abundance \n of that group per layer (%)")
 

  
