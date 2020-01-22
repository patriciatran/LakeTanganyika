# MAG Characteristics : Supplementary Figures

#MIMAGS
mags <- read.table("~/Documents/Github/LakeTanganyika/MAG_Characteristics/MAG_chars.tsv", sep="\t", header=TRUE)

library(dplyr)

mimags <- mags %>% filter(Genome.completeness.CheckM >= 50 & Genome.contamination.CheckM < 10)

plot(mimags$GC, mimags$Genome.completeness.CheckM)

#compare Completeness CheckM and DasTool
library(ggplot2)

# for (i in 1:nrow(mimags)){
#   if(mimags$Candidate.phyla[i] == "X"){
#     mimags$Candidate.phyla[i] = "is a CP"
#   }
#   else{
#     mimags$Candidate.phyla[i] = "Not a CP"
#   }
# }

completeness.plot <- ggplot(mags, aes(x= Genome.completeness.CheckM, y= Completeness.DasTool, colour=factor(Candidate.phyla), shape=Domain))+
  geom_point(size=3, alpha=0.5)+
  ylim(0,100)+
  xlim(0,100)+
  geom_abline()+
  theme_bw()+
  ylab("Genome Completeness % (DasTool)")+
  xlab("Genome Completeness % (CheckM)")+
  facet_grid(.~Domain)+
  annotate("rect", xmin = 0, xmax = 50, ymin = 0, ymax = 100,
           alpha = .25)

contam.plot <- ggplot(mags, aes(x= Genome.contamination.CheckM, y= Contamination.DasTool, colour=factor(Candidate.phyla), shape=Domain))+
  geom_point(size=3, alpha=0.5)+
  ylim(0,100)+
  xlim(0,100)+
  geom_abline()+
  theme_bw()+
  ylab("Genome Contamination % (DasTool)")+
  xlab("Genome Contamination % (CheckM)")+
  facet_grid(.~Domain)+
  annotate("rect", xmin = 10, xmax = 100, ymin = 0, ymax = 100,
           alpha = .25)+

# Genomes to keep: MIMAGS

library(tidyr)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

library(RColorBrewer)

mimags.GC <- mimags %>% separate(Taxonomy.GTDB.Tk, c("Domain","Phylum","Class","Order","Family","Genus","Species"), sep=";") %>%
  group_by(Phylum, Domain) %>%
  summarise("mean_GC" = mean(GC)) %>%
  ggplot(aes(x=reorder(Phylum,-mean_GC), y=mean_GC, fill=factor(Domain)))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Mean %GC")+
  xlab("Phylum (according to GTDB-tk)")+
  scale_fill_brewer(palette="Set2")

#mimags.GC
#annotate("rect", xmin = 0, xmax = , ymin = 0, ymax = 100,
#           alpha = .25)


library(ggpubr)
ggarrange(completeness.plot, contam.plot, mimags.GC, ncol=1, nrow=3, 
          common.legend=TRUE, labels=c("A","B","C"))



  