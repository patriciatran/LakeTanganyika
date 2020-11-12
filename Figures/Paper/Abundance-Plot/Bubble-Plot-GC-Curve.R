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

abundance_df <- abundance_df %>% gather("Sample","Percent_Abundance", KigC100:Mahsurface_9)

abundance_df$Taxonomy <- str_replace(abundance_df$Taxonomy, "CP Verstraetaerchaeota", "CP Verstratearchaeota")
abundance_df$Taxonomy <- str_replace(abundance_df$Taxonomy, "Kirimatiellacea", "Kirimatiellaeota")
abundance_df$Taxonomy <- str_replace(abundance_df$Taxonomy, "CP Aminicemantes (OP8)", "CP Aminicenantes (CP8)")



# Great I would like to make an animation where this changes as a function of date.
sample.lookup.date <- read.table("~/Documents/Github/LakeTanganyika/Figures/Other/Cyano-vs-SQR-genomes/sample_look_up.tsv", sep="\t", header=T)
abundance_df_info <- left_join(abundance_df,sample.lookup.date, by="Sample")

abundance_df_info %>% filter(is.na(Taxonomy))


# Replace taxon name (spelling mistakes)

library(lubridate)
abundance_df_info$Date <- ymd(paste(abundance_df_info$Date_year, abundance_df_info$Date_month, abundance_df_info$Date_day, sep="-"))

order.taxon.table <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Abundance-Plot/OrderTaxon.csv", header=TRUE)
#order.taxon.table$Manual.Taxonomy <- as.factor(order.taxon.table$Manual.Taxonomy, levels=order.taxon.table$Manual.Taxonomy)

order.I.want <- factor(order.taxon.table$Manual.Taxonomy, levels=order.taxon.table$Manual.Taxonomy)

# Order that I want:

abundance_df_info$Taxonomy <- factor(abundance_df_info$Taxonomy, levels=order.I.want)
abundance_df_info$Taxonomy.copy <- as.character(abundance_df_info$Taxonomy)

abundance_df_info <- left_join(abundance_df_info, order.taxon.table, by=c("Taxonomy.copy" = "Manual.Taxonomy"))

## Bubble Plot
abundance_df_info%>%
ggplot(aes(x=Taxonomy, y=-Depth))+
  geom_point(aes(fill=Percent_Abundance), colour="black", size=3, pch=21)+
  facet_grid(Location+Date~Domain+CPR, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))+
  coord_cartesian(clip = "off")+
  scale_y_continuous(
    labels = scales::number_format(accuracy =  1))+
  scale_fill_gradient(low = "white", high="black")

ggsave("Paper/Abundance-Plot/Full.bubble.plot.PDF", height = 8, width = 11, units="in")


abundance_df_info$Sample <- factor(abundance_df_info$Sample, levels=c("KigCas0", "KigCa35", "KigCa65", "KigC100", "KigC150", "KigC250", "KigC300", "KigC1200", "KigOffSurface", "KigOff0", "KigOff40", "KigOff80", "KigOff120", "Mahsurface_6", "Mahsurface_9", "Mahsurface_8", "Mahsurface_7", "MahCa10", "MahCa50", "MahCa65", "MahC100", "MahC200", "MahC400", "Mahsurface_10"))

abundance_df_info %>% filter(is.na(Domain))

abundance_df_info %>% 
  filter(Date != "2015-07-11") %>%
  filter(Date != "2015-07-14") %>%
  filter(Date != "2015-07-16") %>%
  filter(Date != "2015-07-18") %>%
  filter(Date != "2015-07-22") %>%
  ggplot(aes(x=Taxonomy, y=-Depth))+
  geom_point(aes(fill=Percent_Abundance), colour="black", size=3, pch=21)+
  facet_grid(Location+Date+Sample~Domain+CPR, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))+
  coord_cartesian(clip = "off")+
  scale_y_continuous(
    labels = scales::number_format(accuracy =  1))+
  scale_fill_gradient(low = "white", high="black")

ggsave("~/Documents/Github/LakeTanganyika/Figures/Paper/Abundance-Plot/Abundance.bubble.portrait.reordered.pdf", height = 8.5, width = 11, units = "in")

levels(factor(abundance_df_info$Taxonomy))

abundance_df_info$Taxonomy


str(abundance_df_info)
#abundance_df_info$Taxonomy <- factor(abundance_df_info$Taxonomy, levels=order.I.want)


abundance_df_info$CPR <- factor(abundance_df_info$CPR, levels=c("Not CPR", "CPR"))
abundance_df_info$DPANN <- factor(abundance_df_info$DPANN, levels=c("DPANN", "Non-DPANN"))

## Same data but bar plot instead:
abundance.new.table <- abundance_df_info %>% 
  filter(Date != "2015-07-11") %>%
  filter(Date != "2015-07-14") %>%
  filter(Date != "2015-07-16") %>%
  filter(Date != "2015-07-18") %>%
  filter(Date != "2015-07-22")


abundance_df_info %>% 
#  filter(Date != "2015-07-11") %>%
#  filter(Date != "2015-07-14") %>%
#  filter(Date != "2015-07-16") %>%
#  filter(Date != "2015-07-18") %>%
#  filter(Date != "2015-07-22") %>%
  filter(Domain=="Archaea") %>%
  ggplot(aes(x=as.factor(-Depth), y=Percent_Abundance, fill=Taxonomy))+
  #geom_point(aes(fill=Percent_Abundance), colour="black", size=3, pch=21)+
  geom_bar(stat= "identity")+
  facet_grid(Location+Date+Sample~Domain+DPANN, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))+
  #scale_y_continuous(
  #  labels = scales::number_format(accuracy =  0.1))+
  coord_flip()+
  xlab("Depth (m)")+
  ylab("Percent Abundance (%)")

ggsave("Paper/Abundance-Plot/Archaea.bar.full.PDF", width = 11, height = 8.5, units = "in")

abundance_df_info %>% 
#  filter(Date != "2015-07-11") %>%
#  filter(Date != "2015-07-14") %>%
#  filter(Date != "2015-07-16") %>%
#  filter(Date != "2015-07-18") %>%
#  filter(Date != "2015-07-22") %>%
  filter((Domain=="Bacteria")) %>%
  ggplot(aes(x=as.factor(-Depth), y=Percent_Abundance, fill=Taxonomy))+
  #geom_point(aes(fill=Percent_Abundance), colour="black", size=3, pch=21)+
  geom_bar(stat= "identity")+
  facet_grid(Location+Date+Sample~CPR, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))+
  scale_y_continuous(
    labels = scales::number_format(accuracy =  1))+
  coord_flip()+
  xlab("Depth (m)")+
  ylab("Percent Abundance (%)")

ggsave("~/Documents/Github/LakeTanganyika/Figures/Paper/Abundance-Plot/Bact.CPR.full.PDF", width = 14, height = 8.5, units = "in")



abundance_df_info %>% 
  #  filter(Date != "2015-07-11") %>%
  #  filter(Date != "2015-07-14") %>%
  #  filter(Date != "2015-07-16") %>%
  #  filter(Date != "2015-07-18") %>%
  #  filter(Date != "2015-07-22") %>%
  #filter(CPR == "CPR") %>%
  ggplot(aes(x=as.factor(-Depth), y=Percent_Abundance, fill=Taxonomy))+
  #geom_point(aes(fill=Percent_Abundance), colour="black", size=3, pch=21)+
  geom_bar(stat= "identity")+
  facet_grid(Location+Date+Sample~Domain+CPR+DPANN, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))+
  scale_y_continuous(
    labels = scales::number_format(accuracy =  1))+
  coord_flip()+
  xlab("Depth (m)")+
  ylab("Percent Abundance (%)")

ggsave("~/Documents/Github/LakeTanganyika/Figures/Paper/Abundance-Plot/All.bacteria.archaea.bar.plot.2020.11.11.2nd.PDF", width = 14, height = 11, units = "in")


abundance_df_info %>% 
  #  filter(Date != "2015-07-11") %>%
  #  filter(Date != "2015-07-14") %>%
  #  filter(Date != "2015-07-16") %>%
  #  filter(Date != "2015-07-18") %>%
  #  filter(Date != "2015-07-22") %>%
  filter(Taxonomy == "CP WWE1") %>%
  ggplot(aes(x=as.factor(-Depth), y=Percent_Abundance, fill=Taxonomy))+
  #geom_point(aes(fill=Percent_Abundance), colour="black", size=3, pch=21)+
  geom_bar(stat= "identity")+
  facet_grid(Location+Date+Sample~., scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))+
  coord_flip()+
  xlab("Depth (m)")+
  ylab("Percent Abundance (%)")

wwe1 <- abundance.mags %>% filter(Taxonomy == "CP WWE1") %>%
  gather("Sample","PercentAbundance",KigC100:Mahsurface_9)


wwe1$Sample_f <- factor(wwe1$Sample,levels=c(sample.lookup.date$Sample)) 
wwe1 <- left_join(wwe1, sample.lookup.date)

wwe1$Date <- paste0(wwe1$Date_year, "-",wwe1$Date_month, "-",wwe1$Date_day)
wwe1$Date <- ymd(wwe1$Date)


wwe1 %>%
  ggplot(aes(x=as.factor(-Depth), y=PercentAbundance, fill=MAG))+
  #geom_point(aes(fill=Percent_Abundance), colour="black", size=3, pch=21)+
  geom_bar(stat= "identity")+
  facet_grid(Location+Date+Sample_f~., scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 0),
        legend.position = "top")+
  guides(colour=guide_legend(nrow=1))+
  #scale_y_continuous(
  #  labels = scales::number_format(accuracy =  0.1))+
  coord_flip()+
  xlab("Depth (m)")+
  ylab("Percent Abundance of WWE1(%)")



abundance_df_info %>% filter(Location == "Mahale") %>%
  filter(Date != "2015-07-21") %>%
  ggplot(aes(x=Taxonomy, y=Depth))+
  geom_point(aes(fill=Percent_Abundance), colour="black", size=3, pch=21)+
  facet_grid(Date~Domain+CPR, scales="free")+
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

ggsave("Paper/Abundance-Plot/Abundance.bubble.Mahale.reordered.pdf", height = 8.5, width = 11, units = "in")

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
