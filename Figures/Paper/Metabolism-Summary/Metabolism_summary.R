# Patricia Tran
# Analysis of metabolism table
# October 24, 2019

metabolism <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Metabolism-Summary/metabolism.tsv", header=T, sep="\t")
library(dplyr)
metabolism <- metabolism %>% filter(Completeness_checkm >= 50) %>% filter(Contamination_checkm <= 10)

lookup.metabolism <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Metabolism-Summary/lookup_metabolism.tsv", header=T, sep="\t")

#lookup.metabolism2<- aggregate(lookup.metabolism$Genes, list(lookup.metabolism$Category, lookup.metabolism$Reaction), paste, collapse=", ")



colnames(metabolism)
str(metabolism)
summary(metabolism)

library(tidyverse)

# Ok but I want a column that says Comammox if it genes amoA, nxrA and nxrB > 0
metabolism$Comammox <- ifelse(metabolism$amoA >0 & metabolism$nitrite_oxidoreductase_nxrA >0 & metabolism$nitrite_oxidoreductase_nxrB>0, 1, NA)

colnames(metabolism)
summary(metabolism)

metabolism.gathered <- metabolism %>% gather(key="Genes",value="Nb.of.genes",c(9:138))

metabolism.gathered.categories <- left_join(metabolism.gathered,lookup.metabolism, by="Genes")

head(metabolism.gathered.categories)
summary(metabolism.gathered.categories)

#nitrogen.summary <- metabolism.gathered.categories %>% filter(Category == "NITROGEN" & Nb.of.genes != 0) %>% group_by(Taxonomy, MAG, Genes) %>% summarize(Count=n())

library(ggplot2)
library(plyr)
library(scales)

metabolism.m <- ddply(metabolism.gathered.categories, .(Genes), transform,rescale = rescale(Nb.of.genes))
var_width = 5

metabolism.m <- mutate(metabolism.m, short_reaction_name = str_wrap(Reaction, width = var_width))

# Remove cysC and cysN
metabolism.m <- metabolism.m %>% filter(!Genes %in% c("cysC","cysN"))

order.taxo <- read.table("Paper/Cazyme/order.taxonomy.tsv", header=TRUE, sep="\t")
order.I.want <- unique(order.taxo$Taxon.order)

metabolism.m$Taxonomy_f <- factor(metabolism.m$Taxonomy, levels=order.I.want)

metabolism.m <- left_join(metabolism.m, order.taxo, by=c("Taxonomy_f"="Taxon.order"))


str(metabolism.m)
metabolism.m$Taxonomy_f <- factor(metabolism.m$Taxonomy_f, levels=order.I.want)

str(metabolism.m)

metabolism.m.na <- metabolism.m %>% filter(is.na(metabolism.m))


# Select just MAGS > 50% complete:
## Apply the filter you want here! :) 
## Can also choose contam level but caveats

# MIMAGS standards:
metabolism.m <- metabolism.m %>% filter(Completeness_checkm >= 50 & Contamination_checkm <10)
#Open a PDF file to save plots we will make:

#pdf("~/Documents/Github/LakeTanganyika/FigureS7-Metabolism-Summary/Metabolism-plots-by-category-LT-2020-01-20-Mimags.pdf", width = 16 , height = 10, title = "Lake Tanganyika MAGs and Metabolism")

metabolism.m[metabolism.m == "NA"] <- NA

# NITROGEN

nitrogen.taxa <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("NITROGEN","UREASE")) %>% summarise(NG = n_distinct(Taxonomy_f))
nitrogen.mags <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("NITROGEN","UREASE")) %>% summarise(NG = n_distinct(MAG))

ggplot(metabolism.m %>% filter(!is.na(rescale) & Category %in% c("NITROGEN","UREASE")), 
       aes(x=Genes,y=Taxonomy_f))+
  geom_tile(aes(fill = Nb.of.genes),colour = "white") + 
  #scale_fill_gradient(low = "light blue", high="steelblue")+
  ggtitle("Nitrogen", subtitle=paste0(nitrogen.taxa," taxonomic groups and ", nitrogen.mags, " distinct MAGs"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(Domain.y+CPR ~ short_reaction_name, scales="free",space = "free_y")

ggsave("Paper/Metabolism-Summary/nitrogen.mags.pdf", width = 8.5, height = 11, units = "in")


# SULFUR:
s.taxa <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("SULFUR","DSR","Thiosulfate oxidation")) %>% summarise(NG = n_distinct(Taxonomy_f))
s.mags <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("SULFUR","DSR","Thiosulfate oxidation")) %>% summarise(NG = n_distinct(MAG))

s.table <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("SULFUR","DSR","Thiosulfate oxidation"))
# convert the reaction to the right order:
s.table$short_reaction_name_f = factor(s.table$short_reaction_name, levels=c("sulfide\noxidation","strongly\nassociated\nwith\nsulfur\noxidation","sulfur\noxidation",
                                                                             "sulfate\nreduction","sulfite\nreduction","associated\nwith\nthe\nelectron\ntransport\nchain",
                                                                             "ancillary\ngenes","core\ngenes"))
ggplot(s.table, aes(x=Genes,y=Taxonomy_f))+
  geom_tile(aes(fill = Nb.of.genes),colour = "white") + 
  #scale_fill_gradient(low = "light blue", high="steelblue")+
  theme_bw()+
  facet_grid(Domain.y+CPR~short_reaction_name_f, scales="free",space = "free_y")+
  ggtitle("Sulfur",subtitle=paste0(s.taxa," taxonomic groups and ", s.mags, " distinct MAGs"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("Paper/Metabolism-Summary/sulfur.mags.pdf", width = 8.5, height = 11, units = "in")


# Hydrogen:
H.taxa <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HYDROGEN")) %>% summarise(NG = n_distinct(Taxonomy_f))
H.mags <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HYDROGEN")) %>% summarise(NG = n_distinct(MAG))

ggplot(metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HYDROGEN")), aes(x=Genes,y=Taxonomy_f))+
  geom_tile(aes(fill = Nb.of.genes),colour = "white") + 
  #scale_fill_gradient(low = "light blue", high="steelblue")+
  ggtitle("Hydrogen",subtitle=paste0(H.taxa," taxonomic groups and ", H.mags, " distinct MAGs"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(Domain.y+CPR~short_reaction_name, scales="free",space = "free_y")

ggsave("Paper/Metabolism-Summary/sulfur.mags.pdf", width = 8.5, height = 11, units = "in")

# Oxygen
O.taxa <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("OXYGEN")) %>% summarise(NG = n_distinct(Taxonomy_f))
O.mags <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("OXYGEN")) %>% summarise(NG = n_distinct(MAG))

ggplot(metabolism.m %>% filter(!is.na(rescale) & Category %in% c("OXYGEN")), aes(x=Genes,y=Taxonomy_f))+
  geom_tile(aes(fill =Nb.of.genes),colour = "white") + 
  #scale_fill_gradient(low = "light blue", high="steelblue")+
  ggtitle("Oxygen",subtitle=paste0(O.taxa," taxonomic groups and ", O.mags, " distinct MAGs"))+
  theme_bw()+
  facet_grid(Domain.y+CPR~short_reaction_name, scales="free",space = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("Paper/Metabolism-Summary/oxygen.mags.pdf", width = 8.5, height = 11, units = "in")

#Carbon:
C.taxa <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("METHANE","C1 METABOLISM","C MONOXIDE","CARBON FIXATION")) %>% summarise(NG = n_distinct(Taxonomy_f))
C.mags <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("METHANE","C1 METABOLISM","C MONOXIDE","CARBON FIXATION")) %>% summarise(NG = n_distinct(MAG))

ggplot(metabolism.m %>% filter(!is.na(rescale) & Category %in% c("METHANE","C1 METABOLISM","C MONOXIDE","CARBON FIXATION")), 
       aes(x=Genes,y=Taxonomy_f))+
  geom_tile(aes(fill =Nb.of.genes),colour = "white") + 
  #scale_fill_gradient(low = "light blue", high="steelblue")+
  theme_bw()+
  facet_grid(Domain.y+CPR~short_reaction_name, scales="free",space = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Carbon",subtitle=paste0(C.taxa," taxonomic groups and ", C.mags, " distinct MAGs"))

ggsave("Paper/Metabolism-Summary/carbon.mags.pdf", width = 8.5, height = 11, units = "in")

#Other metabolism:
other.taxa <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HALOGENATED COMPOUNDS","ARSENIC","SELENIUM","NITRILES","METALS")) %>% summarise(NG = n_distinct(Taxonomy_f))
other.mags <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HALOGENATED COMPOUNDS","ARSENIC","SELENIUM","NITRILES","METALS")) %>% summarise(NG = n_distinct(MAG))


ggplot(metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HALOGENATED COMPOUNDS","ARSENIC","SELENIUM","NITRILES","METALS")), 
       aes(x=Genes,y=Taxonomy_f))+
  geom_tile(aes(fill =  Nb.of.genes),colour = "white") + 
  #scale_fill_gradient(low = "light blue", high="steelblue")+
  theme_bw()+
  facet_grid(Domain.y+CPR~short_reaction_name, scales="free",space = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Other",subtitle=paste0(other.taxa," taxonomic groups and ", other.mags, " distinct MAGs"))

ggsave("Paper/Metabolism-Summary/other.mags.pdf", width = 8.5, height = 11, units = "in")

# Make a bar plot comparing taxonomic groups vs. number of genomes involved in each category
summary.stats <- as.data.frame(matrix(nrow=6, ncol=3))
colnames(summary.stats) <- c("Category","Number.of.Taxa","Number.of.MAGs")
summary.stats$Category<- c("Nitrogen","Sulfur","Hydrogen","Oxygen","Carbon","Other")
summary.stats$Number.of.Taxa <- c(nitrogen.taxa,s.taxa,H.taxa,O.taxa,C.taxa,other.taxa)
summary.stats$Number.of.MAGs <- c(nitrogen.mags,s.mags,H.mags,O.mags,C.mags,other.mags)

to.plot.summary <- gather(data= summary.stats, key="Category_attribute",value="ToPlot",c(2:3))
to.plot.summary$ToPlot <- as.numeric(to.plot.summary$ToPlot)

bar.plot <- ggplot(to.plot.summary, aes(x=factor(Category),y=ToPlot,fill=factor(Category_attribute))) + 
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_discrete(name = "Shown", labels = c("Number of MAGs", "Number of Taxa"))+
  theme_minimal()+
  theme(text = element_text(size=20))+
  xlab("Biogeochemical category")+
  ylab("Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar.plot


# plot the metabolism of just the two new CP:

ggplot(metabolism.m %>% filter(!is.na(rescale) & 
                                 Taxonomy %in% c("CP Tanganyikabacteria", "CP Ziwabacteria")), 
       aes(x=Genes,y=MAG))+
  geom_tile(aes(fill = Nb.of.genes),colour = "white") + 
  #scale_fill_gradient(low = "light blue", high="steelblue")+
  theme_bw()+
  facet_grid(Taxonomy ~ Category,scales = "free", space="free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Candidate Phyla")

ggplot(metabolism.m %>% filter(!is.na(rescale) & 
                                 Taxonomy %in% c("CP Aenigmarchaeota","Diapherotrites","CP Parvarchaeota","Woesearchaeota","CP Pacearchaeota")), 
       aes(x=Genes,y=MAG))+
  geom_tile(aes(fill = Nb.of.genes),colour = "white") + 
  #scale_fill_gradient(low = "light blue", high="steelblue")+
  theme_bw()+
  facet_grid(Taxonomy ~ Category+Reaction,scales = "free", space="free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.x.top = element_text(angle = 90),
        strip.text.y.right = element_text(angle = 0))+
  ggtitle("DPANN in Lake Tanganyika")

## Now Let's try a cool thing -- extract the abundance information for all the MAGs in these category and plot their abundance through depths:
# Let's try with Nitrogen first

list.N_cycling.MAGS <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("NITROGEN","UREASE")) %>% distinct(MAG)
list.S <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("SULFUR","DSR","Thiosulfate oxidation")) %>% distinct(MAG)
list.H <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HYDROGEN")) %>% distinct(MAG)
list.O <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("OXYGEN")) %>% distinct(MAG)
list.C <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("METHANE","C1 METABOLISM","C MONOXIDE","CARBON FIXATION")) %>% distinct(MAG)
list.other <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HALOGENATED COMPOUNDS","ARSENIC","SELENIUM","NITRILES","METALS")) %>% distinct(MAG)

pdf("~/Documents/Github/LakeTanganyika/FigureS7-Metabolism-Summary/Distribution-of-organisms-2020-01-20-mimags.pdf", width = 16 , height = 10, title = "Lake Tanganyika MAGs and Metabolism")


# Load abundance table:
abund <- read.table("~/Documents/Github/LakeTanganyika/Figures/Paper/Abundance-Plot/Abundance-MAGS.tsv", sep="\t", header=T)
lookup.samples <- read.table("~/Documents/Github/LakeTanganyika/Figures/Other/Cyano-vs-SQR-genomes/sample_look_up.tsv", sep="\t", header=T)


abund.N <- abund %>% filter(Taxonomy %in% c("Aenigmarchaeota","Diapherotrites","Parvarchaeota","Woesearchaeota","Pacearchaeota"))
colnames(abund.N)
abund.N <- abund.N %>% gather("Sample","Coverage",c(4:27))

abund.N <- left_join(abund.N,lookup.samples,by="Sample")

sum(abund.N$Coverage)

ggplot(abund.N, aes(Coverage,-Depth, color=Taxonomy, shape=Taxonomy))+geom_point()+
  theme_bw()+
  geom_hline(yintercept = -100)

plot(x=abund.N$Coverage, y=-(abund.N$Depth), main="Nitrogen", xlab="Coverage", ylab="Depth", )
abline(h=-100,col="grey",lty=2)
abline(h=-50,col="grey",lty=2)
abline(h=-70,col="red",lty=2)

##  S
abund.S <- abund %>% filter(MAG %in% list.S$MAG)
colnames(abund.S)
abund.S <- abund.S %>% gather("Sample","Coverage",c(4:27))

abund.S <- left_join(abund.S,lookup.samples,by="Sample")

plot(x=abund.S$Coverage, y=-(abund.S$Depth), main="Sulfur", xlab="Coverage", ylab="Depth", xlim=c(0,400))
abline(h=-100,col="grey",lty=2)
abline(h=-50,col="grey",lty=2)
abline(h=-70,col="red",lty=2)

# H
abund.H <- abund %>% filter(MAG %in% list.H$MAG)
colnames(abund.H)
abund.H <- abund.H %>% gather("Sample","Coverage",c(4:27))

abund.H <- left_join(abund.H,lookup.samples,by="Sample")
plot(x=abund.H$Coverage, y=-(abund.H$Depth), main="Hydrogen", xlab="Coverage", ylab="Depth", xlim=c(0,400))
abline(h=-100,col="grey",lty=2)
abline(h=-50,col="grey",lty=2)
abline(h=-70,col="red",lty=2)

# O
abund.O <- abund %>% filter(MAG %in% list.O$MAG)
colnames(abund.O)
abund.O <- abund.O %>% gather("Sample","Coverage",c(4:27))

abund.O <- left_join(abund.O,lookup.samples,by="Sample")

plot(x=abund.O$Coverage, y=-(abund.O$Depth), main="Oxygen", xlab="Coverage", ylab="Depth", xlim=c(0,400))
abline(h=-100,col="grey",lty=2)
abline(h=-50,col="grey",lty=2)
abline(h=-70,col="red",lty=2)

# C
abund.C <- abund %>% filter(MAG %in% list.C$MAG)
colnames(abund.C)
abund.C <- abund.C %>% gather("Sample","Coverage",c(4:27))

abund.C <- left_join(abund.C,lookup.samples,by="Sample")
plot(x=abund.C$Coverage, y=-(abund.C$Depth), main="Carbon", xlab="Coverage", ylab="Depth", xlim=c(0,400))
abline(h=-100,col="grey",lty=2)
abline(h=-50,col="grey",lty=2)
abline(h=-70,col="red",lty=2)

# Other
abund.other <- abund %>% filter(MAG %in% list.other$MAG)
colnames(abund.other)
abund.other <- abund.other %>% gather("Sample","Coverage",c(4:27))

abund.other <- left_join(abund.other,lookup.samples,by="Sample")
plot(x=abund.other$Coverage, y=-(abund.other$Depth),main="Other", xlab="Coverage", ylab="Depth",xlim=c(0,400))
abline(h=-100,col="grey",lty=2)
abline(h=-50,col="grey",lty=2)
abline(h=-70,col="red",lty=2)

dev.off()

## now plot the abundance by Biogeochem cycle:
pdf("~/Documents/Github/LakeTanganyika/FigureS7-Metabolism-Summary/Distribution-organisms-by-reactions-2020-01-20-mimags.pdf", width = 16 , height = 10, title = "Lake Tanganyika MAGs and Metabolism")

## ggplot version coloured by reaction:
abund.N.rxn <- left_join(abund.N, metabolism.m, by="MAG")

abund.N.plot <- ggplot(abund.N.rxn %>% filter(!is.na(Nb.of.genes) & Category %in% c("NITROGEN","UREASE")), aes(x=Coverage, y=Depth, col=Reaction))+
  geom_point(aes(col=short_reaction_name,alpha=.5))+
  geom_line(y=-50, col="black")+
  geom_line(y=-100, col="black")+
  geom_line(y=-70, col="red")+
  facet_grid(~short_reaction_name)+
  scale_y_reverse(lim=c(1250,0))+
  theme_bw()+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Distribution of MAGs involved in nitrogen cycling reactions")
abund.N.plot

abund.S.rxn <- left_join(abund.S, metabolism.m, by="MAG")
abund.S.plot <- ggplot(abund.S.rxn %>% filter(!is.na(Nb.of.genes) & Category %in% c("SULFUR","DSR","Thiosulfate oxidation")), aes(x=Coverage, y=Depth, col=Reaction))+
  geom_point(aes(col=short_reaction_name,alpha=.5))+
  geom_line(y=-50, col="black")+
  geom_line(y=-100, col="black")+
  geom_line(y=-70, col="red")+
  facet_grid(~short_reaction_name)+
  scale_y_reverse(lim=c(1250,0))+
  theme_bw()+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Distribution of MAGs involved in sulfur cycling reactions")
abund.S.plot

abund.C.rxn <- left_join(abund.C, metabolism.m, by="MAG")

C.rxn <- abund.C.rxn %>% filter(!is.na(Nb.of.genes) & Category %in% c("METHANE","C1 METABOLISM","C MONOXIDE","CARBON FIXATION"))
#combine the soluble and partculate methane oxidation into one category:
C.rxn
                       

Abund.C.plot <- ggplot(abund.C.rxn %>% filter(!is.na(Nb.of.genes) & Category %in% c("METHANE","C1 METABOLISM","C MONOXIDE","CARBON FIXATION")), aes(x=Coverage, y=Depth, col=Reaction))+
  geom_point(aes(col=short_reaction_name,alpha=.5))+
  geom_line(y=-50, col="black")+
  geom_line(y=-100, col="black")+
  geom_line(y=-70, col="red")+
  facet_grid(~short_reaction_name)+
  scale_y_reverse(lim=c(1250,0))+
  theme_bw()+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Distribution of MAGs involved in carbon cycling reactions")
Abund.C.plot

library(ggpubr)
library(patchwork)

Abund.C.plot + abund.N.plot + abund.S.plot

ggarrange(Abund.C.plot, abund.N.plot, abund.S.plot, ncol=2, nrow=2, labels=c("A","B","C"))


dev.off()


# List of Actinobacteria Thermoleophilia for Liz:
library(dplyr)
list.thermo <- metabolism.m %>% filter(MAG %in% c("M_DeepCast_400m_m1_127","K_Offshore_40m_m2_012","M_DeepCast_65m_m2_256","K_Offshore_80m_m2_094",
                                                 "K_Offshore_surface_m2_014","K_DeepCast_65m_m2_213","K_DeepCast_150m_m2_272"))

abund.thermo <- abund %>% filter(MAG %in% list.thermo$MAG)
colnames(abund.thermo)
library(tidyverse)
abund.thermo <- abund.thermo %>% gather("Sample","Coverage",c(4:27))

abund.thermo <- left_join(abund.thermo,lookup.samples,by="Sample")

plot(x=abund.thermo$Coverage, y=-(abund.thermo$Depth),main="Other", xlab="Coverage", ylab="Depth", col=factor(abund.thermo$MAG))
abline(h=-100,col="grey",lty=2)
abline(h=-50,col="grey",lty=2)
abline(h=-70,col="red",lty=2)
library(ggplot2)

ggplot(abund.thermo, aes(x=Coverage,y=-Depth, col=MAG, shape=Location))+geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Actinobacteria Thermoleophilia from Lake Tanganyika")+
  geom_line(y=-100, col="black")+
  geom_line(y=-50, col="black")+
  geom_line(y=-70, col="red")



## List Nitrogen Fixers:
list.Nfix$MAG <-  c("K_DeepCast_100m_mx_141","M_DeepCast_65m_m2_041",
                "M_DeepCast_65m_m2_268",
                "K_DeepCast_1200m_m2_283",
                "K_DeepCast_100m_m2_241")
abund.nfix <- abund %>% filter(MAG %in% list.Nfix$MAG)
colnames(abund.nfix)
abund.nfix <- abund.nfix %>% gather("Sample","Coverage",c(4:27))

abund.nfix$Name_Tqxonomy <- paste0(abund.nfix$MAG,"(",abund.nfix$Taxonomy,")",sep=" ")

abund.nfix <- left_join(abund.nfix,lookup.samples,by="Sample")
ggplot(abund.nfix, aes(x=Coverage,y=-Depth, col=Name_Tqxonomy, shape=Location))+geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Nitrogen fixers from Lake Tanganyika", subtitle = "nifA_mo, nifA_mo and nifH present")+
  geom_line(y=-100, col="black")+
  geom_line(y=-50, col="black")+
  geom_line(y=-70, col="red")

##Plotting metabolisms of CP only
subset.metabolism.m<- metabolism.m[grep("CP ", metabolism.m$Taxonomy), ]

ggplot(subset.metabolism.m %>% filter(!is.na(rescale)),
       aes(x=Reaction,y=MAG))+
  geom_tile(aes(fill = Nb.of.genes),colour = "white") + 
  #scale_fill_gradient(low = "light blue", high="steelblue")+
  theme_bw()+
  facet_grid(Taxonomy ~ Category,scales = "free", space="free_y", switch = "both")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.y = element_text(angle = 180))+
  ggtitle("Candidate Phyla")


