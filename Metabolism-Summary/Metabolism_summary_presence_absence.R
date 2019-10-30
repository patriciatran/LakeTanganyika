# Patricia Tran
# Analysis of metabolism table
# October 24, 2019

metabolism <- read.csv("~/Box/PhD/Research/Lake-Tanganyika/1_Code/Metabolism-Summary/metabolism.tsv", header=T, sep="\t")

lookup.metabolism <- read.csv("~/Box/PhD/Research/Lake-Tanganyika/1_Code/Metabolism-Summary/lookup_metabolism.tsv", header=T, sep="\t")

library(dplyr)

colnames(metabolism)
str(metabolism)
summary(metabolism)

library(tidyverse)
metabolism.gathered <- metabolism %>% gather(key="Genes",value="Nb.of.genes",c(9:137))

metabolism.gathered.categories <- left_join(metabolism.gathered,lookup.metabolism, by="Genes")

head(metabolism.gathered.categories)
summary(metabolism.gathered.categories)

library(ggplot2)
library(plyr)
library(scales)

metabolism.m <- ddply(metabolism.gathered.categories, .(Genes), transform,rescale = rescale(Nb.of.genes))
var_width = 5

metabolism.m <- mutate(metabolism.m, short_reaction_name = str_wrap(Reaction, width = var_width))

# If they have the gene at least once write: 1 in the column called presence
metabolism.m <- mutate(metabolism.m, presence = as.integer(Nb.of.genes != 0))

detach(package:dplyr)
# Now we need to sum up the # of gene (presence absence) to per reaction per mag:
library(dplyr)
metabolism.m.sumpresence <- metabolism.m %>% group_by(Reaction, short_reaction_name, MAG) %>% summarize(sumGenes = sum(presence))
# Now get the sum of genes that each reactions should have:
nb.of.genes.table <- lookup.metabolism %>% group_by(Category, Reaction) %>% summarise(Nb.Distinct.Genes = n_distinct(Genes))

metabolism.m.sumpresence <- left_join(metabolism.m.sumpresence, nb.of.genes.table, by=c("Reaction"))

no.categories <- metabolism.m.sumpresence %>% filter(is.na(Category))
nrow(no.categories)
# There are exactly the number of MAGS -- weird
# Remove rows with NA:
metabolism.m.sumpresence <- metabolism.m.sumpresence %>% filter(!is.na(Category))

metabolism.m.sumpresence$PercentGenes <- metabolism.m.sumpresence$sumGenes/metabolism.m.sumpresence$Nb.Distinct.Genes
summary(metabolism.m.sumpresence)

# There shouldn't be value above 1!!
rows.above.1 <- metabolism.m.sumpresence %>% filter(PercentGenes > 1)

metabolism.m.sumpresence <- metabolism.m.sumpresence %>% filter(PercentGenes <= 1)

# Now append the Taxonomic information
taxonomy <- metabolism %>% select(MAG, Taxonomy, Domain)
metabolism.m.sumpresence <- left_join(metabolism.m.sumpresence, taxonomy, by="MAG")


#Open a PDF file to save plots we will make:
pdf("~/Box/PhD/Research/Lake-Tanganyika/1_Code/Metabolism-Summary/Metabolism-plots-by-category-LT2.pdf", width = 16 , height = 10, title = "Lake Tanganyika MAGs and Metabolism")
#dev.off()
# NITROGEN

nitrogen.taxa <- metabolism.m.sumpresence %>% filter(!is.na(PercentGenes) & Category %in% c("NITROGEN","UREA")) %>% ungroup(Reaction) %>% summarise(n_distinct(Taxonomy))
nitrogen.mags <- metabolism.m.sumpresence %>% filter(!is.na(PercentGenes) & Category %in% c("NITROGEN","UREA")) %>% ungroup(Reaction) %>% summarise(n_distinct(MAG))

ggplot(metabolism.m.sumpresence %>% filter(Category %in% c("NITROGEN","UREASE")), aes(x=short_reaction_name,y=Taxonomy))+
  geom_tile(aes(fill = PercentGenes),colour = "white") + 
  scale_fill_gradient(low = "light blue", high="steelblue")+
  ggtitle("Nitrogen", subtitle=paste0(nitrogen.taxa," taxonomic groups and ", nitrogen.mags, " distinct MAGs"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(Domain~., scales="free",space = "free_y")

# SULFUR:
s.taxa <- metabolism.m.sumpresence %>% filter(!is.na(PercentGenes) & Category %in% c("SULFUR","DSR","Thiosulfate oxidation")) %>% ungroup(Reaction) %>% summarise(NG = n_distinct(Taxonomy))
s.mags <- metabolism.m.sumpresence %>% filter(!is.na(PercentGenes) & Category %in% c("SULFUR","DSR","Thiosulfate oxidation")) %>% ungroup(Reaction) %>% summarise(NG = n_distinct(MAG))

ggplot(metabolism.m.sumpresence %>% filter(!is.na(PercentGenes) & Category %in% c("SULFUR","DSR","Thiosulfate oxidation")), aes(x=short_reaction_name,y=Taxonomy))+
  geom_tile(aes(fill = PercentGenes),colour = "white") + 
  scale_fill_gradient(low = "light blue", high="steelblue")+
  theme_bw()+
  facet_grid(Domain~short_reaction_name, scales="free",space = "free_y")+
  ggtitle("Sulfur",subtitle=paste0(s.taxa," taxonomic groups and ", s.mags, " distinct MAGs"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Hydrogen:
H.taxa <- metabolism.m.sumpresence %>% filter(Category %in% c("HYDROGEN")) %>% ungroup(Reaction) %>% summarise(NG = n_distinct(Taxonomy))
H.mags <- metabolism.m.sumpresence %>% filter(!is.na(PercentGenes) & Category %in% c("HYDROGEN")) %>% ungroup(Reaction) %>% summarise(NG = n_distinct(MAG))

ggplot(metabolism.m %>% filter(!is.na(PercentGenes) & Category %in% c("HYDROGEN")), aes(x=short_reaction_name,y=Taxonomy))+
  geom_tile(aes(fill = Nb.of.genes),colour = "white") + 
  scale_fill_gradient(low = "light blue", high="steelblue")+
  ggtitle("Hydrogen",subtitle=paste0(H.taxa," taxonomic groups and ", H.mags, " distinct MAGs"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(Domain~short_reaction_name, scales="free",space = "free_y")

# Oxygen
O.taxa <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("OXYGEN")) %>% summarise(NG = n_distinct(Taxonomy))
O.mags <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("OXYGEN")) %>% summarise(NG = n_distinct(MAG))

ggplot(metabolism.m %>% filter(!is.na(rescale) & Category %in% c("OXYGEN")), aes(x=Genes,y=Taxonomy))+
  geom_tile(aes(fill =Nb.of.genes),colour = "white") + 
  scale_fill_gradient(low = "light blue", high="steelblue")+
  ggtitle("Oxygen",subtitle=paste0(O.taxa," taxonomic groups and ", O.mags, " distinct MAGs"))+
  theme_bw()+
  facet_grid(Domain~short_reaction_name, scales="free",space = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Carbon:
C.taxa <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("METHANE","C1 METABOLISM","C MONOXIDE","CARBON FIXATION")) %>% summarise(NG = n_distinct(Taxonomy))
C.mags <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("METHANE","C1 METABOLISM","C MONOXIDE","CARBON FIXATION")) %>% summarise(NG = n_distinct(MAG))

ggplot(metabolism.m %>% filter(!is.na(rescale) & Category %in% c("METHANE","C1 METABOLISM","C MONOXIDE","CARBON FIXATION")), aes(x=Genes,y=Taxonomy))+
  geom_tile(aes(fill =Nb.of.genes),colour = "white") + 
  scale_fill_gradient(low = "light blue", high="steelblue")+
  theme_bw()+
  facet_grid(Domain~short_reaction_name, scales="free",space = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Carbon",subtitle=paste0(C.taxa," taxonomic groups and ", C.mags, " distinct MAGs"))

#Other metabolism:
other.taxa <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HALOGENATED COMPOUNDS","ARSENIC","SELENIUM","NITRILES","METALS")) %>% summarise(NG = n_distinct(Taxonomy))
other.mags <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HALOGENATED COMPOUNDS","ARSENIC","SELENIUM","NITRILES","METALS")) %>% summarise(NG = n_distinct(MAG))


ggplot(metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HALOGENATED COMPOUNDS","ARSENIC","SELENIUM","NITRILES","METALS")), aes(x=Genes,y=Taxonomy))+
  geom_tile(aes(fill =  Nb.of.genes),colour = "white") + 
  scale_fill_gradient(low = "light blue", high="steelblue")+
  theme_bw()+
  facet_grid(Domain~short_reaction_name, scales="free",space = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Other",subtitle=paste0(other.taxa," taxonomic groups and ", other.mags, " distinct MAGs"))

dev.off()

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
  ylab("Count")

savePlot(bar.plot, "~/Box/PhD/Research/Lake-Tanganyika/1_Code/Metabolism-Summary/barplot.pdf", type = "png")

# plot the metabolism of just the two new CP:

ggplot(metabolism.m %>% filter(!is.na(rescale) & 
                                 Taxonomy %in% c("CP Tanganyikabacteria", "CP Ziwabacteria")), 
       aes(x=Genes,y=MAG))+
  geom_tile(aes(fill = Nb.of.genes),colour = "white") + 
  scale_fill_gradient(low = "light blue", high="steelblue")+
  theme_bw()+
  facet_grid(Taxonomy ~ Category,scales = "free", space="free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Candidate Phyla")

## Now Let's try a cool thing -- extract the abundance information for all the MAGs in these category and plot their abundance through depths:
# Let's try with Nitrogen first

list.N_cycling.MAGS <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("NITROGEN","UREASE")) %>% summarise(MAG = unique(MAG))
list.S <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("SULFUR","DSR","Thiosulfate oxidation")) %>% summarise(MAG= unique(MAG))
list.H <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HYDROGEN")) %>% summarise(MAG= unique(MAG))
list.O <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("OXYGEN")) %>% summarise(MAG = unique(MAG))
list.C <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("METHANE","C1 METABOLISM","C MONOXIDE","CARBON FIXATION")) %>% summarise(MAG = unique(MAG))
list.other <- metabolism.m %>% filter(!is.na(rescale) & Category %in% c("HALOGENATED COMPOUNDS","ARSENIC","SELENIUM","NITRILES","METALS")) %>% summarise(MAG = unique(MAG))

pdf("~/Box/PhD/Research/Lake-Tanganyika/1_Code/Metabolism-Summary/Distribution-of-organisms.pdf", width = 16 , height = 10, title = "Lake Tanganyika MAGs and Metabolism")
par(mfrow=c(2,3))

# Load abundance table:
abund <- read.table("~/Box/PhD/Research/Lake-Tanganyika/1_Code/Figure3-RankAbundanceCurve/Abundance-MAGS.tsv", sep="\t", header=T)
lookup.samples <- read.table("~/Box/PhD/Research/Lake-Tanganyika/1_Code/Cyano-vs-SQR-genomes/sample_look_up.tsv", sep="\t", header=T)

abund.N <- abund %>% filter(MAG %in% list.N_cycling.MAGS$MAG)
colnames(abund.N)
abund.N <- abund.N %>% gather("Sample","Coverage",c(4:27))

abund.N <- left_join(abund.N,lookup.samples,by="Sample")

plot(x=abund.N$Coverage, y=-(abund.N$Depth), main="Nitrogen", xlab="Coverage", ylab="Depth",xlim=c(0,400))

##  S
abund.S <- abund %>% filter(MAG %in% list.S$MAG)
colnames(abund.S)
abund.S <- abund.S %>% gather("Sample","Coverage",c(4:27))

abund.S <- left_join(abund.S,lookup.samples,by="Sample")

plot(x=abund.S$Coverage, y=-(abund.S$Depth), main="Sulfur", xlab="Coverage", ylab="Depth", xlim=c(0,400))

# H
abund.H <- abund %>% filter(MAG %in% list.H$MAG)
colnames(abund.H)
abund.H <- abund.H %>% gather("Sample","Coverage",c(4:27))

abund.H <- left_join(abund.H,lookup.samples,by="Sample")
plot(x=abund.H$Coverage, y=-(abund.H$Depth), main="Hydrogen", xlab="Coverage", ylab="Depth", xlim=c(0,400))

# O
abund.O <- abund %>% filter(MAG %in% list.O$MAG)
colnames(abund.O)
abund.O <- abund.O %>% gather("Sample","Coverage",c(4:27))

abund.O <- left_join(abund.O,lookup.samples,by="Sample")
plot(x=abund.O$Coverage, y=-(abund.O$Depth), main="Oxygen", xlab="Coverage", ylab="Depth", xlim=c(0,400))

# C
abund.C <- abund %>% filter(MAG %in% list.C$MAG)
colnames(abund.C)
abund.C <- abund.C %>% gather("Sample","Coverage",c(4:27))

abund.C <- left_join(abund.C,lookup.samples,by="Sample")
plot(x=abund.C$Coverage, y=-(abund.C$Depth), main="Carbon", xlab="Coverage", ylab="Depth", xlim=c(0,400))

# Other
abund.other <- abund %>% filter(MAG %in% list.other$MAG)
colnames(abund.other)
abund.other <- abund.other %>% gather("Sample","Coverage",c(4:27))

abund.other <- left_join(abund.other,lookup.samples,by="Sample")
plot(x=abund.other$Coverage, y=-(abund.other$Depth),main="Other", xlab="Coverage", ylab="Depth",xlim=c(0,400))

dev.off()

