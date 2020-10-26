dbcan <- read.delim("~/Documents/Github/LakeTanganyika/Figures/Paper/Cazyme/dbcan-2019-10-11.tsv")

dbcan$Taxonomy <- as.character(dbcan$Taxonomy)
dbcan$Cazyme.density..nb.of.CAZYME.bp.<- as.numeric(dbcan$Cazyme.density..nb.of.CAZYME.bp.)
str(dbcan)

metabolism <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Metabolism-Summary/metabolism.tsv", header=T, sep="\t")
library(dplyr)
metabolism <- metabolism %>% filter(Completeness_checkm >= 50) %>% filter(Contamination_checkm <= 10)

list.mag <- as.character(metabolism$MAG)

dbcan <- dbcan %>% filter(MAG_ID %in% list.mag)
# boxplot(dbcan$Cazyme.density..nb.of.CAZYME.bp.~ dbcan$Taxonomy,
#   xlab="Taxonomic group",
#   ylab="CAZYME density per genome size (Hits/bp)")

#dbcan <- dbcan[-460,]

library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)

dbcan <- dbcan[,c(1:4,7:14)]

#install.packages("reshape")
library(reshape)
m.dbcan <- melt(dbcan)

head(m.dbcan)
str(m.dbcan)

m.dbcan$Taxonomy_f <- as.factor(m.dbcan$Taxonomy)

subset.mdbcan <- subset(m.dbcan, m.dbcan$variable == "Cazyme.density..nb.of.CAZYME.bp.")

library(dplyr)


nb.of.MAG.each.taxa <- subset.mdbcan %>% group_by(Taxonomy) %>% tally()
str(nb.of.MAG.each.taxa)

nb.of.MAG.each.taxa$newMAGname <- paste(nb.of.MAG.each.taxa$Taxonomy," (",nb.of.MAG.each.taxa$n,")",sep="")

order.wanted <- reorder(subset.mdbcan$Taxonomy, subset.mdbcan$value, FUN= sum)

head(order.wanted)

#View(order.wanted)

str(m.dbcan[which(m.dbcan$value>0),])

# Only showing dbcan values greater than one
# Append a column of taxonomy that match value between m.dbcan and the nb.of.MAG.each.taxa$newMAGname
new.m.dbcan <- m.dbcan %>% left_join(nb.of.MAG.each.taxa)

bp <- ggplot(new.m.dbcan[which(new.m.dbcan$value>0),], aes(x=newMAGname, y=value)) + 
  geom_boxplot(lwd=0.4)+
  facet_grid(Domain ~ variable,scales="free",space = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+
  ggtitle("CAZyme distribution in 523 MAGs in Lake Tanganyika",subtitle = "April 3, 2020")+
  xlab("Taxonomy")+
  ylab("Number of Hits")

bp

order.taxo <- read.table("Paper/Cazyme/order.taxonomy.tsv", header=TRUE, sep="\t")
order.I.want <- order.taxo$Taxonomy

new.m.dbcan$Taxonomy_f2 <- factor(new.m.dbcan$Taxonomy_f, levels=order.I.want)

new.m.dbcan <- left_join(new.m.dbcan, order.taxo, by=c("Taxonomy_f2"="Taxonomy"))

new.m.dbcan$Taxonomy_f

str(new.m.dbcan)

levels(new.m.dbcan$Taxonomy_f2)

# Replot:
ggplot(new.m.dbcan[which(new.m.dbcan$value>0),], aes(x=reorder(Taxonomy_f2, desc(Taxonomy_f2)), y=value)) + 
  geom_boxplot(lwd=0.4)+
  facet_grid(Domain.y+CPR ~ variable,scales="free",space = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+
  ggtitle("CAZyme distribution in 523 MAGs in Lake Tanganyika",subtitle = "October 26, 2020")+
  xlab("Taxonomy")+
  ylab("Number of Hits")

ggsave("cazyme.2020.10.26.pdf", height = 11, width = 8.5, units = "in")

# 
# ## Read data:
# cazyme <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Cazyme/cazyme.csv")
# # Just take the GH:
# colnames(cazyme)
# cazyme.GH <- cazyme %>% select(MAG_ID, GH1:GH99)
# #rownames(cazyme.GH) <- cazyme.GH$MAG_ID
# #cazyme.GH <- cazyme.GH[,-1]
# unique(cazyme.GH$MAG_ID)
# 
# # Add taxonomic info and summarize by GH
# library(d3heatmap)
# library(tidyverse)
# cazyme.GH <- cazyme.GH %>% gather("GH","Value",GH1:GH99)
# cazyme.GH <- cazyme.GH %>% filter(!is.na(Value))
# 
# 
# ggplot(cazyme.GH, aes(x=MAG_ID, y=GH, fill=Value)) +
#   geom_tile()+
#   theme_bw()
# 
# # Taxonomy
# taxo <- m.dbcan[,1:3]
# cazyme.GH <- left_join(cazyme.GH, taxo)
# 
# summary <- cazyme.GH %>% group_by(SimplifiedTaxonomy, GH, Value) %>%
#   summarise()
# 
# summary <- summary %>% arrange(-Value)
# 
# ggplot(summary, aes(x=SimplifiedTaxonomy, y=GH, fill=Value)) +
#   geom_tile()+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 
# taxo2 <- read.delim("~/Documents/Github/LakeTanganyika/Figures/Paper/Abundance-Plot/Simple_taxo_lookup.tsv")
# 
# summary2 <- left_join(summary, taxo2, by=c("SimplifiedTaxonomy"="Taxonomy"))
# summary2$Simplified.Taxonomy <- as.character(summary2$Simplified.Taxonomy)
# 
# summary2$Simplified.Taxonomy2 <- ifelse(is.na(summary2$Simplified.Taxonomy), yes = summary2$SimplifiedTaxonomy, no=summary2$Simplified.Taxonomy)
# 
# #Remove the underscore after the subgroup:
# summary2$GH <- str_replace(summary2$GH, "_.*","")
# 
# summary2 %>% group_by(Simplified.Taxonomy2, GH) %>%
#   summarise(sum=sum(Value)) %>%
#   ggplot(aes(x=Simplified.Taxonomy2, y=GH, fill=sum))+
#   geom_tile()+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         text = element_text(size=8),
#         panel.grid.major= element_line("light grey"),
#         panel.grid.minor= element_line("black"))
# 
# # The plot looks ok but there are way too many rows.


