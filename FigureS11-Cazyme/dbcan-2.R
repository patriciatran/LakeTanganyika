dbcan <- read.delim("~/Documents/Github/LakeTanganyika/FigureS11-Cazyme/dbcan-2019-10-11.tsv")

dbcan$Taxonomy <- as.character(dbcan$Taxonomy)
dbcan$Cazyme.density..nb.of.CAZYME.bp.<- as.numeric(dbcan$Cazyme.density..nb.of.CAZYME.bp.)
str(dbcan)

metabolism <- read.csv("~/Documents/Github/LakeTanganyika/FigureS7-Metabolism-Summary/metabolism.tsv", header=T, sep="\t")
library(dplyr)
metabolism <- metabolism %>% filter(Completeness_checkm >= 70) %>% filter(Contamination_checkm <= 10)

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
  theme_base()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+
  ggtitle("CAZyme distribution in 431 MAGs in Lake Tanganyika",subtitle = "January 20, 2020")+
  ylab("Taxonomy")+
  xlab("Number of Hits")

bp
