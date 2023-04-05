# Load packages:
library(tidyverse)
library(dplyr)
library(reshape)


# Load data
dbcan <- read.delim("~/Documents/Github/LakeTanganyika/Figures/Paper/Cazyme/dbcan-2019-10-11.tsv")

# Fix spelling mistakes
dbcan$Taxonomy <- as.character(dbcan$Taxonomy)
dbcan$Taxonomy <- str_replace(dbcan$Taxonomy, "CP Verstraetaerchaeota", "CP Verstratearchaeota")
dbcan$Taxonomy <- str_replace(dbcan$Taxonomy, "Kirimatiellacea", "Kirimatiellaeota")
dbcan$Taxonomy <- str_replace(dbcan$Taxonomy, "CP Aminicemantes (OP8)", "CP Aminicenantes (CP8)")

# Convert to a numeric table
dbcan$Cazyme.density..nb.of.CAZYME.bp.<- as.numeric(dbcan$Cazyme.density..nb.of.CAZYME.bp.)

str(dbcan)

# Load metabolism table:
metabolism <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Metabolism-Summary/metabolism.tsv", header=T, sep="\t")

# Only select MAGs that are MIMAG (50% complete and less than 10% contam)
metabolism <- metabolism %>% filter(Completeness_checkm >= 50) %>% filter(Contamination_checkm <= 10)

# Get a list of their names
list.mag <- as.character(metabolism$MAG)

# Subset the dbcan table using those mag names
dbcan <- dbcan %>% filter(MAG_ID %in% list.mag)

dbcan <- dbcan[,c(1:4,7:14)]


m.dbcan <- melt(dbcan)
#m.dbcan <- dbcan %>% gather("variable","value",Cazyme.density..nb.of.CAZYME.bp.:PL)

head(m.dbcan)
str(m.dbcan)

m.dbcan$Taxonomy_f <- as.factor(m.dbcan$Taxonomy)

subset.mdbcan <- subset(m.dbcan, m.dbcan$variable == "Cazyme.density..nb.of.CAZYME.bp.")

library(dplyr)


nb.of.MAG.each.taxa <- subset.mdbcan %>% group_by(Taxonomy) %>% tally()
str(nb.of.MAG.each.taxa)

nb.of.MAG.each.taxa$newMAGname <- paste(nb.of.MAG.each.taxa$Taxonomy," (",nb.of.MAG.each.taxa$n,")",sep="")

#str(m.dbcan[which(m.dbcan$value>0),])

# Only showing dbcan values greater than one
# Append a column of taxonomy that match value between m.dbcan and the nb.of.MAG.each.taxa$newMAGname
new.m.dbcan <- m.dbcan %>% left_join(nb.of.MAG.each.taxa)

str(new.m.dbcan)

# Get a table specifying the order of the taxonomies
order.taxo <- read.table("Paper/Cazyme/order.taxonomy.tsv", header=TRUE, sep="\t")
order.I.want <- order.taxo$Taxonomy

order.I.want

new.m.dbcan$Taxonomy_f <- factor(new.m.dbcan$Taxonomy, levels=order)

# Plot figures:
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

new.m.dbcan$Taxonomy_f2 <- factor(new.m.dbcan$Taxonomy_f, levels=order.I.want)

new.m.dbcan <- left_join(new.m.dbcan, order.taxo, by=c("Taxonomy_f2"="Taxonomy"))

new.m.dbcan$Taxonomy_f

str(new.m.dbcan)

levels(new.m.dbcan$Taxonomy_f2)

# Replot:
ggplot(new.m.dbcan[which(new.m.dbcan$value>0),], aes(x=Taxonomy_f2, y=value)) + 
  geom_boxplot(lwd=0.4)+
  facet_grid(Domain.y+CPR+DPANN ~ variable,scales="free",space = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+
  ggtitle("CAZyme distribution in 523 MAGs in Lake Tanganyika",subtitle = "Nov 11, 2020")+
  xlab("Taxonomy")+
  ylab("Number of Hits")

ggsave("Paper/Cazyme/cazyme.2020.11.10.pdf", height = 11, width = 8.5, units = "in")


nb.of.MAG.each.taxa$Taxonomy_f <- factor(nb.of.MAG.each.taxa$Taxonomy, levels=order.I.want)

nb.of.MAG.each.taxa2 <- with(nb.of.MAG.each.taxa, nb.of.MAG.each.taxa[order(Taxonomy_f),])

order.I.want.mag.name <- nb.of.MAG.each.taxa2$newMAGname

new.m.dbcan$newMAGname_f <- factor(new.m.dbcan$newMAGname, levels=order.I.want.mag.name)


# Final plot :) !!
ggplot(new.m.dbcan[which(new.m.dbcan$value>0),], aes(x=newMAGname_f, y=value)) + 
  geom_boxplot(lwd=0.4)+
  facet_grid(Domain.y+CPR+DPANN ~ variable,scales="free",space = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+
  ggtitle("CAZyme distribution in 523 MAGs in Lake Tanganyika",subtitle = "November 11, 2020")+
  xlab("Taxonomy")+
  ylab("Number of Hits")

ggsave("Paper/Cazyme/cazyme.2020.11.11.with.number.of.mags.2.pdf", height = 11, width = 8.5, units = "in")