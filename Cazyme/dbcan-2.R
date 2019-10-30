dbcan <- read.delim("~/Box/PhD/Research/Lake-Tanganyika/3_Output/dbCan-AllNRMAGS/dbcan-2019-10-11.tsv")

dbcan$Taxonomy <- as.character(dbcan$Taxonomy)
dbcan$Cazyme.density..nb.of.CAZYME.bp.<- as.numeric(dbcan$Cazyme.density..nb.of.CAZYME.bp.)
str(dbcan)

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

View(order.wanted)

str(m.dbcan[which(m.dbcan$value>0),])

# Only showing dbcan values greater than one
# Append a column of taxonomy that match value between m.dbcan and the nb.of.MAG.each.taxa$newMAGname
new.m.dbcan <- m.dbcan %>% left_join(nb.of.MAG.each.taxa)

bp <- ggplot(new.m.dbcan[which(new.m.dbcan$value>0),], aes(x=newMAGname, y=value)) + 
  geom_boxplot()+
  facet_grid(Domain ~ variable,scales="free",space = "free_y")+
  theme_base()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()

bp
# 
# #### EXTRA PLOT ### 
# order.wanted <- reorder(dbcan$Taxonomy, dbcan$Cazyme.density..nb.of.CAZYME.bp., FUN = median)
# 
# ALL <- ggplot(dbcan, aes(x = order.wanted, y = Cazyme.density..nb.of.CAZYME.bp.)) + geom_boxplot()+
#   theme_base()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   xlab("Taxonomic group")+
#   ylab("Cazyme density per genome size (Hits/bp)")+
#   coord_flip()
# ALL
# 
# AA <- ggplot(dbcan, aes(x=order.wanted, y=AA))+geom_boxplot()+
#   theme_base()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   #xlab("Taxonomic group")+
#   ylab("AA")+
#   coord_flip()+
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank())
# 
# AA
# 
# CBM <- ggplot(dbcan, aes(x=order.wanted, y=CBM))+geom_boxplot()+
#   theme_base()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   #xlab("Taxonomic group")+
#   ylab("CBM")+
#   coord_flip()+
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank())
# CBM
# 
# 
# CE <- ggplot(dbcan, aes(x=order.wanted, y=CE))+geom_boxplot()+
#   theme_base()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   #xlab("Taxonomic group")+
#   ylab("CE")+
#   coord_flip()+
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank())
# CE
# 
# cohesin <- ggplot(dbcan, aes(x=order.wanted, y=cohesin))+geom_boxplot()+
#   theme_base()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   #xlab("Taxonomic group")+
#   ylab("cohesin")+
#   coord_flip()+
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank())
# cohesin
# 
# GH <- ggplot(dbcan, aes(x=order.wanted, y=GH))+geom_boxplot()+
#   theme_base()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   #xlab("Taxonomic group")+
#   ylab("GH")+
#   coord_flip()+
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank())
# GH
# 
# GT <- ggplot(dbcan, aes(x=order.wanted, y=GT))+geom_boxplot()+
#   theme_base()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   #xlab("Taxonomic group")+
#   ylab("GT")+
#   coord_flip()+
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank())
# GT
# 
# 
# PL <- ggplot(dbcan, aes(x=order.wanted, y=PL))+geom_boxplot()+
#   theme_base()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   #xlab("Taxonomic group")+
#   ylab("PL")+
#   coord_flip()+
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank())
# PL
# 
# library(ggpubr)
# 
# 
# 
# 
# ggarrange(AA, CBM, CE, cohesin, GH, GT, PL,ncol=7, nrow=1)
