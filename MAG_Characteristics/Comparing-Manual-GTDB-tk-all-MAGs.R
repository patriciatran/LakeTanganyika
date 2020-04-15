# Exploring CSV Match between Manual and GTDB classification:

mag.data <- read.csv("~/Documents/Github/LakeTanganyika/MAG_Characteristics/Supplementary_Table_S2_MAG_characteristics.csv")

library(ggplot2)

# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

# Grouped
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")

# Stacked
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")

library(tidyverse)
data.to.plot <- mag.data %>% select(Domain,Manual.Taxonomy, Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk) %>%
  group_by(Domain,Manual.Taxonomy, Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk) %>%
  summarise(count=n())

str(data.to.plot$Manual.Taxonomy)

library(RColorBrewer)
# Stacked
ggplot(data.to.plot, aes(fill=Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk, x=count, y=as.character(Manual.Taxonomy))) + 
  geom_bar(position="stack", stat="identity")+
  facet_grid(Domain~., scales="free",space="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("Manual Taxonony of 523 MAGs")+
  ylab("Count")
  scale_fill_discrete(name = "Comparison between Manual Taxonomy and GTDB-tk taxonomy")
  
# Comparison of genome completeness per group
mag.data %>% select(Domain, Manual.Taxonomy, Genome.completeness..CheckM.) %>%
  ggplot(aes(x=Genome.completeness..CheckM., y=Manual.Taxonomy))+
  facet_grid(Domain~., scales="free", space="free_y")+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("Genome completeness (CheckM) (%)")
  

mag.data %>% select(Domain, Manual.Taxonomy, Genome.completeness..CheckM., Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk) %>%
  group_by(Domain, Manual.Taxonomy, Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk)+
  ggplot(aes(x=Genome.completeness..Checkm,y=Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk))+
  geom_boxplot()
