# Exploring CSV Match between Manual and GTDB classification:

mag.data <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/MAG_Characteristics/Supplementary_Table_S2_MAG_characteristics.csv")

library(ggplot2)

str(mag.data)

library(tidyverse)
data.to.plot <- mag.data %>% select(Domain,Manual.Taxonomy, Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk) %>%
  group_by(Domain,Manual.Taxonomy, Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk) %>%
  summarise(count=n())

data.to.plot

str(data.to.plot$Manual.Taxonomy)

library(RColorBrewer)
# Stacked
stack.bar.plot.compare <- ggplot(data.to.plot, aes(fill=Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk, x=count, y=as.character(Manual.Taxonomy))) + 
  geom_bar(position="stack", stat="identity")+
  facet_grid(Domain~., scales="free",space="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        #axis.text = element_text(size=10),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))+
  ylab("Manual Taxonony of 523 MAGs")+
  xlab("Count")+
  scale_fill_discrete(name = "Comparison between Manual Taxonomy and GTDB-tk taxonomy")+
  guides(shape = guide_legend(override.aes = list(size = 0.25)),
         color = guide_legend(override.aes = list(size = 0.25))) # make legend smaller
  
stack.bar.plot.compare
# Comparison of genome completeness per group
box.plot.compare <- mag.data %>% select(Domain, Manual.Taxonomy, Genome.completeness..CheckM.) %>%
  ggplot(aes(x=Genome.completeness..CheckM., y=Manual.Taxonomy))+
  facet_grid(Domain~., scales="free", space="free_y")+
  geom_point(pch=21, color="black", fill="white", alpha=0.4)+
  geom_boxplot(pch=21, color="black", fill="black", alpha=0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #text = element_text(size=10)),
        axis.text.y=element_blank(),
        axis.title.y =  element_blank(),
        axis.ticks.y = element_blank())+
  xlab("Genome completeness (CheckM) (%)")
  #ylab("Manual Taxonomy (RP16)")
  
box.plot.compare

library(ggpubr)
ggarrange(stack.bar.plot.compare, box.plot.compare, nrow=1, labels=c("A","B"), align="h", widths = c(1,0.5))

ggsave("Compared.gtdb.manual.pdf", width = 11, height = 8.5, units = "in")

plot2 <- mag.data %>% select(Domain, Manual.Taxonomy, Genome.completeness..CheckM., Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk) %>%
  group_by(Domain, Manual.Taxonomy, Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk) %>% 
  ggplot(aes(x=Genome.completeness..CheckM., y=Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk))+
  geom_point(alpha=0.5, pch=21, colour="black", size=3, aes(fill=Comparison.of.Manual.Taxonomy..16RP..and.GTDB.tk))+
  theme_bw()+
  xlab("Genome Completeness (%)")+
  ylab("Comparison of manual taxonomy (RP16) \n and automated taxonomy (GTDB-tk)")+
  theme(text = element_text(size=12),
        legend.position = "none")

plot2
ggsave("violin.plot.compare.pdf", width = 8, height = 12, units="in")

plot1 <- ggarrange(stack.bar.plot.compare, box.plot.compare, nrow=1, align="h", widths = c(1,0.5))

plot3 <- ggarrange(plot1, plot2, ncol=1,labels=c("A","B"), heights = c(1,0.75))

plot3
ggsave("compare.gtdb.stats", width = 8.5, height = 11, unit="in")
