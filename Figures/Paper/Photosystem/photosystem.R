photosystem.assembly <- read.table("~/Documents/JGI-LakeTanganyika-Globus-Data/list.photosystem.tsv", header=FALSE, sep="\t")

str(photosystem.assembly)
library(tidyverse)

ps.assembly <- photosystem.assembly %>% select(V1, V2, V4, V44)

ps.assembly$V1<- str_replace(ps.assembly$V1, "/Table.*","")
ps.assembly$V1 <- str_replace(ps.assembly$V1, "_FD.*","")
ps.assembly$V1 <- str_replace(ps.assembly$V1, "TA2","")
ps.assembly$V1 <- str_replace(ps.assembly$V1, "mmetaG","")

unique(ps.assembly$V1)

ps.assembly <- ps.assembly %>% separate(col = V2, sep="_", into=c("GaID","Scaffold"))

sample.lookup.date <- read.table("~/Documents/Github/LakeTanganyika/Figures/Paper/Samples/samples.tsv", sep="\t", header=T)


ps.assembly <- left_join(ps.assembly, sample.lookup.date, by=c("GaID"="GaID"))

ps.assembly$ShortenedName_f <- factor(ps.assembly$ShortenedName, levels=sample.lookup.date$ShortenedName)

ps.assembly.tally <- ps.assembly %>% group_by(ShortenedName_f, V4, V44) %>% tally()

str(ps.assembly.tally)

ggplot(ps.assembly.tally, aes(x=ShortenedName_f, y=n, fill=V44))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Sample Name (organized by Location and Date")+
  ylab("Number of hits")

ggsave("Paper/Photosystem/MAG.vs.samples.PDF", width = 11, height = 8.5, units = "in")

# Let's add a column for the taxonomic assignment:
taxonomy <- read.csv("Paper/Photosystem/taxonomy.csv", header=TRUE)

ps.assembly.tally$V44 <- str_replace(ps.assembly.tally$V44,".fasta","")
ps.assembly.tally <- left_join(ps.assembly.tally, taxonomy, by=c("V44"="MAG"))

unique(ps.assembly.tally$ManualTaxonomy)

ggplot(ps.assembly.tally, aes(x=ShortenedName_f, y=n, fill=ManualTaxonomy))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Sample Name (organized by Location and Date")+
  ylab("Number of hits")+
  ggtitle("Diversity of organisms with Photosystem genes in binned scafflds/MAGs",
          subtitle="Binned MAGs and unbinned scaffolds")

ggsave("Paper/Photosystem/DivofmagswithPSgenes.PDF", width = 11, height = 8.5, units = "in")


# Same plot but without the NA:
ps.assembly.tally %>% filter(!is.na(ManualTaxonomy)) %>%
ggplot(aes(x=ShortenedName_f, y=n, fill=ManualTaxonomy))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Sample Name (organized by Location and Date")+
  ylab("Number of hits")+
  ggtitle("Diversity of organisms with Photosystem genes in binned scafflds/MAGs",
          subtitle="Binned MAGs only")

ggsave("Paper/Photosystem/DivofmagswithPSgenes.binnedonly.PDF", width = 11, height = 8.5, units = "in")

plotly.plot <- ps.assembly.tally %>% filter(!is.na(ManualTaxonomy)) %>%
  ggplot(aes(x=ShortenedName_f, y=n, fill=ManualTaxonomy))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Sample Name (organized by Location and Date")+
  ylab("Number of hits")+
  ggtitle("Diversity of organisms with Photosystem genes in binned scafflds/MAGs",
          subtitle="Binned MAGs only")

#library(plotly)

#ggplotly(plotly.plot)

# Organize by taxon.order:
taxon.order <- read.table("Paper/Cazyme/order.taxonomy.tsv", sep="\t", header=TRUE)

taxon.order$Taxonomy <- str_replace(taxon.order$Taxonomy ,"CP Tanganyikabacteria","Sericytochromatia, Tanganyikabacteria")

ps.assembly.tally$ManualTaxonomy <- str_replace(ps.assembly.tally$ManualTaxonomy, "Sericytochromatia Tanganyikabacteria", "Sericytochromatia, Tanganyikabacteria")

unique(ps.assembly.tally$ManualTaxonomy)

ps.assembly.tally$ManualTaxonomy_f <- factor(ps.assembly.tally$ManualTaxonomy, levels=taxon.order$Taxonomy)


ps.assembly.tally <- left_join(ps.assembly.tally, taxon.order, by=c("ManualTaxonomy"= "Taxonomy"))

test <- ps.assembly.tally %>% filter(!is.na(V44)) 

# I want to know out of all MAGs which genomes have which genes:
ps.assembly.tally %>% filter(!is.na(ManualTaxonomy)) %>%
  ggplot(aes(x=V4, y=ManualTaxonomy_f, fill=n))+
  geom_tile()+
  theme_bw()+
  facet_grid(CPR+DPANN~., scales="free", space="free")+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Genes associated with photosynthesis in binned MAGs")+
  xlab("Gene")+
  ylab("Taxonomic assignment of binned MAGs")

ggsave("Paper/Photosystem/Genes.associated.with.Ps.by.Taxonomy.PDF", height = 8.5, width = 11, units = "in")

# Let's zoom into Cyanos:
cyanos <- ps.assembly.tally %>% filter(ManualTaxonomy == "Cyanobacteria")

unique(cyanos$Class)
unique(cyanos$Order.x)
unique(cyanos$Family)
unique(cyanos$Species)

# All the Cyano bacteria belong to Synechococcales, Cyanobiaceae.

ps.assembly.tally %>% filter(ManualTaxonomy == "Cyanobacteria") %>%
  ggplot(aes(x=V4, y=V44, fill=n))+
  geom_tile()+
  theme_bw()+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Genes associated with photosynthesis in binned MAGs: Cyanobacteria only")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Gene")+
  ylab("MAG name")

ggsave("Paper/Photosystem/Cyanos.PDF", height = 11, width = 8.5, units="in")

# Chlorobi -- the PS genes in Kig 1200m:
ps.assembly.tally %>% filter(ManualTaxonomy == "Chlorobi") %>%
  ggplot(aes(x=V4, y=V44, fill=n))+
  geom_tile()+
  theme_bw()+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Genes associated with photosynthesis in binned MAGs: Chlorobi only")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Gene")+
  ylab("MAG name")

ggsave("Paper/Photosystem/Chlorobi.PDF", height = 8.5, width = 11, units="in")

## WOR-3 our only CPR with PS genes
ps.assembly.tally %>% filter(ManualTaxonomy == "CP WOR-3") %>%
  ggplot(aes(x=V4, y=V44, fill=n))+
  geom_tile()+
  theme_bw()+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Genes associated with photosynthesis in binned MAGs: CP WOR-3  only")+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Gene")+
  ylab("MAG name")

ggsave("Paper/Photosystem/CP-WOR-3.PDF", height = 8.5, width = 11, units="in")


## All MAGs with a bar on the side for Taxo:

ps.assembly.tally %>% filter(!is.na(ManualTaxonomy)) %>%
  ggplot(aes(x=V4, y=V44, fill=n))+
  geom_tile()+
  theme_bw()+
  facet_grid(CPR+DPANN~., scales="free", space="free")+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Genes associated with photosynthesis in binned MAGs")+
  xlab("Gene")+
  ylab("Taxonomic assignment of binned MAGs")




