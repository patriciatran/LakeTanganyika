sqr_list <- read.table("~/Documents/Github/LakeTanganyika/FigureS4-Cyano-vs-SQR-genomes/list_SQR.txt", sep="\t", header=TRUE)
cov.all.mags <- read.table("~/Documents/Github/LakeTanganyika/FigureS4-Cyano-vs-SQR-genomes/coverage_all_MAGS.tsv", sep="\t", header=TRUE)

library(tidyverse)

sqr_cov <- left_join(sqr_list, cov.all.mags, by="MAG")

colnames(sqr_cov)
# Now get it in a plottable format:
sqr_cov_transposed <- as.data.frame(t(sqr_cov %>% select(MAG, c(13:36) )))
colnames(sqr_cov_transposed) <- as.character(unlist(sqr_cov_transposed[1,]))
sqr_cov_transposed <- sqr_cov_transposed[-1,]
sqr_cov_transposed$Sample <- rownames(sqr_cov_transposed)
#Reoder:
sqr_cov_transposed <- sqr_cov_transposed[,c(96,1:95)]
# Now we add a column that says what depth the 24 samples are:
sample.lookup <- read.table("~/Documents/Github/LakeTanganyika/FigureS4-Cyano-vs-SQR-genomes/sample_look_up.tsv", header=TRUE, sep="\t")

joined.sample.lookup <- left_join(sqr_cov_transposed, sample.lookup)

# Gather the data:
sqr.gathered <- joined.sample.lookup %>% gather(key="MAG", value="Coverage",c(2:96))
# Add a column with Taxonomy

sqr.gathered.taxonomy <- sqr.gathered %>% 
  left_join(select(sqr_list, MAG, Taxonomy), by = "MAG")

sqr.gathered.taxonomy$Coverage <- as.numeric(sqr.gathered.taxonomy$Coverage)
library(ggplot2)

# split into Cyano (anoxygenic photosynthesis versus others)

sqr.gathered.taxonomy$Category <- ""

for (i in 1:nrow(sqr.gathered.taxonomy)){
  if (sqr.gathered.taxonomy$Taxonomy[i] == "Cyanobacteria"){
    sqr.gathered.taxonomy$Category[i] = "Cyanobacteria"
  }
  else {
    sqr.gathered.taxonomy$Category[i] = "Others"
    }
}

# count how many cyanos with sqr:
just_cyano_sqr <- subset(sqr_list, sqr_list$Taxonomy == "Cyanobacteria")
length(unique(just_cyano_sqr$MAG))
# there are 19 MAGs with SQR.

abundance.mags <- read.table("~/Documents/Github/LakeTanganyika/FigureS4-Cyano-vs-SQR-genomes/coverage_all_MAGS.tsv", header=T, sep="\t")
# count how many cyanos there are overall in dataset:
just_cyano_abund <- subset(abundance.mags, abundance.mags$Taxonomy == "Cyanobacteria")
length(unique(just_cyano_abund$MAG))
# and there are 57 Cyanos overall.

#Non cyano:
non_cyano_sqr <- subset(sqr_list, sqr_list$Taxonomy != "Cyanobacteria")
length(unique(non_cyano_sqr$MAG))


# Reshape the cyanos:
cyano_cov_transposed <- as.data.frame(t(just_cyano_abund %>% select(MAG, c(4:26) )))
colnames(cyano_cov_transposed) <- as.character(unlist(cyano_cov_transposed[1,]))
cyano_cov_transposed <- cyano_cov_transposed[-1,]
cyano_cov_transposed$Sample <- rownames(cyano_cov_transposed)
#Reoder:
cyano_cov_transposed <- cyano_cov_transposed[,c(58,1:57)]

joined.cyano.cov <- left_join(cyano_cov_transposed, sample.lookup)

# Gather the data:
cyano.gathered <- joined.cyano.cov %>% gather(key="MAG", value="Coverage",c(2:58))
# Add a column with Taxonomy

cyano.gathered$Coverage<- as.numeric(cyano.gathered$Coverage)
# add a column to say if the MAG names is part of sqr_list

cyano.gathered$sqr <- cyano.gathered$MAG %in% sqr_list$MAG
cyano.gathered$Cyano <- "Cyanobacteria"

# plot cyanoonly
cyano.plot <- ggplot(cyano.gathered) + geom_point(aes(x=Coverage, y=Depth, shape=Location, color=sqr))+
  facet_grid(.~Cyano)+
  scale_color_manual(values=c("#000000","#56B4E9"))+
  scale_y_reverse(lim=c(1250,0))+
  theme_bw()+
  xlab("Coverage in metagenome")+
  ylab("Depth (m)")+
  geom_hline(yintercept=50, linetype="dashed", color = "grey")+
  geom_hline(yintercept=100, linetype="dashed", color = "grey")+
  geom_hline(yintercept=70, linetype="dashed", color = "red")+
  ggtitle("Distribution of all Cyanobacteria")+
  theme(legend.position="none")
  
cyano.plot


# Plot bipanel of Cyanos with sqr with the Other groups
sqr_plot <- ggplot(sqr.gathered.taxonomy) + geom_point(aes(x=Coverage, y=Depth, color=Taxonomy, shape=Location))+
  facet_grid(.~Category)+
  scale_y_reverse(lim=c(1250,0))+
  theme_bw()+
  xlab("Coverage in metagenome")+
  ylab("Depth (m)")+
  geom_hline(yintercept=50, linetype="dashed", color = "grey")+
  geom_hline(yintercept=100, linetype="dashed", color = "grey")+
  geom_hline(yintercept=70, linetype="dashed", color = "red")+
  ggtitle("Distribution of Sulfide Oxidizers")

library(ggpubr)
ggarrange(cyano.plot, sqr_plot, labels=c("A","B"),widths=c(1,1.5))

## Try a different plot look where I sum the coverage by phyla instead of showing all the indivdual MAGS:
# Get a count for how many MAGS in each phyla first though:
detach("package:plyr", unload=TRUE)

tally.sqr<- sqr.gathered.taxonomy %>%
  group_by(Taxonomy) %>%
  summarise(Nb.distinct.MAG = n_distinct(MAG))

tally.sqr$tallied.taxo.sqr <- paste(tally.sqr$Taxonomy, " (",tally.sqr$Nb.distinct.MAG," MAGs)",sep="")


library(dplyr)

sqr.gathered.taxonomy.depth <- sqr.gathered.taxonomy %>% group_by(Taxonomy, Depth, Sample, Location, Category) %>% summarize(sum(Coverage))

colnames(sqr.gathered.taxonomy.depth)[6] <- "Sum.Of.Coverage"

# Add a column of tally:
sqr.gathered.taxonomy.depth.tally <- left_join(sqr.gathered.taxonomy.depth, tally.sqr)
# Now we replot:
ggplot(sqr.gathered.taxonomy.depth.tally %>% filter(Taxonomy != "Woesearchaeota")) + geom_point(aes(x=Sum.Of.Coverage, y=Depth, color=tallied.taxo.sqr, shape=Location))+
  facet_grid(.~Category)+
  scale_y_reverse(lim=c(1250,0))+
  theme_bw()+
  xlab("Sum of Taxonomic Groups' Coverage in metagenome")+
  ylab("Depth (m)")+
  geom_hline(yintercept=50, linetype="dashed", color = "grey")+
  geom_hline(yintercept=100, linetype="dashed", color = "grey")+
  geom_hline(yintercept=70, linetype="dashed", color = "red")+
  ggtitle("Distribution of Sulfide Oxidizers")
