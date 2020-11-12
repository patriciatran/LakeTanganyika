# Patricia Tran
# drep data

drep_full <- read.delim("Paper/dRrep/dRep-all-mags-nonderep.tsv", header=TRUE, sep="\t")
drep_representative <- read.delim("Paper/dRrep/dRep-clusters-523MAGS.tsv", header=TRUE, sep="\t")

drep_full_clusters <- drep_full %>% filter(Cluster.ID %in% drep_representative$Cluster.Rep)

# So when we do the JGI tables (see GitHub page), the list of headers should technically also contain the headers of the 3439 MAGs not just the 523 MAGs...?

write.table(x = drep_full_clusters, file="Paper/dRrep/drep_full_clusters.tsv",sep = "\t",quote = FALSE, row.names = FALSE)
