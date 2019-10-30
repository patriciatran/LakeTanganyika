##
## Three genes
##
## Load library & data
install.packages("genoPlotR")
library(genoPlotR)
data("three_genes")

mag1 <- read.table("Box/PhD/Research/Lake-Tanganyika/1_Code/Archaea-amoABC/K_DeepCast_100m_mx_034.tsv", sep="\t", header=TRUE)
mag2 <- read.table("Box/PhD/Research/Lake-Tanganyika/1_Code/Archaea-amoABC/K_Offshore_80m_m2_203.tsv", sep="\t", header=TRUE)
#ref <- read.table("Box/PhD/Research/Lake-Tanganyika/1_Code/Archaea-amoABC/Nitrososphaera gargensis.tsv", sep="\t", header=TRUE)

mag1 <- dna_seg(mag1)
mag2 <- dna_seg(mag2)
#ref <- dna_seg(ref)

my_dna_segs <- list(mag1,mag2)
plot_gene_map(dna_segs=my_dna_segs)