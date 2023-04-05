#install.packages("BiodiversityR")
## Load packages:
library(tidyverse)
library(ggrepel)
library(BiodiversityR)
library(stringr)

## Load data
sp.matrix <- read.table("Paper/Abundance-Plot/abundance.2020-20-21-grouped-by-taxa.tsv", sep="\t", row.names=1,
                        header=TRUE)
sp.matrix$taxa <- rownames(sp.matrix)

# Fix spelling mistakes
sp.matrix$taxa <- str_replace(sp.matrix$taxa, "CP Verstraetaerchaeota", "CP Verstratearchaeota")
sp.matrix$taxa <- str_replace(sp.matrix$taxa, "Kirimatiellacea", "Kirimatiellaeota")
sp.matrix$taxa <- str_replace(sp.matrix$taxa, "CP Aminicemantes (OP8)", "CP Aminicenantes (CP8)")

# Rename rows
rownames.sp.matrix <- sp.matrix$taxa
sp.matrix <- sp.matrix[,-25]
row.names(sp.matrix) <- rownames.sp.matrix

# Transpose species matrix
sp.matrix <- t(sp.matrix)

cal.rankabund <- as.data.frame(rankabundance(sp.matrix))
rankabunplot(rankabundance(sp.matrix), scale='abundance', addit=FALSE, specnames=c(1:62),srt=0, las=1)


cal.rankabund$specie <- rownames(cal.rankabund)

# Plot figure using ggplot:
ggplot(cal.rankabund, aes(x=rank, y=proportion, label=specie))+
  geom_point(pch=21)+
  geom_line()+
  scale_x_discrete(name ="Taxa", 
                   limits=cal.rankabund$specie)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab("Abundance")



matrix.env <- read.table("~/Documents/Github/LakeTanganyika/Figures/Paper/Samples/samples.tsv", sep="\t", header=TRUE, row.names=1)[,8:9]

matrix.env$Location <- as.factor(matrix.env$Location)
matrix.env$Depth <- as.factor(matrix.env$Depth)

dim(sp.matrix)

rankabundance(x=sp.matrix, y=matrix.env, factor='Location', level = )

rankabuncomp(sp.matrix, y=matrix.env, factor='Location', 
             scale='proportion', legend=TRUE)

rankabuncomp(sp.matrix, y=matrix.env, factor='Depth', 
            scale='proportion', legend=FALSE)

