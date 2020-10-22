#install.packages("BiodiversityR")
library(BiodiversityR)

sp.matrix <- read.table("Paper/Abundance-Plot/abundance.2020-20-21-grouped-by-taxa.tsv", sep="\t", row.names=1,
                        header=TRUE)
sp.matrix <- t(sp.matrix)

rankabunplot(rankabundance(sp.matrix), scale='abundance', addit=FALSE, specnames=c(1,2,3), )

## CLICK IN THE GRAPH TO INDICATE WHERE THE LEGEND NEEDS TO BE PLACED
## IF YOU OPT FOR LEGEND=TRUE.

matrix.env <- read.table("Paper/Samples/samples.csv", sep="\t", header=TRUE, row.names=1)[,8:9]

matrix.env$Location <- as.factor(matrix.env$Location)
matrix.env$Depth <- as.factor(matrix.env$Depth)

dim(sp.matrix)
rankabuncomp(sp.matrix, y=matrix.env, factor='Location', 
             scale='proportion', legend=TRUE)

rankabuncomp(sp.matrix, y=matrix.env, factor='Depth', 
            scale='proportion', legend=TRUE)

