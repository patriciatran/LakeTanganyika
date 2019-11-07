mag_results <- read.table("~/Documents/Github/LakeTanganyika/FigureSX-DiversityIndices/mag_abundance.tsv", sep="\t", header=TRUE)
str(mag_results)
rownames(mag_results) <- mag_results$Taxa
mag_results <- mag_results[,-1]

str(mag_results)

mag_results <- as.matrix(mag_results)
mag_results <- t(mag_results)

class(mag_results)
mag_results.df <- as.data.frame(mag_results)


library(vegan)

## FOR MAGS:

class(mag_results.df)
H <- diversity(mag_results.df)
simp <- diversity(mag_results.df, "simpson")
invsimp <- diversity(mag_results.df, "inv")


H.df <- as.data.frame(H)
simp.df <- as.data.frame(simp)
invsimp.df <- as.data.frame(invsimp)

H.simp <- merge(H.df, simp.df)

pca.res <- prcomp(mag_results.df)
library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)

ggbiplot(pca.res,labels=rownames(mag_results.df))

depths <- c(0,35,0,40,0,10,0,0,0,0,0,50,65,65,80,100,150,100, 250,300,1200,120,200,400)
locations <- c(rep("Kigoma",5),rep("Mahale",8),rep("Kigoma",4),"Mahale",rep("Kigoma",4),rep("Mahale",2))
layers <- c(rep("Above Oxycline",11),rep("Oxycline",7),rep("Below Oxycline",6))


ggbiplot(pca.res,labels=rownames(mag_results.df), ellipse=TRUE, groups=depths)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  ggtitle("PCA of metagenome community in Lake Tanganyika")

ggbiplot(pca.res,labels=rownames(mag_results.df), eclipse=TRUE, groups=locations)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  ggtitle("PCA of metagenome community in Lake Tanganyika")

ggbiplot(pca.res,labels=rownames(mag_results.df), eclipse=TRUE, groups=layers)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  ggtitle("PCA of metagenome community in Lake Tanganyika")

