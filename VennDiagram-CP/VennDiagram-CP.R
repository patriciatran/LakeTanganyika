venn.data <- read.table("~/Box/PhD/Research/Lake-Tanganyika/1_Code/VennDiagram-CP/Venn-Diagram-CP.tsv", sep="\t", header=TRUE)

library(dplyr)
tanganyikabacteria <- venn.data %>% filter(CP=="CP Tanganyikabacteria")

unique(tanganyikabacteria$MAG)
list.K_DeepCast_65m_m2_236 <- tanganyikabacteria %>% filter(MAG=="K_DeepCast_65m_m2_236") %>% distinct(KO)
list.K_DeepCast_65m_m2_236 <- as.character(list.K_DeepCast_65m_m2_236$KO)

list.K_OffShore_40m_m2_103 <- tanganyikabacteria %>% filter(MAG=="K_OffShore_40m_m2_103") %>% distinct(KO)
list.K_OffShore_40m_m2_103 <- as.character(list.K_OffShore_40m_m2_103$KO)
list.M_DeepCast_65m_m2_071 <- tanganyikabacteria %>% filter(MAG=="M_DeepCast_65m_m2_071") %>% distinct(KO)
list.M_DeepCast_65m_m2_071 <- as.character(list.M_DeepCast_65m_m2_071$KO)

library(VennDiagram)

library(RColorBrewer)
myCol <- brewer.pal(3, "Pastel2")

pdf(file="Tanganyikabacteria_venn.pdf")
grid.draw(venn.diagram(
  x = list(list.K_DeepCast_65m_m2_236, list.K_OffShore_40m_m2_103, list.M_DeepCast_65m_m2_071),
  category.names = c("K_DeepCast_65m_m2_236" , "K_OffShore_40m_m2_103" , "M_DeepCast_65m_m2_071"),
  filename = NULL,
  output=TRUE,
  
  # Output features
  imagetype="tiff" ,
  scaled=TRUE,
  units = "px",
  #height = 100, 
  #width = 100 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)
)

dev.off()

## Now Let's Do this for Ziwabacteria

ziwabacteria <- venn.data %>% filter(CP=="CP Ziwabacteria")

unique(ziwabacteria$MAG)
list.K_DeepCast_100m_m2_001 <- ziwabacteria %>% filter(MAG=="K_DeepCast_100m_m2_001") %>% distinct(KO)
list.K_DeepCast_100m_m2_001 <- as.character(list.K_DeepCast_100m_m2_001$KO)

list.K_DeepCast_100m_m2_307 <- ziwabacteria %>% filter(MAG=="K_DeepCast_100m_m2_307") %>% distinct(KO)
list.K_DeepCast_100m_m2_307 <- as.character(list.K_DeepCast_100m_m2_307$KO)

list.K_DeepCast_100m_m2_321 <- ziwabacteria %>% filter(MAG=="K_DeepCast_100m_m2_321") %>% distinct(KO)
list.K_DeepCast_100m_m2_321 <- as.character(list.K_DeepCast_100m_m2_321$KO)


pdf(file="Ziwabacteria_venn.pdf")

grid.draw(venn.diagram(
  x = list(list.K_DeepCast_100m_m2_001, list.K_DeepCast_100m_m2_307, list.K_DeepCast_100m_m2_321),
  category.names = c("K_DeepCast_100m_m2_001" , "K_DeepCast_100m_m2_307" , "K_DeepCast_100m_m2_321"),
  filename = NULL,
  output=TRUE,
  
  # Output features
  imagetype="tiff" ,
  scaled=TRUE,
  units = "px",
  #height = 100, 
  #width = 100 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)
)

dev.off()
