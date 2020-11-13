# Patricia Tran
# Nov 13, 2020
# comparison of sample size between baikal and tanganyika

comparison.mg.size <- read.csv("Paper/Compare-Baikal/compare.mg.size.baikal.tangy.csv")

library(tidyverse)
library(ggrepel)
ggplot(comparison.mg.size, aes(x=Uncompressed.size, y=nb.reads.millions, color=Dataset, label=Short.name))+
  geom_point(size=4, alpha=0.5)+
  theme_bw()+
  theme(legend.position = "top")+
  xlab("Uncompressed fastq size (Gb)")+
  ylab("Number of reads (Millions)")+
  geom_text_repel()

ggsave("Paper/Compare-Baikal/comparison.size.PDF", width = 8.5, height = 8.5, units="in")

ggsave("Paper/Compare-Baikal/comparison.size.png", width = 8.5, height = 8.5, units="in")
