# Patricia Tran
# compare ANI Baikal and Tanganyika
# October 22, 2020

# Load libraries:
library(tidyverse)


# Load tables:
LB.LT <- read.table("Paper/Compare-Baikal/ANI.Baikal.Tang.491.vs.491.tsv", header=TRUE, sep="\t")
LB.LT.gathered <- LB.LT %>% gather("Genome2", "ANI", 2:ncol(LB.LT))
colnames(LB.LT.gathered) <- c("Genome1","Genome2", "ANI")
LB.LT.gathered.not.na <- LB.LT.gathered %>% filter(!is.na(ANI)) %>% unique()
#Remove where you're comparing the same genome

LB.LT.gathered.not.na$self <- TRUE

for (i in 1:nrow(LB.LT.gathered.not.na)){
  if (LB.LT.gathered.not.na$Genome1[i] == LB.LT.gathered.not.na$Genome2[2]){
    LB.LT.gathered.not.na$self[i] <- TRUE
  }
  else{
    LB.LT.gathered.not.na$self[i] <- FALSE
  }
  print(i)
}

LB.LT.gathered.not.na %>% filter(self==TRUE)

# There are no dupplicates

LB.LT.gathered.not.na <- LB.LT.gathered.not.na %>%  
  mutate(group = case_when(grepl("genomic", Genome1) ~ "Baikal",
                           grepl("fasta", Genome1, ignore.case = TRUE) ~"Tanganyika"))

LB.LT.gathered.not.na <- LB.LT.gathered.not.na %>%  
  mutate(group2 = case_when(grepl("genomic", Genome2) ~ "Baikal",
                           grepl("fasta", Genome2, ignore.case = TRUE) ~"Tanganyika"))

LB.LT.gathered.not.na$versus <- paste0(LB.LT.gathered.not.na$group,"_vs_",LB.LT.gathered.not.na$group2)

# calculate mean of each group:
library(plyr)
mu <- ddply(LB.LT.gathered.not.na, "versus", summarise, grp.mean=mean(ANI))

mu

LB.LT.gathered.not.na %>% group_by(versus) %>% tally()

ani.1 <- ggplot(LB.LT.gathered.not.na, aes(x=ANI, color=versus)) +
  geom_histogram(alpha=0.5,bins=100, aes(fill=versus))+
  theme_bw()+
  theme(legend.position="top")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=versus),
             linetype="dashed")+
  xlab("Average Nucleotide Identity (ANI) in %")+
  ylab("Number of pairs")+
  ggtitle("Comparison of all vs. all MAGs in the deepest \nsamples from Lake Tanganyika and Lake Baikal")
  
ani.1

ggsave("Compare.ANI.pdf", height = 8.5, width = 11, units = "in")

ani.2 <- ggplot(LB.LT.gathered.not.na %>% filter(ANI >80),aes(x=ANI, color=versus)) +
         geom_histogram(alpha=0.5,bins=100, aes(fill=versus))+
         theme_bw()+
  facet_grid(versus~.)+
         theme(legend.position="top")+
         geom_vline(data=mu, aes(xintercept=grp.mean, color=versus),
                    linetype="dashed")+
         xlab("Average Nucleotide Identity (ANI) in %")+
         ylab("Number of pairs")+
         ggtitle("Comparison of all vs. all MAGs in the deepest \nsamples from Lake Tanganyika and Lake Baikal",
                 subtitle="Only showing ANI > 80%")
ani.2

ggsave("Compare.ANI.above.80.pdf", height = 8.5, width = 11, units = "in")

plot1 <- ggarrange(ani.1, ani.2, ncol=2, labels=c("A","B"), heights = c(0.5,1), common.legend = TRUE)

plot1
ggsave("Compare.ANI.above.80.panel.pdf", height = 11, width = 8.5, units = "in")

## Now I'm curious to see among the pairwise Tang vs. Baikal -- what is the phylogenetic distribution of those?
LB.LT.gathered.not.na.versus <- LB.LT.gathered.not.na %>% filter(versus == "Tanganyika_vs_Baikal")


lookup <- read.table("~/Documents/Github/LakeTanganyika/Figures/Paper/Abundance-Plot/Simple_taxo_lookup.tsv", header=TRUE, sep="\t")
mag.data <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/MAG_Characteristics/Supplementary_Table_S2_MAG_characteristics.csv")

colnames(mag.data)
mag.data  <- mag.data %>% select(Genome..MAG.Identifier.,Manual.Taxonomy,Taxonomy..GTDB.tk.)

# remove ".fasta" from the name:
LB.LT.gathered.not.na.versus$Genome1 <- str_replace(LB.LT.gathered.not.na.versus$Genome1,".fasta","")

LB.LT.gathered.not.na.versus <- left_join(LB.LT.gathered.not.na.versus, mag.data, by=c("Genome1"="Genome..MAG.Identifier."))

plot3 <- ggplot(LB.LT.gathered.not.na.versus,aes(x=ANI, color=Manual.Taxonomy)) +
  geom_histogram(alpha=0.5,bins=100, aes(fill=Manual.Taxonomy))+
  theme_bw()+
  theme(legend.position="top")+
  xlab("Average Nucleotide Identity (ANI) in %")+
  ylab("Number of pair-wise comparisons")+
  ggtitle("Comparison of all vs. all MAGs in the deepest \nsamples from Lake Tanganyika and Lake Baikal",
          subtitle="Tanganyika_vs_Baikal only")

ggsave("Compare.ANI.Tanganyika.vs.Baikal.only.pdf", height = 8.5, width = 11, units = "in")



ggplot(LB.LT.gathered.not.na.versus %>% filter(ANI > 77),aes(x=ANI, color=Manual.Taxonomy)) +
  geom_histogram(alpha=0.5,bins=100, aes(fill=Manual.Taxonomy))+
  theme_bw()+
  theme(legend.position="top")+
  xlab("Average Nucleotide Identity (ANI) in %")+
  ylab("Number of pair-wise comparisons")+
  ggtitle("Comparison of all vs. all MAGs in the deepest \nsamples from Lake Tanganyika and Lake Baikal",
          subtitle="Tanganyika_vs_Baikal only, filter >80%  ANI")


scaleFUN <- function(x) sprintf("%.2f", x)

plot4 <- ggplot(LB.LT.gathered.not.na.versus,aes(x=ANI, color=Manual.Taxonomy)) +
  geom_histogram(alpha=0.5,bins=100, aes(fill=Manual.Taxonomy))+
  theme_bw()+
  facet_wrap(.~Manual.Taxonomy, scales="free")+
  theme(legend.position="none")+
  xlab("Average Nucleotide Identity (ANI) in %")+
  ylab("Number of pair-wise comparisons")+
  ggtitle("Tanganyika_vs_Baikal only")

ggarrange(plot1,  plot4, nrow=2,heights=c(0.75,1))

ggsave("Compare.ANI.3.panels.pdf", height = 11, width = 8.5, units = "in")




