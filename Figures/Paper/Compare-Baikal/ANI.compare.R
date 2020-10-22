# compare ANI Baikal and Tanganyika

LB.LT <- read.table("Paper/Compare-Baikal/ANI.Baikal.Tanganyika.tsv", header=TRUE, sep="\t")
library(tidyverse)

LB.LT.gathered <- LB.LT %>% gather("Genome2", "ANI", 2:699)
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
       
ggsave("Compare.ANI.above.80.pdf", height = 8.5, width = 11, units = "in")

ggarrange(ani.1, ani.2, nrow=2, labels=c("A","B"), heights = c(0.5,1))
