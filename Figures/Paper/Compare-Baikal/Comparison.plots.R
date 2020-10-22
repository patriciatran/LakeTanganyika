# Patricia Tran
# Compare Baikal 
# Top 90% most abundant organism in 1200m Tanganyika vs. all MAGs from 1300m in Baikal

library(tidyverse)

#Load data
compare.data <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Compare-Baikal/Compare-Baikal.csv", header=TRUE)[,-1]

str(compare.data)
library(dplyr)

library(RColorBrewer)
# At the Domain level
Domain.plot <- compare.data %>% select(Domain, Category) %>%
  group_by(Domain, Category) %>%
  tally() %>%
  ggplot(aes(x=Domain, y=n, fill=Category)) +
  geom_bar(position="stack", stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette = "Dark2")+
  ylab("Number of MAGs")

Domain.plot

# Phylum:

data1 <- compare.data %>% select(Domain, Phylum, Category) %>%
  group_by(Phylum, Category) %>%
  tally() 

compare.data1 <- compare.data %>% select(Domain, Phylum) %>% unique()

data1 <- left_join(data1, compare.data1)


Phylum.plot <- data1%>% 
  ggplot(aes(x=Phylum, y=n, fill=Category)) +
  geom_bar(position="stack", stat="identity")+
  facet_grid(.~Domain, scales="free") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette = "Dark2")+
  ylab("Number of MAGs")

Phylum.plot


library(ggpubr)

ggarrange(Domain.plot, Phylum.plot, common.legend = TRUE, widths=c(1,2), labels = c("A","B"))

# Class
Class.plot <- compare.data %>% select(Domain, Phylum, Class, Category) %>%
  group_by(Class, Category) %>%
  tally() %>%
  # Add a column with the phylum:
  left_join(compare.data) %>% 
  ggplot(aes(x=Class, y=n, fill=Category)) +
  geom_bar(position="stack", stat="identity")+
  facet_grid(.~Domain+Phylum, scales="free") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette = "Dark2")+
  ylab("Number of MAGs")

Class.plot


# Class
Order.plot.Archaea <- compare.data %>% select(Domain, Phylum, Class, Order.1, Category) %>%
  group_by(Order.1, Category) %>%
  tally() %>%
  # Add a column with the phylum:
  left_join(compare.data) %>% 
  filter(Domain=="d__Archaea") %>%
  ggplot(aes(x=Order.1, y=n, fill=Category)) +
  geom_bar(position="stack", stat="identity")+
  facet_grid(.~Phylum+Class, scales="free") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette = "Dark2")+
  ylab("Number of MAGs")+
  ylab("Order")


Order.plot.Archaea


Order.plot.Bacteria <- compare.data %>% select(Domain, Phylum, Class, Order.1, Category) %>%
  group_by(Order.1, Category) %>%
  tally() %>%
  # Add a column with the phylum:
  left_join(compare.data) %>% 
  filter(Domain=="d__Bacteria") %>%
  ggplot(aes(x=Order.1, y=n, fill=Category)) +
  geom_bar(position="stack", stat="identity")+
  facet_grid(.~Phylum+Class, scales = "free") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1, size = 5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette = "Dark2")

Order.plot.Bacteria

library(patchwork)


pdf("~/Documents/Github/LakeTanganyika/Compare-Baikal/Domain.phylum.LB.LT.pdf", width = 11, height =8.5)
Domain.plot / Phylum.plot
dev.off()


pdf("~/Documents/Github/LakeTanganyika/Compare-Baikal/Class.LB.LT.pdf", width = 25, height =8.5)
Class.plot
dev.off()

pdf("~/Documents/Github/LakeTanganyika/Compare-Baikal/Archaea.Order.LB.LT.pdf", width = 11, height =8.5)
Order.plot.Archaea
dev.off()

pdf("~/Documents/Github/LakeTanganyika/Compare-Baikal/Bacteria.Order.LB.LT.pdf", width = 30, height =8.5)
Order.plot.Bacteria
dev.off()
