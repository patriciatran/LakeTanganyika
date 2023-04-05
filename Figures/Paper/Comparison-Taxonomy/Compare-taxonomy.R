# This code compares the taxonomy between manual, GTDB-tk and 16S rRNA taxonomy, when applicable.

# Load libraries:
library(readxl)
library(tidyverse)

# Load data
Compare <- read.table("Paper/Comparison-Taxonomy/compare.rp16.gtdb.taxass.tsv", sep="\t", header=TRUE)
#View(Compare)   

# Fix spelling mistakes ...
Compare$Manual_Taxonomy <- str_replace(Compare$Manual_Taxonomy, "CP Verstraetaerchaeota", "CP Verstratearchaeota")
Compare$Manual_Taxonomy <- str_replace(Compare$Manual_Taxonomy, "Kirimatiellacea", "Kirimatiellaeota")
Compare$Manual_Taxonomy <- str_replace(Compare$Manual_Taxonomy, "CP Aminicemantes (OP8)", "CP Aminicenantes (CP8)")
#Compare$Manual_Taxonomy <- str_replace(Compare$Manual_Taxonomy, "Planctomycetes \\(.*", "Phycisphaerae")
Compare$Manual_Taxonomy <- str_replace(Compare$Manual_Taxonomy, "Verrucomicrobia.*", "Verrucomicrobia")

# Plot the 16S taxonomy:
Compare %>% filter(!is.na(TaxAss_combined)) %>%
  group_by(Manual_Taxonomy, Match) %>%
  tally() %>%
  ggplot(aes(x=Manual_Taxonomy, y=n, fill=Match))+
  geom_bar(stat = "identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Reorder the taxa.
order.taxo <- read.table("Paper/Cazyme/order.taxonomy.tsv", header=TRUE, sep="\t")
order.I.want <- unique(order.taxo$Taxon.order)

Compare$Manual_Taxonomy <- factor(Compare$Manual_Taxonomy, levels = order.I.want)

order.taxo1 <- order.taxo %>% select(Taxon.order, Domain, CPR, DPANN) %>% unique()

Compare <- left_join(Compare, order.taxo1, by=c("Manual_Taxonomy" = "Taxon.order"))

domain.na <- Compare %>% filter(is.na(Domain))
#install.packages("plotly")
#library(plotly) 

Compare %>% filter(TaxAss_combined != "") %>%
  group_by(Manual_Taxonomy, Match, Domain, CPR, DPANN) %>%
  tally() %>%
  ggplot(aes(x=Manual_Taxonomy, y=n, fill=Match))+
  geom_bar(stat = "identity")+
  facet_wrap(.~Domain+CPR+DPANN, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "top")
 
Compare.plot <- Compare %>% filter(TaxAss_combined != "") %>%
  group_by(Manual_Taxonomy, Match, Domain, CPR, DPANN) %>%
  tally() %>%
  ggplot(aes(x=Manual_Taxonomy, y=n, fill=Match))+
  geom_bar(stat = "identity")+
  facet_wrap(.~Domain+CPR+DPANN, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")+
  ylab("Number of MAGs")
fig <- ggplotly(Compare.plot)

fig
