library(readxl)
Compare <- read_excel("Paper/Comparison-Taxonomy/Compare.xlsx")
View(Compare)   

library(tidyverse)


Compare %>% filter(!is.na(TaxAss_combined)) %>%
  group_by(Manual_Taxonomy, Match) %>%
  tally() %>%
  ggplot(aes(x=Manual_Taxonomy, y=n, fill=Match))+
  geom_bar(stat = "identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

order.taxo <- read.table("Paper/Cazyme/order.taxonomy.tsv", header=TRUE, sep="\t")
order.I.want <- order.taxo$Taxonomy

Compare$Manual_Taxonomy <- factor(Compare$Manual_Taxonomy, levels = order.I.want)

Compare <- left_join(Compare, order.taxo, by=c("Manual_Taxonomy" = "Taxonomy"))

install.packages("plotly")
library(plotly) 

Compare %>% filter(!is.na(TaxAss_combined)) %>%
  group_by(Manual_Taxonomy, Match, Domain, CPR) %>%
  tally() %>%
  ggplot(aes(x=Manual_Taxonomy, y=n, fill=Match))+
  geom_bar(stat = "identity")+
  facet_grid(.~Domain+CPR, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "top")

data <- Compare %>% filter(!is.na(TaxAss_combined)) %>%
  group_by(Manual_Taxonomy, Match, Domain, CPR) %>%
  tally() 

fig <- plot_ly(data, x = ~Manual_Taxonomy, y = ~n, color = ~Match, type = 'bar')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack', facet_row='Domain')

fig
