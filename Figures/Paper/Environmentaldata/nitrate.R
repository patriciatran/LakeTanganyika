# Ploting the nitrate data
# Load packages:

# Load data
nitrate <- read.table("~/Documents/Github/LakeTanganyika/Figures/Paper/Environmentaldata/Nitrate.tsv", sep="\t", header=TRUE)
colnames(nitrate)

ggplot(nitrate)+
  geom_point(aes(x=SRP.ug.L, y=-Depth..m.), fill="blue", alpha=0.7, size=4, pch=21, color="black")+
  geom_point(aes(x=NO3.ug.L, y=-Depth..m.), fill="red", alpha=0.7, size=4, pch=21, color="black")+
  facet_grid(.~Site)+
  ylab("Depth (m)")+
  xlab("Concentration (ug/L)")+
  theme_bw()

ggsave("Figures/Paper/Environmentaldata/Nitrate.pdf", width=10, height=8)