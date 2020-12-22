# Patricia Tran
# Environmental Data from Lake Tanganyika
# Data from Benjamin Kraemer
# October 1, 2019

cast.tang <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Environmentaldata/LT_CASTS_v4.csv")
library(lubridate)
library(ggplot2)
cast.tang$Date2 <- dmy(cast.tang$Date)
#Convert date to julian day
cast.tang$julianday <- format(cast.tang$Date2, "%j")
cast.tang$julianday <- as.numeric(cast.tang$julianday)

DO <- ggplot(cast.tang) + geom_point(aes(x = DO, y = Depth, colour = julianday)) + 
  scale_y_reverse()+
  facet_grid(cols = vars(Year))+
  ggtitle("DO")+
  theme_bw()

DO

Temp <- ggplot(cast.tang) + geom_point(aes(x = Temp, y = Depth, colour = julianday)) + 
  scale_y_reverse()+
  facet_grid(cols = vars(Year))+
  ggtitle("Temperature")+
  theme_bw()

Temp

library(ggpubr)

ggarrange(DO,Temp, nrow=2, labels=c("C","D"))

Chla <- ggplot(cast.tang) + 
  geom_point(aes(x = Chla, y = Depth, color = julianday,fill = julianday), alpha=1, pch=21) + 
  scale_y_reverse()+
  facet_wrap(.~Year)+
  xlab("Chlorophyll a (μg/L)")+
  theme_bw()+
  geom_hline(yintercept = 120)+
  ylab("Depth(m)")+
  theme(#axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        strip.background = element_rect(fill = 'white', colour = 'black'),
        strip.text = element_text(size=12),
        #legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=15),
        axis.text = element_text(colour = "black"))
  
Chla

ggsave("Paper/Environmentaldata/chla-2020-12-22.PDF", width = 11, height = 8.5, units = "in")

Conductivity <- ggplot(cast.tang) + geom_point(aes(x = Conductivity, y = Depth, colour = julianday)) + 
  scale_y_reverse()+
  facet_grid(cols = vars(Year))+
  ggtitle("Conductivity")+
  theme_bw()

Conductivity

str(cast.tang)
summary(cast.tang)

library(ggpubr)
ggarrange(DO, Temp, Chla, Conductivity, ncol = 1, nrow = 4)


cast.tang$Date
class(cast.tang$Date)

## SECCHI DEPTH:
secchi.tang <- read.csv("~/Documents/Github/LakeTanganyika/Figures/Paper/Environmentaldata/LT_Secchi_v1.csv")

secchi.tang$LatLong <- paste(secchi.tang$Latitude,secchi.tang$Longitude,sep=",")

secchi.plot <- ggplot(secchi.tang) + 
  geom_point(aes(x = ymd(Date), y = -Secchi.Depth),
             pch=21, color="black", fill="black", alpha=0.5, size=4) + 
  #facet_grid(cols = vars(secchi.tang$LatLong))+
  ggtitle("Secchi Depths in Lake Tanganyika (Lat -4.89, Long 29.59)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Secchi Depth in meters")+
  ylim(c(-20,0))
  
secchi.tang$kd <- 1.7/secchi.tang$Secchi.Depth

boxplot(secchi.tang$kd)

kd.plot <- ggplot(secchi.tang, aes(x=ymd(Date), y=kd))+
  geom_point(pch=21, color="black", fill="black", alpha=0.5, size=4)+
  ggtitle("Kd coefficient in Lake Tanganyika (Lat -4.89, Long 29.59)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("kd")

library(ggpubr)

ggarrange(secchi.plot, kd.plot, nrow=2, labels=c("A","B"))

library(rLakeAnalyzer)
#thermo.depth(cast.tang$Temp, unique(cast.tang$Depth))

library(dplyr)
average_temp_depth <- cast.tang %>% group_by(Date, Depth) %>% summarise(average=mean(Temp))
average_temp_depth <- average_temp_depth %>% mutate(thermocline = thermo.depth(average, Depth))

average_temp_depth %>% select(Date, thermocline) %>%
  ggplot()+
  geom_point(aes(x=Date, y=thermocline))+
  theme_bw()
 
library(ggplot2)
#Plot Thermocline Depth
library(tidyr)
# Split the date into 3 columns:
average_temp_depth <- average_temp_depth %>% separate(Date, c("Day","Month","Year"), remove=FALSE)

library(lubridate)
average_temp_depth$Date2 <- dmy(average_temp_depth$Date)
average_temp_depth


ggplot(average_temp_depth) + geom_point(aes(Date2, thermocline))+
  #facet_grid(cols = vars(Year))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #ylim(0,-150)+
  scale_y_reverse()+
  xlab("Year")+
  ylab("Thermocline Depth (m)")+
  ggtitle("Thermocline Depths over time in Lake Tanganyika")
  

## Oxycline depth:
library(dplyr)
average_o_depth <- cast.tang %>% group_by(Date, Depth) %>% summarise(average=mean(DO))
average_o_depth <- average_o_depth %>% mutate(oxycline = thermo.depth(average, Depth))

average_o_depth <- average_o_depth %>% separate(Date, c("Day","Month","Year"), remove=FALSE)
average_o_depth$Date2 <- dmy(average_o_depth$Date)

ggplot(average_o_depth) + 
  geom_point(aes(Date2, oxycline))+
  #facet_grid(cols = vars(Year))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0,-150)+
  scale_y_reverse()+
  xlab("Year")+
  ylab("Oxycline Depth (m)")+
  ggtitle("Oxycline Depths over time in Lake Tanganyika")


length(sort(unique(subset(cast.tang, cast.tang$Year=="2013")$julianday)))
length(sort(unique(subset(cast.tang, cast.tang$Year=="2012")$julianday)))
length(sort(unique(subset(cast.tang, cast.tang$Year=="2011")$julianday)))
length(sort(unique(subset(cast.tang, cast.tang$Year=="2010")$julianday)))

min((unique(subset(cast.tang, cast.tang$Year=="2013")$julianday)))
max((unique(subset(cast.tang, cast.tang$Year=="2013")$julianday)))

min((unique(subset(cast.tang, cast.tang$Year=="2012")$julianday)))
max((unique(subset(cast.tang, cast.tang$Year=="2012")$julianday)))

min((unique(subset(cast.tang, cast.tang$Year=="2011")$julianday)))
max((unique(subset(cast.tang, cast.tang$Year=="2011")$julianday)))

min((unique(subset(cast.tang, cast.tang$Year=="2010")$julianday)))
max((unique(subset(cast.tang, cast.tang$Year=="2010")$julianday)))


## DO and Temp contour plot:
library(akima)
library(fields)
library(viridis)

pdf(file="Paper/Environmentaldata/FigureS2-2020-12-18.PDF", width = 8.5, height = 11)

par(mfrow=c(4,2))

year.loop <- c("10","11","12","13")

for (i in 1:length(year.loop)) {
  
  thermocline.year<- subset(average_temp_depth, average_temp_depth$Year == year.loop[i])
  
  cast.tang.year <- subset(cast.tang, cast.tang$Year == paste0("20",year.loop[i]))
  
  # Interpolate:
  heatmap.data.temp <- interp(x = as.Date(cast.tang.year$Date2), y = -(cast.tang.year$Depth), z = cast.tang.year$Temp, duplicate = "strip")
  
  unique.dates.temp <- unique(cast.tang.year$Date2)
  
  unique.dates <- unique(cast.tang.year$Date2)

  # plot
  image.plot(heatmap.data.temp, axes = F, col = viridis(20),zlim=c(24,28))
  axis(side = 1, at= unique.dates, col = "black", labels=format(unique.dates, "%m-%d"), las=2, cex.axis=1)
  axis(side = 2, at = seq(from = -200, to = 0, by = 50), labels = T, las=2, cex.axis=1)
  title(paste0("20",year.loop[i]), ylab="Depth(m)", cex.axis=1)
  points(x=thermocline.year$Date2, y=-(thermocline.year$thermocline))
  
  ## DO:
  heatmap.data.do <- interp(x = as.Date(cast.tang.year$Date2), y = -(cast.tang.year$Depth), z = cast.tang.year$DO, duplicate = "strip")

  # plot
  image.plot(heatmap.data.do, axes = F, col = plasma(20),zlim=c(0,100))
  axis(side = 1, at= unique.dates, col = "black", labels= format(unique.dates, "%m-%d"), las=2, cex.axis=1)
  axis(side = 2, at = seq(from = -200, to = 0, by = 50), labels = T, las=2, cex.axis=1)
  title(paste0("20",year.loop[i]), ylab="Depth(m)", cex.axis=1)
  points(x=thermocline.year$Date2, y=-(thermocline.year$thermocline))
  
  
}

dev.off()


# Mean secchi depth per month:
secchi.tang %>% group_by(Month) %>% summarize(mean(Secchi.Depth))
