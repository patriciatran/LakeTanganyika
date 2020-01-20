# Patricia Tran
# Environmental Data from Lake Tanganyika
# Data from Benjamin Kraemer
# October 1, 2019

cast.tang <- read.csv("~/Documents/Github/LakeTanganyika/Figure1-S1-S2-Environmentaldata/LT_CASTS_v4.csv")
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
  theme_classic()

DO

Temp <- ggplot(cast.tang) + geom_point(aes(x = Temp, y = Depth, colour = julianday)) + 
  scale_y_reverse()+
  facet_grid(cols = vars(Year))+
  ggtitle("Temperature")+
  theme_classic()

Chla <- ggplot(cast.tang) + geom_point(aes(x = Chla, y = Depth, colour = julianday)) + 
  scale_y_reverse()+
  facet_grid(cols = vars(Year))+
  ggtitle("Chl a")+
  theme_classic()

Chla

Conductivity <- ggplot(cast.tang) + geom_point(aes(x = Conductivity, y = Depth, colour = julianday)) + 
  scale_y_reverse()+
  facet_grid(cols = vars(Year))+
  ggtitle("Conductivity")+
  theme_classic()

str(cast.tang)
summary(cast.tang)

library(ggpubr)
ggarrange(DO, Temp, Chla, Conductivity, ncol = 1, nrow = 4)


cast.tang$Date

## SECCHI DEPTH:
secchi.tang <- read.csv("~/Downloads/LT_Secchi_v1.csv")
secchi.tang$LatLong <- paste(secchi.tang$Latitude,secchi.tang$Longitude,sep=",")
ggplot(secchi.tang) + geom_point(aes(x = Date, y = Secchi.Depth)) + 
  scale_y_reverse()+
  #facet_grid(cols = vars(secchi.tang$LatLong))+
  ggtitle("Secchi Depths in Lake Tanganyika")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Secchi Depth in meters")
  
secchi.tang$kd <- 1.7/secchi.tang$Secchi.Depth

boxplot(secchi.tang$kd)

str(secchi.tang)

ggplot(secchi.tang) + geom_boxplot(aes(x=as.factor(Month), y=c(Secchi.Depth)))

ggplot(secchi.tang) + geom_boxplot(aes(x=as.factor(Month), y=kd))

library(rLakeAnalyzer)
thermo.depth(cast.tang$Temp, cast.tang$Depth)

library(dplyr)
average_temp_depth <- cast.tang %>% group_by(Date, Depth) %>% summarise(average=mean(Temp))
average_temp_depth <- average_temp_depth %>% mutate(thermocline = thermo.depth(average, Depth))
plot(average_temp_depth$Date, average_temp_depth$thermocline)
 
library(ggplot2)
#Plot Thermocline Depth
library(tidyr)
# Split the date into 3 columns:
average_temp_depth <- average_temp_depth %>% separate(Date, c("Day","Month","Year"), remove=FALSE)

library(lubridate)
average_temp_depth$Date2 <- dmy(average_temp_depth$Date)
average_temp_depth


ggplot(average_temp_depth) + geom_line(aes(Date2, thermocline))+
  #facet_grid(cols = vars(Year))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0,-150)+
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

ggplot(average_o_depth) + geom_line(aes(Date2, oxycline))+
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

## Plotting DO contour plot

#index <- which(ysi$Year == input$chosen.year)
#heatmap.data <- ysi[index, c(1,10,7)]
#valid.indexes = !is.na(heatmap.data$DO.mg.L)
#unique.dates <- decimal_date(unique(heatmap.data$sample.date))
#heatmap.data$sample.date <- decimal_date(heatmap.data$sample.date)
library(akima)

cast.tang.2013 <- subset(cast.tang, cast.tang$Year == "2013")


heatmap.data <- interp(x = cast.tang.2013$Date2, y = -(cast.tang.2013$Depth), z = cast.tang.2013$DO, duplicate = "strip")

par(mar = c(4.5,3,2,0.5))
library(fields)
#install.packages("viridis")
library(viridis)

unique.dates <- unique(cast.tang.2013$Date2)

image.plot(heatmap.data, axes = F, col = viridis(20),zlim=c(0,100))
axis(side = 1, at= unique.dates, col = "black", labels=unique.dates, las=0.5)
axis(side = 2, at = seq(from = -200, to = 0, by = 5), labels = T)
title("Dissolved oxygen (% saturation) in Lake Tanganyika in 2013")
points(x=thermocline.2013$Date2, y=-(thermocline.2013$thermocline))

## Thermocline contour plot:
heatmap.data.temp <- interp(x = as.Date(cast.tang.2013$Date2), y = -(cast.tang.2013$Depth), z = cast.tang.2013$Temp, duplicate = "strip")
par(mar = c(4.5,3,2,0.5))
unique.dates.temp <- unique(cast.tang.2013$Date2)

image.plot(heatmap.data.temp, axes = F, col = viridis(20),zlim=c(24,28))
axis(side = 1, at= unique.dates, col = "black", labels=unique.dates, las=0.5)
axis(side = 2, at = seq(from = -200, to = 0, by = 5), labels = T)
title("Temperature (degrees celcius) in Lake Tanganyika in 2013")
# Add a line for the thermocline
thermocline.2013<- subset(average_temp_depth, average_temp_depth$Year == "13")


points(x=thermocline.2013$Date2, y=-(thermocline.2013$thermocline))

# Mean secchi depth per month:
secchi.tang %>% group_by(Month) %>% summarize(mean(Secchi.Depth))

## After reviewer comments I'm attempting to convert prevous Figure S1 into a contour plot.
## 2010
cast.tang.2010 <- subset(cast.tang, cast.tang$Year == "2010")
thermocline.2010<- subset(average_temp_depth, average_temp_depth$Year == 10)

heatmap.data <- interp(x = cast.tang.2010$Date2, y = -(cast.tang.2010$Depth), z = cast.tang.2010$DO, duplicate = "strip")

par(mar = c(4.5,3,2,0.5))

unique.dates <- unique(cast.tang.2010$Date2)

image.plot(heatmap.data, axes = F, col = viridis(20),zlim=c(0,100))
axis(side = 1, at= unique.dates, col = "black", labels=unique.dates, las=0.5)
axis(side = 2, at = seq(from = -200, to = 0, by = 5), labels = T)
title("Dissolved oxygen (% saturation) in Lake Tanganyika in 2010")
points(x=thermocline.2010$Date2, y=-(thermocline.2010$thermocline))

## Thermocline contour plot:
heatmap.data.temp <- interp(x = as.Date(cast.tang.2010$Date2), y = -(cast.tang.2010$Depth), z = cast.tang.2010$Temp, duplicate = "strip")
par(mar = c(4.5,3,2,0.5))
unique.dates.temp <- unique(cast.tang.2010$Date2)

image.plot(heatmap.data.temp, axes = F, col = viridis(20),zlim=c(24,28))
axis(side = 1, at= unique.dates, col = "black", labels=unique.dates, las=0.5)
axis(side = 2, at = seq(from = -200, to = 0, by = 5), labels = T)
title("Temperature (degrees celcius) in Lake Tanganyika in 2010")
# Add a line for the thermocline
points(x=thermocline.2010$Date2, y=-(thermocline.2010$thermocline))

## 2011
cast.tang.2011 <- subset(cast.tang, cast.tang$Year == "2011")
thermocline.2011<- subset(average_temp_depth, average_temp_depth$Year == 11)

heatmap.data <- interp(x = cast.tang.2011$Date2, y = -(cast.tang.2011$Depth), z = cast.tang.2011$DO, duplicate = "strip")

par(mar = c(4.5,3,2,0.5))

unique.dates <- unique(cast.tang.2011$Date2)

image.plot(heatmap.data, axes = F, col = viridis(20),zlim=c(0,100))
axis(side = 1, at= unique.dates, col = "black", labels=unique.dates, las=0.5)
axis(side = 2, at = seq(from = -200, to = 0, by = 5), labels = T)
title("Dissolved oxygen (% saturation) in Lake Tanganyika in 2011")
points(x=thermocline.2011$Date2, y=-(thermocline.2011$thermocline))

## Thermocline contour plot:
heatmap.data.temp <- interp(x = as.Date(cast.tang.2011$Date2), y = -(cast.tang.2011$Depth), z = cast.tang.2011$Temp, duplicate = "strip")
par(mar = c(4.5,3,2,0.5))
unique.dates.temp <- unique(cast.tang.2011$Date2)

image.plot(heatmap.data.temp, axes = F, col = viridis(20),zlim=c(24,28))
axis(side = 1, at= unique.dates, col = "black", labels=unique.dates, las=0.5)
axis(side = 2, at = seq(from = -200, to = 0, by = 5), labels = T)
title("Temperature (degrees celcius) in Lake Tanganyika in 2011")
# Add a line for the thermocline
points(x=thermocline.2011$Date2, y=-(thermocline.2011$thermocline))

## 2012
cast.tang.2012 <- subset(cast.tang, cast.tang$Year == "2012")
thermocline.2012<- subset(average_temp_depth, average_temp_depth$Year == 12)

heatmap.data <- interp(x = cast.tang.2012$Date2, y = -(cast.tang.2012$Depth), z = cast.tang.2012$DO, duplicate = "strip")

par(mar = c(4.5,3,2,0.5))

unique.dates <- unique(cast.tang.2012$Date2)

image.plot(heatmap.data, axes = F, col = viridis(20),zlim=c(0,100))
axis(side = 1, at= unique.dates, col = "black", labels=unique.dates, las=0.5)
axis(side = 2, at = seq(from = -200, to = 0, by = 5), labels = T)
title("Dissolved oxygen (% saturation) in Lake Tanganyika in 2012")
points(x=thermocline.2012$Date2, y=-(thermocline.2012$thermocline))

## Thermocline contour plot:
heatmap.data.temp <- interp(x = as.Date(cast.tang.2012$Date2), y = -(cast.tang.2012$Depth), z = cast.tang.2012$Temp, duplicate = "strip")
par(mar = c(4.5,3,2,0.5))
unique.dates.temp <- unique(cast.tang.2012$Date2)

image.plot(heatmap.data.temp, axes = F, col = viridis(20),zlim=c(24,28))
axis(side = 1, at= unique.dates, col = "black", labels=unique.dates, las=0.5)
axis(side = 2, at = seq(from = -200, to = 0, by = 5), labels = T)
title("Temperature (degrees celcius) in Lake Tanganyika in 2012")
# Add a line for the thermocline
points(x=thermocline.2012$Date2, y=-(thermocline.2012$thermocline))

## Chl a contour plots: 2013
# Remove NA:
cast.tang.2013 <- cast.tang.2013 %>% filter(!is.na(Chla))

heatmap.data.chla <- interp(x = as.Date(cast.tang.2013$Date2), y = -(cast.tang.2013$Depth), z = cast.tang.2013$Chla, duplicate = "strip")
par(mar = c(4.5,3,2,0.5))
unique.dates.temp <- unique(cast.tang.2013$Date2)

image.plot(heatmap.data.chla, axes = F, col = viridis(20))
axis(side = 1, at= unique.dates, col = "black", labels=unique.dates, las=0.5)
axis(side = 2, at = seq(from = -200, to = 0, by = 5), labels = T)
title("Chlorophyll a in Lake Tanganyika in 2013")
# Add a line for the thermocline
points(x=thermocline.2013$Date2, y=-(thermocline.2013$thermocline))
