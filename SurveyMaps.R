setwd("~/Documents/UMBC/MDSG /Surveys")

library(readxl)

SurveyLocations <- read_xlsx("~/Documents/UMBC/MDSG /Surveys/SurveyLocations.xlsx")
SurveyLocations

library("tmap")
library("tmaptools")
library("sf")
library("leaflet")


library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)


## plotting chesapeake bay - found online ##
## URL : https://rpubs.com/narinderps1991/ChesapeakeBay ##
library("rnaturalearth")
library("rnaturalearthdata")
library(ggspatial)

### Perkinsus Sites ###
worldMap <- ne_countries(scale = "medium", returnclass = "sf")
plot<-ggplot(data = worldMap) +geom_sf(fill="gray90") + coord_sf(xlim = c(-77, -75.5), ylim = c(38, 39),expand = TRUE)+xlab("Longitude") +ylab("Latitude")

plot<-plot+theme(panel.background = element_rect(fill = "white")) 

plot <-plot+annotate(geom = "text",x = -76.1,y = 38,label = "Chesapeake Bay",color = "grey",size = 4, angle=90, fontface = "italic")
plot
plot<-plot +annotation_north_arrow(location = "bl",pad_x = unit(0.5, "cm"),pad_y = unit(1, "cm"),height=unit(1,"cm"),width=unit(0.5,"cm")) 
plot  
plot<- plot+theme(panel.grid.major = element_line(linetype = "dashed",color = "dark grey" ,size = 0.2))
plot

SurveyLocations$Lat <- as.numeric(SurveyLocations$Lat)
SurveyLocations$Long <- as.numeric(SurveyLocations$Long)


plot <- plot+geom_point(data = SurveyLocations,aes(Long, Lat, shape = Sites), size = 5)
plot + annotation_scale()+theme(axis.text=element_text(size=10),
                                axis.title=element_text(size=14,face="bold"), legend.text=element_text(size = 10))


worldMap <- ne_countries(scale = "medium", returnclass = "sf")
plot2<-ggplot(data = worldMap) +geom_sf(fill="gray90") + coord_sf(xlim = c(-77.5, -75.5), ylim = c(37, 39.5),expand = TRUE)

plot2<-plot2+theme(panel.background = element_rect(fill = "white")) 

plot2 <-plot2+annotate(geom = "text",x = -76.1,y = 37.6,label = "Chesapeake Bay",color = "grey",size = 4, angle=90, fontface = "italic")
plot2
plot2<- plot2+theme(panel.grid.major = element_line(linetype = "dashed",color = "dark grey" ,size = 0.2))
plot2

######### Temp & salinity graphs #########
library(ggplot2)
library(ggpubr)

EnvData <- read_xlsx("~/Documents/UMBC/MDSG /Surveys/TempSal_Surv.xlsx")
EnvData


plot1<- ggplot(EnvData, aes(Month, Temperature)) + geom_point(aes(colour=factor(Site)), size= 2.5)+
  ylim(15,30) + xlim(c("June","July","August")) + guides(color = guide_legend(title = "Site"))+
  theme_linedraw() +theme(axis.text=element_text(size=15),
                          axis.title=element_text(size=20,face="bold"))
plot1

plot2<- ggplot(EnvData, aes(Month, Salinity)) + geom_point(aes(colour=factor(Site)), size= 2.5)+
  ylim(5,20) + xlim(c("June","July","August")) + guides(color = guide_legend(title = "Site"))+
  theme_linedraw() +theme(axis.text=element_text(size=15),
                          axis.title=element_text(size=20,face="bold"))
plot2
