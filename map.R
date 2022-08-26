setwd("E:/KnowSandeel15781/Data")

######################
###   make maps    ###
######################
library("rnaturalearth")
library("rnaturalearthdata")
library("sf")
library("ggplot2")
library("rnaturalearthhires")
library("ggspatial") #for scale bar
lapply(c("gps.Rdata",  "spdf.Rdata", "ctd_EROS.Rdata", 'EROS.Rdata'),load,.GlobalEnv)


# zoom-out map #
library("ggthemes") # for theme_map
w <- ne_countries(scale = "large", returnclass = "sf")
ggplot(data = w) + geom_sf() + 
  theme_bw(base_size=15) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank())+
  #theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA))+
  coord_sf(xlim = c(-5, 9), ylim = c(54,62.3), expand = FALSE, datum = NA)+ #
  annotate(geom = "text", x = 7.6, y = 60, label = "Norway", fontface = "bold", color = "grey80", size = 7)+
  annotate(geom = "text", x = -4, y = 57.2, label = "UK", fontface = "bold", color = "grey80", size = 7)+
  geom_polygon(data = spdf, aes(long, lat, group=area), colour="black", fill="red", alpha =0.2) + 
  #geom_point(data=sta.ind, aes(x=Longitude, y=Latitude), size=1, shape=1, col="blue")+
  #geom_point(data=School_EROS_bio.df, aes(x=Longitude, y=Latitude), size=1, shape=1, col="blue", stroke=2)+
  #geom_point(data=sandeel.dt[!area %in% "outside"], aes(x=Longitude, y=Latitude, col=area), size=1, shape=1)+
  #geom_point(data=sandeel.dt[area %in% "outside"], aes(x=Longitude, y=Latitude), size=1, shape=1, col="black")+
  geom_point(data=ctd_EROS.dt, aes(x=Longitude, y=Latitude), size=1, col='red')+
  #geom_path(data=gps.dt, aes(x=Longitude, y=Latitude, colour=vessel), size=.1)+ theme(legend.position = "none", axis.title = element_blank())+
  labs(x="Longitude", y="Latitude")
#
#
p<-ggplot(data = w) + geom_sf() + theme_bw(base_size=15) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA))+
  coord_sf(xlim = c(-5, 9), ylim = c(54,62.3), expand = FALSE, datum = NA)+ #
  annotate(geom = "text", x = 7.6, y = 60, label = "Norway", fontface = "bold", color = "grey80", size = 7)+
  annotate(geom = "text", x = -4, y = 57.2, label = "UK", fontface = "bold", color = "grey80", size = 7)+
  #geom_path(data=sd1031.df, aes(x=Longitude, y=Latitude), size=.1, colour="blue")+ theme(legend.position = "none", axis.title = element_blank())+
  #geom_path(data=sd1032.df, aes(x=Longitude, y=Latitude), size=.1, colour="green")+
  geom_path(data=subset(EROS.df, area ==  "Engelsk_Klondyke"), aes(x=Longitude, y=Latitude), size=.1, colour="red") +
  theme(axis.title = element_blank())#+labs(x="Longitude", y="Latitude")
#
#ggsave(p, filename = "map2SD.png",  bg = "transparent")
#

