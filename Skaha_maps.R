----------------------------------------------------------------------------------
#load them libraries yo
library(maptools)
library(rgdal)
library(raster)
library(maps)
library(mapdata)
library(ggmap)
library(marmap)
library(lattice)
library(methods)
library(sf)
library(ggspatial)
library(ggplot2)
library(rgeos)
library(GEOmap) 

setwd('B:/Mainframe_Russello/Room_temp/Playground/2_Skaha_500Panel_Journey/ITSMAPTIME')
#download the Canada spatial data at the province level
Canada<- getData('GADM', country="CAN", level=1)

#convert data to a shapefile so you can plot it w ggplot 
Canada_sf<-st_as_sf(Canada)

#download the Canada spatial data at thehighest level (only plots countries)
Canada_0 <- getData('GADM', country="CAN", level=0)

#convert data to a shapefile so you can plot it w ggplot 
Canada_sf_0<-st_as_sf(Canada_0)

#subset the Canada data into BC data
BC <- Canada[Canada$NAME_1=="British Columbia",]

#convert BC data to shapefile
BC_sf = st_as_sf(BC)

#Canada river data in Documents file as shapefile
#BCwater <- st_read("shapefile/British_Columbia_Water/british_columbia_Water.shp")
#BCwater <- st_read("shapefile/BC_Basins_GoogleMapPL/BC_Basins_GoogleMapPL.shp")
#BCwater <- st_read("shapefile/freshwater/lwm9bc.shp")
#crs(BCwater) <- CRS("+proj=longlat +datum=WGS84")
#crs(BCwater)
#st_set_crs(BCwater)
BCwater <- st_read("shapefile/freshwater/lwslbcgz.shp")
BCwater <- na.omit(BCwater)
BCnatural <- st_read("shapefile/british_columbia_natural/british_columbia_natural.shp")
#lolwhateven
Washwater <-st_read("shapefile/DNR_Hydrography_-_Water_Bodies_-_Forest_Practices_Regulation/DNR_Hydrography_-_Water_Bodies_-_Forest_Practices_Regulation.shp")


#pour yourself a tall glass of assorted waters
#https://data-wadnr.opendata.arcgis.com/datasets/28a0f93c33454297b4a9d3faf3da552a_1?geometry=-119.707%2C48.932%2C-119.053%2C49.011

Okanagan <- BCwater[BCwater$GAZE_NAME == "OKANAGAN LAKE",]
Okanagan <- na.omit(Okanagan)
OkanaganR <- BCwater[BCwater$GAZE_NAME == "OKANAGAN RIVER",]
OkanaganR <- na.omit(OkanaganR)
PentictonCr <- BCwater[BCwater$GAZE_NAME == "PENTICTON CREEK",]
Skaha <- BCwater[BCwater$GAZE_NAME == "SKAHA LAKE",]
Skaha <- na.omit(Skaha)
Vaseux <- BCwater[BCwater$GAZE_NAME == "VASEUX LAKE",]
Vaseux <- na.omit(Vaseux)
Osoyoos <- BCwater[BCwater$GAZE_NAME == "OSOYOOS LAKE",]
Osoyoos <- na.omit(Osoyoos)
WashOsoyoos <- Washwater[Washwater$OBJECTID== "123",]
WashOKR <- Washwater[Washwater$OBJECTID== "756",]

#combine data
Lakes_0<-rbind(Okanagan, Skaha, Vaseux, Osoyoos)
BCRiver <- rbind(OkanaganR, PentictonCr)
Washlake <- rbind(WashOsoyoos)
WashRiver <- rbind(WashOKR)


#extent of coordinates of shapefile for river
#st_bbox(BCwater)

#subset the Canada data into Yukon data
#YK<- Canada[Canada$NAME_1=="Yukon",]

#convert Yukon data into shapefile
#YK_sf<- st_as_sf(YK)

#donwload USA spatial data at the state level
US<-getData('GADM', country='usa', level=1)

#convert to shape file
US_sf<-st_as_sf(US)

#download spatial data at highest (country) level
US_0<-getData('GADM', country='usa', level=0)

#convert to shapefle
US_sf_0<-st_as_sf(US_0)

#subset into Washington state data
WA<- US[US$NAME_1=="Washington",]

#convert
WA_sf<- st_as_sf(WA)

#subset to Alaska data
#AK<-US[US$NAME_1=="Alaska",]

#convert
#AK_sf<-st_as_sf(AK)

#combine data into a North America set
NAmerica_0<-rbind(Canada_sf_0,US_sf_0)

#explicity pull out geometric data
NAmerica.geo_0<-NAmerica_0$geometry

#combine data into a North West N.A. set
NW<-rbind(BC_sf,WA_sf)


#explicity pull out geometric data
NW.geo<-NW$geometry

#create plot of Wash, BC data, filled in grey, with light black outline. Only keep certain area of the plot and add a scale bar
plott<-ggplot(data=NW.geo)+
  geom_sf(fill="grey79", col="black",size= 0.2) +
  geom_sf(data = Lakes_0, color = "blue", size = 0.35) +
  geom_sf(data = BCRiver, color = "blue", size = 0.4) +
  geom_sf(data = Washlake, color = "blue", size = 0.35) +
  geom_sf(data = WashRiver, color = "blue", size = 0.4) +
  coord_sf(xlim=c(-120,-119),ylim=c(48.95,50.4)) +
  geom_point(aes(x = -139.5, y = 60.75),cex=2.85, pch=15, fill="black") +
  theme(panel.background=element_rect(fill= "white"),panel.grid.major = element_line(colour="white"),panel.grid.minor=element_line(colour="white"),axis.title= element_blank())+
  annotation_scale(style='ticks',pad_x = unit(0.10, "in"), pad_y= unit(0.10, "in"),text_cex = 0.7) 
  


#create plot of Canada and US that is grey, outlined in black
NAA<-ggplot(data=NAmerica.geo_0)+
  geom_sf(fill="grey79", col="black", size=0.2)+
  coord_sf(xlim = c(-170,-40)) +
  theme(panel.background=element_rect(fill= "white"),panel.grid.major = element_line(colour="white"),panel.grid.minor=element_line(colour="white"))

#plot Northwest plot as a hi-res jpeg
jpeg("Skaha.jpeg", width =10, height =12, units='in', res=1000)
plot(plott)
dev.off()

#plot North america plot as a hi-res jpeg
jpeg("NA_master.jpeg", width =5, height =6, units='in', res=1000)
plot(NAA)
dev.off()
