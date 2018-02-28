library(ggplot2)
library (rgdal)
library (rgeos)
library(maptools)
library(viridis)
library(ggthemes)
library(dplyr)
library(scales)

PGdata <- readOGR("shapefiles/subzone-census-2010/subzone-census2010-shp/Subzone_Census2010.shp")  
PG <- readOGR("shapefiles/subzone-census-2010/subzone-census2010-kml/Subzone_Census2010.kml")   
AG <- fortify(PG)

brk<-(max(PGdata@data$SHAPE_Area)+1)*c(0,5,10,20,50,100)/100

PGdata@data$id<-row.names(PGdata@data)

AG<-left_join(AG,PGdata@data[,c("id","SHAPE_Area")])


##############################################################################
#
##############################################################################
PA.kml<-readOGR("shapefiles/PlanningArea2010/kml/Planning_Area_Census2010.kml")
PA.shp<-readOGR("shapefiles/PlanningArea2010/shp/Planning_Area_Census2010.shp")
SZ.kml<-readOGR("shapefiles/Subzone2010/kml/Subzone_Census2010.kml")
SZ.shp<-readOGR("shapefiles/Subzone2010/shp/Subzone_Census2010.shp")


PA.shp<-PA.shp%>%spTransform(CRS("+proj=longlat"))
PA.fort<-fortify(PA.shp)

map<-get_map(location=c(103.8,1.36),maptype = "roadmap", source = "google", zoom = 12)
ggmap(map) + 
  geom_polygon(data = PA.fort, aes(long, lat, fill = group), 
               , colour="white", alpha=0.5)+scale_fill_discrete(guide=F)



map<-get_map(location=c(103.8,1.36),maptype = "roadmap", source = "osm", zoom = 11)
ggmap(map) + 
  geom_polygon(data = AG, aes(long, lat, group = group, fill=SHAPE_Area), 
               , colour="white", alpha=0.2) + coord_equal() + 
  scale_fill_viridis(trans = "log", breaks=brk, name="Number of recidence", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "Title",
    subtitle = "Subtitle", 
    caption = "HSOR@NHG"
  )+
  theme(
    #axis.text = element_blank(),
    axis.title=element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    text = element_text(color = "gray"), 
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.7, 0.09)
  )
  
map <- get_map(location = c(lon[1], lat[2], lon[2], lat[1]),
               maptype = "roadmap", source = "google", zoom = 9)

library(proj4)
proj4string <- "+ellps=WGS84"

# Source data
xy <- data.frame(x=142642.66, y=142642.66)

# Transformed data
pj <- proj4::project(xy, proj4string, inverse=TRUE)
latlon <- data.frame(lat=pj$y, lon=pj$x)
print(latlon)



utm2latlong<-function(df){
  names(df)<-c("X","Y")
  temp<-SpatialPoints(df, proj4string=CRS("+proj=utm +zone=48N"))
  spTransform(temp,CRS("+proj=longlat"))
}

head(AG)

AG[,1:2]<-as.data.frame(utm2latlong(AG[,1:2]))
