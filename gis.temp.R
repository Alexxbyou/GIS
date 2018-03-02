library(ggplot2)
library (rgdal)
library (rgeos)
library(maptools)
library(viridis)
library(ggthemes)
library(dplyr)
library(scales)
library(RODBC)
rhs<-odbcConnect("rhs")

postal<-sqlQuery(rhs,"SELECT * FROM [RHS_Workspace].[dbo].[Singapore_location_complete_2016_Apr]")
saveRDS(postal,"shapefiles/postal.RDS")

PGdata <- readOGR("shapefiles/subzone-census-2010/subzone-census2010-shp/Subzone_Census2010.shp")  
PG <- readOGR("shapefiles/subzone-census-2010/subzone-census2010-kml/Subzone_Census2010.kml")   
AG <- fortify(PG)

brk<-(max(PGdata@data$SHAPE_Area)+1)*c(0,5,10,20,50,100)/100

PGdata@data$id<-row.names(PGdata@data)

AG<-left_join(AG,PGdata@data[,c("id","SHAPE_Area")])


##############################################################################
#
##############################################################################
#PA.kml<-readOGR("shapefiles/PlanningArea2010/kml/Planning_Area_Census2010.kml")
PA.shp<-readOGR("shapefiles/PlanningArea2010/shp/Planning_Area_Census2010.shp")%>%spTransform(CRS("+proj=longlat"))
#SZ.kml<-readOGR("shapefiles/Subzone2010/kml/Subzone_Census2010.kml")
SZ.shp<-readOGR("shapefiles/Subzone2010/shp/Subzone_Census2010.shp")%>%spTransform(CRS("+proj=longlat"))


PA.shp<-PA.shp%>%spTransform(CRS("+proj=longlat"))
PA.fort<-fortify(PA.shp)

map<-get_map(location=c(103.833,1.36),maptype = "roadmap", source = "google", zoom = 11)
ggmap(map, extent = 'panel') + 
  geom_polygon(data = PA.fort, aes(long, lat, fill = group),colour="white", alpha=0.5)+scale_fill_discrete(guide=F)



map<-get_map(location=c(103.8,1.366),maptype = "roadmap", source = "osm", zoom = 11)
ggmap(map) + 
  geom_polygon(data = AG, aes(long, lat, group = group, fill=SHAPE_Area), 
               colour="white", alpha=0.2) + coord_equal() + 
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




map.canvas2+geom_point()





ggplot()+
  geom_polygon(data = AG, aes(long, lat, group = group, fill=SHAPE_Area), 
               colour="white", alpha=0.2) +
  theme(
    axis.text = element_blank(),
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


#map<-get_map(location=c(103.8,1.366),maptype = "roadmap", source = "osm", zoom = 11)
ggmap(map) + 
  geom_polygon(data = AG, aes(long, lat, group = group, fill=SHAPE_Area), 
               colour="white", alpha=0.2) + coord_equal() + 
  scale_fill_viridis(trans = "log", breaks=brk, name="Number of recidence", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "Title",
    subtitle = "Subtitle", 
    caption = "HSOR@NHG"
  )+
  theme(
    axis.text = element_blank(),
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







library(ggplot2)
library (rgdal)
library (rgeos)
library(maptools)
library(viridis)
library(ggthemes)
library(dplyr)
library(scales)
library(RODBC)
rhs<-odbcConnect("rhs")


PA.resid<-readRDS("data/PA.resid.RDS")
SZ.resid<-readRDS("data/SZ.resid.RDS")
demo.gis<-readRDS("data/demo.gis.RDS")

demo.gis<-demo.gis[!is.na(demo.gis$DGP),]
PA.stats<-demo.gis%>%
  group_by(DGP)%>%
  summarize(
    n=length(Death),
    outcome=sum(Death,na.rm=T),
    out.rate=mean(Death,na.rm=T)
  )
PA.stats<-left_join(PA.stats,PA.resid@data[,c("PLN_AREA_N","TOTAL")],by=c("DGP"="PLN_AREA_N"))
PA.stats$recruit.rate<-PA.stats$n/PA.stats$TOTAL

loc.sum<-demo.gis%>%
  group_by(x,y)%>%
  summarize(
    n=length(Death)
  )




SZ.fort<-fortify(SZ.resid)
PA.fort<-fortify(PA.resid)
SZ.resid.data<-SZ.resid@data
SZ.resid.data$id<-row.names(SZ.resid.data)
PA.resid.data<-PA.resid@data
PA.resid.data$id<-row.names(PA.resid.data)
PA.resid.data$ColorCD<-runif(nrow(PA.resid.data))
SZ.resid.data$ColorCD<-runif(nrow(SZ.resid.data))
SZ.fort<-left_join(SZ.fort,SZ.resid.data[,c("id","ColorCD")])
PA.fort<-left_join(PA.fort,PA.resid.data[,c("id","ColorCD")])


map<-get_map(location=c(103.82,1.36),maptype = "roadmap", source = "google", zoom = 11)
ggmap(map)



theme.setting1<-ggplot()+theme_void() +coord_cartesian()+
  theme(
    #axis.text = element_blank(),
    axis.title=element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    text = element_text(color = "black"), 
    plot.background = element_rect(fill = "#738F99", color = NA), 
    panel.background = element_rect(fill = "#738F99", color = NA), 
    legend.background = element_rect(fill = "#738F99", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.7, 0.09)
  )


# Map canvas
map.canvas<-theme.setting1 + 
  geom_polygon(data = PA.fort, aes(long, lat,group=group, fill=ColorCD),colour="white",size =.8, alpha=0.8)+
  geom_path(data = SZ.fort, aes(long, lat,group=group),colour="white", size=.1)+scale_fill_viridis(guide=F)

# Scatter plot
map.canvas+geom_point(data = loc.sum, aes(x,y,colour=n),alpha=.3)+scale_color_continuous(guide=F)



map.canvas2<-theme.setting1 + 
  geom_path(data = PA.fort, aes(long, lat,group=group),colour="white",size =.8)+
  geom_path(data = SZ.fort, aes(long, lat,group=group),colour="white", size=.1)


map.canvas2+geom_bin2d(data = demo.gis, aes(x,y),bins=150,alpha=0.7)+scale_color_continuous(guide=F)

map.canvas2+scale_color_continuous(guide=F)+stat_density_2d(data = demo.gis,aes(x,y,fill = ..level..),n=100, geom = "polygon", colour="white")



map<-get_map(location=c(103.82,1.36),maptype = "roadmap", source = "google", zoom = 11)

theme.setting2<-ggmap(map)+theme_void() +coord_cartesian()+
  theme(
    #axis.text = element_blank(),
    axis.title=element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    text = element_text(color = "black"), 
    plot.background = element_rect(fill = "#738F99", color = NA), 
    panel.background = element_rect(fill = "#738F99", color = NA), 
    legend.background = element_rect(fill = "#738F99", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.7, 0.09)
  )


theme.setting2 + geom_bin2d(data = demo.gis, aes(x,y),bins=150,alpha=0.7)+scale_fill_continuous(guide=F)
theme.setting2 + geom_hex(data = demo.gis, aes(x,y),bins=80,colour="gray",size=.6)+scale_fill_continuous(guide=F)


map.canvas2+geom_point(data=demo.gis,mapping=aes(x,y),position="jitter",colour="#00FFFF",alpha=0.5,shape="a",size=3)


data<-demo.gis[,c("x","y","Death")]
names(data)[3]<-"outcome"
data$outcome<-sample(c("Yes","No"),nrow(data),replace=T)








