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



gis.PA.stat.cal<-function(
  gis.data   # data.frame(outcome,PA)
){
  names(gis.data)<-c("outcome","PA")
  gis.data<-gis.data[!is.na(gis.data$PA),]
  PA.stats<-gis.data%>%
    group_by(PA)%>%
    summarize(
      n=length(outcome),
      outcome=sum(outcome,na.rm=T)
    )
  PA.stats<-left_join(PA.stats,PA.resid@data[,c("PLN_AREA_N","TOTAL")],by=c("PA"="PLN_AREA_N"))
  PA.stats$out.rate<-PA.stats$outcome/PA.stats$n
  PA.stats$recruit.rate<-PA.stats$n/PA.stats$TOTAL
  return(PA.stats)
}




















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



