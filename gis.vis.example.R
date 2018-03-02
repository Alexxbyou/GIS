source("gis.function.R")
demo.gis<-readRDS("data/demo.gis.RDS")
demo.gis<-demo.gis[!is.na(demo.gis$DGP),]
PA.stats<-demo.gis%>%
  group_by(DGP)%>%
  summarize(
    n=length(Death),
    outcome=sum(Death,na.rm=T),
    out.rate=mean(Death,na.rm=T)*100
  )
PA.stats<-left_join(PA.stats,PA.resid@data[,c("PLN_AREA_N","TOTAL")],by=c("DGP"="PLN_AREA_N"))
PA.stats$recruit.rate<-PA.stats$n/PA.stats$TOTAL

loc.sum<-demo.gis%>%
  group_by(x,y)%>%
  summarize(
    n=length(Death)
  )

#############################################
# Scatter plots
#
data1<-demo.gis[,c(3:4,1)]
data1$Death[data1$Death==1]<-"Yes"
data1$Death[data1$Death!="Yes"]<-"No"
add.point(map.canvas1,demo.gis[,3:4],"gray30")%>%
  add.labs(title="Cohort Distribution with Canvas 1")
add.point(map.canvas2,demo.gis[,3:4])%>%
  add.labs(title="Cohort Distribution with Canvas 2",
           subtitle="Visualization over PA.stat",
           caption="Cohort Distribution with Canvas 2")


#############################################
# distribution by group
add.point(map.canvas1,data1)%>%
  add.labs(title="Cohort Distribution with Canvas 2",
           subtitle="Visualization over PA.stat",
           caption="Cohort Distribution with Canvas 2")
add.point(map.canvas2,data1)%>%
  add.labs(title="Cohort Distribution with Canvas 2",
           subtitle="Visualization over PA.stat",
           caption="Cohort Distribution with Canvas 2")



#############################################
# 2D histogram by PA
Pa.stat.data<-PA.stats[order(PA.stats$n,decreasing=T)[1:20],c(1,4)]

PA.stats.vis(Pa.stat.data,"Mortality Rate (%)")%>%
  add.labs(title="Mortality Rate by Planning Area",
           subtitle="Visualization over PA.stat",
           caption="Mortality Rate by Planning Area with Canvas 2 by default")










