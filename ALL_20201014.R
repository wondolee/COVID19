Sys.setlocale(locale="English_United Kingdom")
setwd("d:/WORKSPACE/COVID19/REVISE_LAST")

##Importing the average/median radius of gyration by CCG level in England - how many?
ccg.mob.mean<-read.csv("d:/WORKSPACE/COVID19/MODEL/FINAL/ORI_DATA/ccg_mean.csv");colnames(ccg.mob.mean)<-c("CCG19NM","DATE","MEAN")
ccg.mob.median<-read.csv("d:/WORKSPACE/COVID19/MODEL/FINAL/ORI_DATA/ccg_median.csv");colnames(ccg.mob.median)<-c("CCG19NM","DATE","MEDIAN")
require(lubridate)
ccg.mob.mean$DATE<-ymd(ccg.mob.mean$DATE)
ccg.mob.mean$DAY<-format(as.Date(ccg.mob.mean$DATE), "%A")
ccg.mob.mean$WEEK<-week(ccg.mob.mean$DATE)
ccg.mob.median$DATE<-ymd(ccg.mob.median$DATE)
ccg.mob.median$DAY<-format(as.Date(ccg.mob.median$DATE), "%A")
ccg.mob.median$WEEK<-week(ccg.mob.median$DATE)

ccg.mob.mean<-subset(ccg.mob.mean,DATE>="2020-03-03")
ccg.mob.median<-subset(ccg.mob.median,DATE>="2020-03-03")

require(dplyr)
tbl.ccg.mob.mean<- as.data.frame(ccg.mob.mean %>% group_by(CCG19NM) %>% arrange(DATE, .by_group = TRUE) %>% mutate(PER = (MEAN-first(MEAN))/first(MEAN)*100))
tbl.ccg.mob.mean[is.na(tbl.ccg.mob.mean)]<-0
tbl.ccg.mob.median<- as.data.frame(ccg.mob.median %>% group_by(CCG19NM) %>% arrange(DATE, .by_group = TRUE) %>% mutate(PER = (MEDIAN-first(MEDIAN))/first(MEDIAN)*100))
tbl.ccg.mob.median[is.na(tbl.ccg.mob.median)]<-0

require(stringr)
tbl.ccg.mob.mean$CCG19NM<-as.factor(tbl.ccg.mob.mean$CCG19NM)
tbl.ccg.mob.mean$CCG19NM<-gsub("NHS Bristol, North Somerset and South Gloucesters*","NHS Bristol, North Somerset and South Gloucestershire CCG",tbl.ccg.mob.mean$CCG19NM)
tbl.ccg.mob.mean$CCG19NM<-gsub("NHS South East Staffordshire and Seisdon Peninsul*","NHS South East Staffordshire and Seisdon Peninsula CCG",tbl.ccg.mob.mean$CCG19NM)
tbl.ccg.mob.mean$CCG19NM<-str_remove(tbl.ccg.mob.mean$CCG19NM, "[*]")

tbl.ccg.mob.median$CCG19NM<-sub("NHS Bristol, North Somerset and South Gloucesters*","NHS Bristol, North Somerset and South Gloucestershire CCG",tbl.ccg.mob.median$CCG19NM)
tbl.ccg.mob.median$CCG19NM<-sub("NHS South East Staffordshire and Seisdon Peninsul*","NHS South East Staffordshire and Seisdon Peninsula CCG",tbl.ccg.mob.median$CCG19NM)
tbl.ccg.mob.median$CCG19NM<-str_remove(tbl.ccg.mob.median$CCG19NM, "[*]")

#Selective dates: 10 March (PER0) 24 Mar (PER1), 31 Mar (PER2), 7 Apr (PER3), 14 Apr (PER4)
##Updating the ADMIN info.
lsoa.to.ccg<-readRDS("d:/WORKSPACE/COVID19/MODEL/REVISE_20200531/lsoa.to.ccg.rds")
eng.ccg<-subset(lsoa.to.ccg,grepl("E",CCG19CD));eng.ccg<-eng.ccg[c(3:5)];eng.ccg<-distinct(eng.ccg)
require(plyr)
eng.ccg.mob.median<-left_join(eng.ccg,tbl.ccg.mob.median,by="CCG19NM")
sel.ccg.mob.median<-subset(eng.ccg.mob.median,DATE=="2020-03-10" | DATE=="2020-03-24" | DATE=="2020-03-24" | DATE=="2020-03-31" 
                           | DATE=="2020-04-07" | DATE=="2020-04-14")

saveRDS(sel.ccg.mob.median,"dep.20201011.rds")
#sel.mob.median<-readRDS("D:/WORKSPACE/COVID19/MODEL/FINAL/dep.20200727.rds")
#require(reshape2)
#require(lubridate)
sel.ccg.mob.median$DATE<-ymd(sel.ccg.mob.median$DATE)
#require(plyr)
#require(tidyr)
eng.ccg.median<-dcast(sel.ccg.mob.median,CCG19CD+CCG19NM~DATE,fun.aggregate=sum,value.var="PER")
colnames(eng.ccg.median)<-c("CCG19CD","CCG19NM","PER0","PER1","PER2","PER3","PER4")
eng.ccg.median$PER0<-eng.ccg.median$PER0*-1/100
eng.ccg.median$PER1<-eng.ccg.median$PER1*-1/100
eng.ccg.median$PER2<-eng.ccg.median$PER2*-1/100
eng.ccg.median$PER3<-eng.ccg.median$PER3*-1/100
eng.ccg.median$PER4<-eng.ccg.median$PER4*-1/100
saveRDS(eng.ccg.median,"update.dep.20200727.rds")

require(lubridate)
require(tseries)
require(forecast)
require(scales)
require(ggplot2)

daily.median.per<- ggplot(data=eng.ccg.mob.median,aes(x=DATE, y=ts(PER)))+
  geom_line(color="grey20",size=0.02,alpha=0.4)+
  labs(x="Day",y="Mobility reduction compared to 3 March (%)",face="bold")+
  scale_x_date(labels=date_format("%B %d %Y"))+
  #scale_y_continuous(limits=c(-1, 50))+
  geom_smooth(method = "loess",span=0.3)

daily.median.per<-daily.median.per+ theme(legend.text=element_text(family="Raleway"),
  axis.text.x = element_text(color = "black", size = rel(1.5),face="plain"),
  axis.text.y = element_text(color = "black", size = rel(1.5),face="plain"),
  axis.title.x = element_text(color = "black", size = rel(1.5), face = "bold"),
  axis.title.y = element_text(color = "black", size = rel(1.5), face = "bold"))

ggsave("daily.median.per.png", width=300, height=300, units = "mm", dpi = 300, bg = "white")


require(sf)
require(RColorBrewer)
require(maptools)
require(tmap)
require(rgdal)
require(ggspatial)
require(extrafont)
font_import(pattern = "Raleway-Bold.ttf")
font_import(pattern = "Raleway-Regular.ttf")
font_import(pattern = "cour.ttf")
loadfonts(device = "win")

ccg<-st_read("d:/WORKSPACE/COVID19/MODEL/REVISE_20200614/CCG.info.shp")
eng.la<-st_read("D:/WORKSPACE/COVID19/REVISE_LAST/ENGLAND_REGIONS.shp")
eng.la.coord <- sf::st_point_on_surface(eng.la)
la.coords <- as.data.frame(sf::st_coordinates(eng.la.coord))
la.coords$NAME <- eng.la.coord$RGN11NM

ccg$PER1_Gi<-as.factor(ccg$PER1_Gi);levels(ccg$PER1_Gi)<-c("Not Sig.","Hot spot","Cold spot")
ccg$PER2_Gi<-as.factor(ccg$PER2_Gi);levels(ccg$PER2_Gi)<-c("Not Sig.","Hot spot","Cold spot")
ccg$PER3_Gi<-as.factor(ccg$PER3_Gi);levels(ccg$PER3_Gi)<-c("Not Sig.","Hot spot","Cold spot")
ccg$PER4_Gi<-as.factor(ccg$PER4_Gi);levels(ccg$PER4_Gi)<-c("Not Sig.","Hot spot","Cold spot")
ccg$INC_20_Gi<-as.factor(ccg$INC_20_Gi);levels(ccg$INC_20_Gi)<-c("Not Sig.","Hot spot","Cold spot")
ccg$INC_80_Gi<-as.factor(ccg$INC_80_Gi);levels(ccg$INC_80_Gi)<-c("Not Sig.","Hot spot","Cold spot")

gi.map <- c("Not Sig." = "lightgrey", "Hot spot" = "red", "Cold spot" = "blue")

m.lg.d1<-ggplot(ccg)+geom_sf(aes(fill=PER1_Gi))+
  #scale_fill_brewer(palette = "OrRd",labels=c("Not Sig.","Hot spot","Cold spot"))
  scale_fill_manual(values = gi.map,name="Gi* values")+
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=25), 
        legend.text=element_text(size=25))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("m.lg.d1.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

m.lg.d2<-ggplot(ccg)+geom_sf(aes(fill=PER2_Gi))+
  #scale_fill_brewer(palette = "OrRd",labels=c("Not Sig.","Hot spot","Cold spot"))
  scale_fill_manual(values = gi.map,name="Gi* values")+
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=25), 
        legend.text=element_text(size=25))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("m.lg.d2.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

m.lg.d3<-ggplot(ccg)+geom_sf(aes(fill=PER3_Gi))+
  #scale_fill_brewer(palette = "OrRd",labels=c("Not Sig.","Hot spot","Cold spot"))
  scale_fill_manual(values = gi.map,name="Gi* values")+
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=25), 
        legend.text=element_text(size=25))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("m.lg.d3.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

m.lg.d4<-ggplot(ccg)+geom_sf(aes(fill=PER4_Gi))+
  #scale_fill_brewer(palette = "OrRd",labels=c("Not Sig.","Hot spot","Cold spot"))
  scale_fill_manual(values = gi.map,name="Gi* values")+
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=25), 
        legend.text=element_text(size=25))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+  
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("m.lg.d4.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

m.inc.20<-ggplot(ccg)+geom_sf(aes(fill=INC_20_Gi))+
  #scale_fill_brewer(palette = "OrRd",labels=c("Not Sig.","Hot spot","Cold spot"))
  scale_fill_manual(values = gi.map,name="Gi* values")+
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=25), 
        legend.text=element_text(size=25))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("local.inc.20.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

m.inc.80<-ggplot(ccg)+geom_sf(aes(fill=INC_80_Gi))+
  #scale_fill_brewer(palette = "OrRd",labels=c("Not Sig.","Hot spot","Cold spot"))
  scale_fill_manual(values = gi.map,name="Gi* values")+
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=25), 
        legend.text=element_text(size=25))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("local.inc.80.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

require(classInt)
require(tmap)
require(sp)
ccg.sp<-st_read("D:/WORKSPACE/COVID19/MODEL/REVISE_20200531/MAP_2020603_REVISE/revise.ccg.sp.proj.shp")
ccg.sp<- ccg.sp %>% st_set_crs(27700)

get.var <- function(vname,df) {
  # function to extract a variable as a vector out of an sf data frame
  # arguments:
  #    vname: variable name (as character, in quotes)
  #    df: name of sf data frame
  # returns:
  #    v: vector with values (without a column name)
  v <- df[vname] %>% st_set_geometry(NULL)
  v <- unname(v[,1])
  return(v)
}

require(classInt)
require(ggplot2)
require(viridis)
require(plyr)
percent <- c(0,.01,.1,.5,.9,.99,1)
var <- get.var("INC_20",ccg.sp);bperc <- quantile(var,percent)
breaks_qt <- classIntervals(ccg.sp$INC_20, n=6, style = "fixed", fixedBreaks=c(0.125,0.1306909,0.1475248,0.1723564,0.2086764,0.2436548,0.3978315))
ccg.sp <- mutate(ccg.sp, INC_20_cat = cut(INC_20, breaks_qt$brks)) 

inc.20.per<-ggplot(ccg.sp)+geom_sf(aes(fill=INC_20_cat))+
  scale_fill_brewer(name="Household income 20 percentile",direction=-1,palette ="RdBu",labels=c("< 1%", "1% - %10", "10% - 50%", "50% - 90%","90% - 99%", "> 99%"))+
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=25), 
        legend.text=element_text(size=25))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("inc.20.per.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

var <- get.var("INC_80",ccg.sp);bperc <- quantile(var,percent)
breaks_qt <- classIntervals(ccg.sp$INC_80, n=6, style = "fixed", fixedBreaks=c(0.024,0.07746589,0.11711983,0.16946878,0.29571518,0.35354065,0.39934866))
ccg.sp <- mutate(ccg.sp, INC_80_cat = cut(INC_80, breaks_qt$brks)) 
inc.80.per<-ggplot(ccg.sp)+geom_sf(aes(fill=INC_80_cat))+
  scale_fill_brewer(name="Household income 80 percentile",direction=-1,palette ="RdBu",labels=c("< 1%", "1% - %10", "10% - 50%", "50% - 90%","90% - 99%", "> 99%"))+
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=25), 
        legend.text=element_text(size=25))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("inc.80.per.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

##Importing the results of MGWR estimation results by different four dates - ccg.d1,ccg.d2,ccg.d3,and ccg.d4
#loading the crosswalk between CCG19CD to regions of England or targeted areas
ccg.to.la<-read.csv("D:/WORKSPACE/COVID19/DATA/NHS/REVISE_20200410/uk.ccg19.to.region.csv")
eng.ccg.to.la<-subset(ccg.to.la,grepl("NHS",ccg.to.la$CCG19NM))
eng.ccg.to.la<-na.omit(eng.ccg.to.la)
eng.ccg.to.la.cross<-distinct(eng.ccg.to.la[c("CCG19CD","RGN11CD","RGN11NM")])
eng.ccg.to.la.cross$RGN11NM[eng.ccg.to.la.cross$CCG19CD=="E38000026"]<-"East of England"
eng.ccg.to.la.cross$RGN11NM[eng.ccg.to.la.cross$CCG19CD=="E38000181"]<-"South West"
eng.ccg.to.la.cross$RGN11NM[eng.ccg.to.la.cross$CCG19CD=="E38000182"]<-"East Midlands"
eng.ccg.to.la.cross$RGN11NM[eng.ccg.to.la.cross$CCG19CD=="E38000228"]<-"North West"
eng.ccg.to.la.cross<-eng.ccg.to.la.cross[c(-2)]
eng.ccg.to.la.cross<-distinct(eng.ccg.to.la.cross,.keep_all=FALSE)
tbl.mgwr.d4<-as.data.frame(left_join(ccg.d4,eng.ccg.to.la.cross,by="CCG19CD"))
tbl.mgwr.d4.regions<-ddply(tbl.mgwr.d4,.(RGN11NM),summarise,count=length(CCG19CD),RSS_median = sum(mgwr_resid^2),RSS_min=min(mgwr_resid^2),RSS_max=max(mgwr_resid^2),R2_median=median(localR2),R2_min=min(localR2),R2_max=max(localR2),
                           b0_median=median(beta_Inter),b0_min=min(beta_Inter),b0_max=max(beta_Inter),
                           b1_median=median(beta_INC_8),b1_min=min(beta_INC_8),b1_max=max(beta_INC_8),
                           b2_median=median(beta_NOT_S),b2_min=min(beta_NOT_S),b2_max=max(beta_NOT_S),
                           b3_median=median(beta_SG_C1),b3_min=min(beta_SG_C1),b3_max=max(beta_SG_C1),
                           b4_median=median(beta_POP_D),b4_min=min(beta_POP_D),b4_max=max(beta_POP_D),
                           b5_median=median(beta_slf_m),b5_min=min(beta_slf_m),b5_max=max(beta_slf_m),
                           b6_median=median(beta_H_06),b6_min=min(beta_H_06),b6_max=max(beta_H_06))
write.csv(tbl.mgwr.d4.regions,"tbl7.regions.re.csv")

eng.la.dis<-distinct(ccg.to.la[c(1,3,12,13)])
eng.la.dis.freq<-plyr::count(eng.la.dis$LAD19NM)
colnames(eng.la.dis.freq)[1]<-'LAD19NM'
eng.la.dis.freq$LAD19NM<-trimws(eng.la.dis.freq$LAD19NM);eng.la.dis.freq<-subset(eng.la.dis.freq,LAD19NM=="City of London" | LAD19NM=="Bristol, City of"| LAD19NM=="Birmingham" | LAD19NM=="Manchester" | LAD19NM=="Leeds" | LAD19NM=="Newcastle upon Tyne")
eng.la.dis.freq<-left_join(eng.la.dis,eng.la.dis.freq,by="LAD19NM");eng.la.dis.freq<-na.omit(eng.la.dis.freq)
tbl.mgwr.d4.city<-as.data.frame(left_join(eng.la.dis.freq,ccg.d4,by="CCG19CD"))
tbl.mgwr.d4.city.sum<-ddply(tbl.mgwr.d4.city,.(LAD19NM),summarise,count=length(CCG19CD),RSS_median = sum(mgwr_resid^2),RSS_min=min(mgwr_resid^2),RSS_max=max(mgwr_resid^2),R2_median=median(localR2),R2_min=min(localR2),R2_max=max(localR2),
                           b0_median=median(beta_Inter),b0_min=min(beta_Inter),b0_max=max(beta_Inter),
                           b1_median=median(beta_INC_8),b1_min=min(beta_INC_8),b1_max=max(beta_INC_8),
                           b2_median=median(beta_NOT_S),b2_min=min(beta_NOT_S),b2_max=max(beta_NOT_S),
                           b3_median=median(beta_SG_C1),b3_min=min(beta_SG_C1),b3_max=max(beta_SG_C1),
                           b4_median=median(beta_POP_D),b4_min=min(beta_POP_D),b4_max=max(beta_POP_D),
                           b5_median=median(beta_slf_m),b5_min=min(beta_slf_m),b5_max=max(beta_slf_m),
                           b6_median=median(beta_H_06),b6_min=min(beta_H_06),b6_max=max(beta_H_06))
write.csv(tbl.mgwr.d4.city.sum,"tbl7.city.csv")
