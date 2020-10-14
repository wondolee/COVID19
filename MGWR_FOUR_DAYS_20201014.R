Sys.setlocale(locale="English_United Kingdom")
setwd("d:/WORKSPACE/COVID19/MODEL/FINAL/MGWR/SHP")

require(sf)
require(ggplot2)
require(RColorBrewer)
require(classInt)
require(maptools)
require(rgdal)
require(ggspatial)
require(extrafont)
loadfonts(device = "win")

ccg.d1<-st_read("MGWR_D1_RESULTS_CCG.shp")
ccg.d2<-st_read("MGWR_D2_RESULTS_CCG.shp")
ccg.d3<-st_read("MGWR_D3_RESULTS_CCG.shp")
ccg.d4<-st_read("MGWR_D4_RESULTS_CCG.shp")
st_crs(ccg.d1)<-27700;st_crs(ccg.d2)<-27700;st_crs(ccg.d3)<-27700;st_crs(ccg.d4)<-27700

eng.la<-st_read("D:/WORKSPACE/COVID19/REVISE_LAST/ENGLAND_REGIONS.shp")
st_crs(eng.la)<-27700
eng.la.coord <- sf::st_point_on_surface(eng.la)
la.coords <- as.data.frame(sf::st_coordinates(eng.la.coord))
la.coords$NAME <- eng.la.coord$RGN11NM

mgwr.r2.d1<- ggplot(ccg.d1)+geom_sf(aes(fill = cut_number(localR2,5)),alpha = 0.8,size = 0.3)+#colour = 'white'                                                            
  scale_fill_brewer(palette = "Reds",                                         
                    name = "Local R2 for March 24, 2020") + 
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("d:/WORKSPACE/COVID19/REVISE_LAST/mgwr.r2.d1.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")


mgwr.r2.d2<- ggplot(ccg.d2)+geom_sf(aes(fill = cut_number(localR2,5)),alpha = 0.8,size = 0.3)+#colour = 'white'                                                            
  scale_fill_brewer(palette = "Reds",                                         
                    name = "Local R2 for March 31, 2020") + 
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("d:/WORKSPACE/COVID19/REVISE_LAST/mgwr.r2.d2.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")


mgwr.r2.d3<- ggplot(ccg.d3)+geom_sf(aes(fill = cut_number(localR2,5)),alpha = 0.8,size = 0.3)+#colour = 'white'                                                            
  scale_fill_brewer(palette = "Reds",                                         
                    name = "Local R2 for April 7, 2020") + 
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("d:/WORKSPACE/COVID19/REVISE_LAST/mgwr.r2.d3.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

mgwr.r2.d4<- ggplot(ccg.d4)+geom_sf(aes(fill = cut_number(localR2,5)),alpha = 0.8,size = 0.3)+#colour = 'white'                                                            
  scale_fill_brewer(palette = "Reds",                                         
                    name = "Local R2 for April 14, 2020") + 
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_text(data=la.coords, aes(X, Y, label = NAME), colour = "Black",size=6,family="Raleway", fontface="bold")+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)
ggsave("d:/WORKSPACE/COVID19/REVISE_LAST/mgwr.r2.d4.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

##local r2
mgwr.r2.d1<-tm_shape(ccg.d1) +
  tm_fill("localR2",title="Local R2 for 24 March 2020",n=5,style="jenks")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Adj. R2", title.position = c("left","top"))
 
mgwr.r2.d2<-tm_shape(ccg.d2) +
  tm_fill("localR2",title="Local R2 for 31 March 2020",n=5,style="jenks")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Adj. R2", title.position = c("left","top"))

mgwr.r2.d3<-tm_shape(ccg.d3) +
  tm_fill("localR2",title="Local R2 for 7 April 2020",n=5,style="jenks")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Adj. R2", title.position = c("left","top"))

mgwr.r2.d4<-tm_shape(ccg.d4) +
  tm_fill("localR2",title="Local R2 for 14 April 2020",n=5,style="jenks")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Adj. R2", title.position = c("left","top"))

pdf(file='mgwr.r2.d1.pdf')
print(mgwr.r2.d1)
dev.off()
    
pdf(file='mgwr.r2.d2.pdf')
print(mgwr.r2.d2)
dev.off()

pdf(file='mgwr.r2.d3.pdf')
print(mgwr.r2.d3)
dev.off()

pdf(file='mgwr.r2.d4.pdf')
print(mgwr.r2.d4)
dev.off()

##beta_Inter, beta_INC_8, beta_NOT_S, beta_SG_C1, beta_POP_D, beta_slf_m, beta_H_06

##Intercept
mgwr.b0.d1<-tm_shape(ccg.d1) +
  tm_fill("beta_Inter",title="Coeff for 24 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Intercept", title.position = c("left","top"))

mgwr.b0.d2<-tm_shape(ccg.d2) +
  tm_fill("beta_Inter",title="Coeff for 31 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Intercept", title.position = c("left","top"))

mgwr.b0.d3<-tm_shape(ccg.d3) +
  tm_fill("beta_Inter",title="Coeff for 7 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Intercept", title.position = c("left","top"))

mgwr.b0.d4<-tm_shape(ccg.d4) +
  tm_fill("beta_Inter",title="Coeff for 14 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Intercept", title.position = c("left","top"))

pdf(file='mgwr.intercept.d1.pdf')
print(mgwr.b0.d1)
dev.off()

pdf(file='mgwr.intercept.d2.pdf')
print(mgwr.b0.d2)
dev.off()

pdf(file='mgwr.intercept.d3.pdf')
print(mgwr.b0.d3)
dev.off()

pdf(file='mgwr.intercept.d4.pdf')
print(mgwr.b0.d4)
dev.off()

##%Households in the top income quintile
mgwr.b1.d1<-tm_shape(ccg.d1) +
  tm_fill("beta_INC_8",title="Coeff for 24 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Households in the top income quintile", title.position = c("left","top"))

mgwr.b1.d2<-tm_shape(ccg.d2) +
  tm_fill("beta_INC_8",title="Coeff for 31 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Households in the top income quintile", title.position = c("left","top"))

mgwr.b1.d3<-tm_shape(ccg.d3) +
  tm_fill("beta_INC_8",title="Coeff for 7 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Households in the top income quintile", title.position = c("left","top"))

mgwr.b1.d4<-tm_shape(ccg.d4) +
  tm_fill("beta_INC_8",title="Coeff for 14 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Households in the top income quintile", title.position = c("left","top"))

pdf(file='mgwr.income80.d1.pdf')
print(mgwr.b1.d1)
dev.off()

pdf(file='mgwr.income80.d2.pdf')
print(mgwr.b1.d2)
dev.off()

pdf(file='mgwr.income80.d3.pdf')
print(mgwr.b1.d3)
dev.off()

pdf(file='mgwr.income80.d4.pdf')
print(mgwr.b1.d4)
dev.off()

##non-English speaker
mgwr.b2.d1<-tm_shape(ccg.d1) +
  tm_fill("beta_NOT_S",title="Coeff for 24 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of non-English speaker", title.position = c("left","top"))

mgwr.b2.d2<-tm_shape(ccg.d2) +
  tm_fill("beta_NOT_S",title="Coeff for 31 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of non-English speaker", title.position = c("left","top"))

mgwr.b2.d3<-tm_shape(ccg.d3) +
  tm_fill("beta_NOT_S",title="Coeff for 7 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of non-English speaker", title.position = c("left","top"))

mgwr.b2.d4<-tm_shape(ccg.d4) +
  tm_fill("beta_NOT_S",title="Coeff for 14 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of non-English speaker", title.position = c("left","top"))

pdf(file='mgwr.nonEng.d1.pdf')
print(mgwr.b2.d1)
dev.off()

pdf(file='mgwr.nonEng.d2.pdf')
print(mgwr.b2.d2)
dev.off()

pdf(file='mgwr.nonEng.d3.pdf')
print(mgwr.b2.d3)
dev.off()

pdf(file='mgwr.nonEng.d4.pdf')
print(mgwr.b2.d4)
dev.off()

##SOcial grade C1: beta_SG_C1
mgwr.b3.d1<-tm_shape(ccg.d1) +
  tm_fill("beta_SG_C1",title="Coeff for 24 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Social grade C1 (lower middle class)", title.position = c("left","top"))

mgwr.b3.d2<-tm_shape(ccg.d2) +
  tm_fill("beta_SG_C1",title="Coeff for 31 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Social grade C1 (lower middle class)", title.position = c("left","top"))

mgwr.b3.d3<-tm_shape(ccg.d3) +
  tm_fill("beta_SG_C1",title="Coeff for 7 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Social grade C1 (lower middle class)", title.position = c("left","top"))

mgwr.b3.d4<-tm_shape(ccg.d4) +
  tm_fill("beta_SG_C1",title="Coeff for 14 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Social grade C1 (lower middle class)", title.position = c("left","top"))

pdf(file='mgwr.C1.d1.pdf')
print(mgwr.b3.d1)
dev.off()

pdf(file='mgwr.C1.d2.pdf')
print(mgwr.b3.d2)
dev.off()

pdf(file='mgwr.C1.d3.pdf')
print(mgwr.b3.d3)
dev.off()

pdf(file='mgwr.C1.d4.pdf')
print(mgwr.b3.d4)
dev.off()

##Resident population; beta_POP_D
mgwr.b4.d1<-tm_shape(ccg.d1) +
  tm_fill("beta_POP_D",title="Coeff for 24 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Resient population density", title.position = c("left","top"))

mgwr.b4.d2<-tm_shape(ccg.d2) +
  tm_fill("beta_POP_D",title="Coeff for 31 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Resient population density", title.position = c("left","top"))

mgwr.b4.d3<-tm_shape(ccg.d3) +
  tm_fill("beta_POP_D",title="Coeff for 7 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Resient population density", title.position = c("left","top"))

mgwr.b4.d4<-tm_shape(ccg.d4) +
  tm_fill("beta_POP_D",title="Coeff for 14 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Resient population density", title.position = c("left","top"))

pdf(file='mgwr.res.pop.d1.pdf')
print(mgwr.b4.d1)
dev.off()

pdf(file='mgwr.res.pop.d2.pdf')
print(mgwr.b4.d2)
dev.off()

pdf(file='mgwr.res.pop.d3.pdf')
print(mgwr.b4.d3)
dev.off()

pdf(file='mgwr.res.pop.d4.pdf')
print(mgwr.b4.d4)
dev.off()

##Share of self-employed workers; beta_slf
mgwr.b5.d1<-tm_shape(ccg.d1) +
  tm_fill("beta_slf_m",title="Coeff for 24 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Self-employed workers", title.position = c("left","top"))

mgwr.b5.d2<-tm_shape(ccg.d2) +
  tm_fill("beta_slf_m",title="Coeff for 31 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Self-employed workers", title.position = c("left","top"))

mgwr.b5.d3<-tm_shape(ccg.d3) +
  tm_fill("beta_slf_m",title="Coeff for 7 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Self-employed workers", title.position = c("left","top"))

mgwr.b5.d4<-tm_shape(ccg.d4) +
  tm_fill("beta_slf_m",title="Coeff for 14 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Self-employed workers", title.position = c("left","top"))

pdf(file='mgwr.slf.emp.d1.pdf')
print(mgwr.b5.d1)
dev.off()

pdf(file='mgwr.slf.emp.d2.pdf')
print(mgwr.b5.d2)
dev.off()

pdf(file='mgwr.slf.emp.d3.pdf')
print(mgwr.b5.d3)
dev.off()

pdf(file='mgwr.slf.emp.d4.pdf')
print(mgwr.b5.d4)
dev.off()

##beta_H_06
mgwr.b6.d1<-tm_shape(ccg.d1) +
  tm_fill("beta_H_06",title="Coeff for 24 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Population in bad health", title.position = c("left","top"))

mgwr.b6.d2<-tm_shape(ccg.d2) +
  tm_fill("beta_H_06",title="Coeff for 31 March 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Population in bad health", title.position = c("left","top"))

mgwr.b6.d3<-tm_shape(ccg.d3) +
  tm_fill("beta_H_06",title="Coeff for 7 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Population in bad health", title.position = c("left","top"))

mgwr.b6.d4<-tm_shape(ccg.d4) +
  tm_fill("beta_H_06",title="Coeff for 14 April 2020",style="sd",palette="-RdBu")  +
  tm_borders(col = "black",lwd=0.001) + tm_compass() + tm_scale_bar() +
  tm_layout(frame = FALSE)+
  tm_layout(title = "Share of Population in bad health", title.position = c("left","top"))

pdf(file='mgwr.bad.hea.d1.pdf')
print(mgwr.b6.d1)
dev.off()

pdf(file='mgwr.bad.hea.d2.pdf')
print(mgwr.b6.d2)
dev.off()

pdf(file='mgwr.bad.hea.d3.pdf')
print(mgwr.b6.d3)
dev.off()

pdf(file='mgwr.bad.hea.d4.pdf')
print(mgwr.b6.d4)
dev.off()