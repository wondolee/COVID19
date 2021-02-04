require(sp)
require(sf)
require(spdep)
require(maptools)
require(raster)
require(rgdal)
ccg.spoly<-readRDS("ccg.spoly.geojson")
proj4string(ccg.spoly) <- CRS("+init=epsg:27700")
ccg.spoly$H_06<-ccg.spoly$H_04+ccg.spoly$H_05
step.model.d1<-lm(formula = scale(PER1) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly)
step.model.d2<-lm(formula = scale(PER2) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly)
step.model.d3<-lm(formula = scale(PER3) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly)
step.model.d4<-lm(formula = scale(PER4) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly)

require(RANN)
require(knn)
coords <- coordinates(ccg.spoly)
knear4 <- knn2nb(knearneigh(coords, k=4))
knear4nb <- nb2listw(knear4, style="W")        

global.moran.d1<-moran.test(ccg.spoly$PER1,listw2U(knear4nb),randomisation=FALSE)
global.moran.d2<-moran.test(ccg.spoly$PER2,listw2U(knear4nb),randomisation=FALSE)
global.moran.d3<-moran.test(ccg.spoly$PER3,listw2U(knear4nb),randomisation=FALSE)
global.moran.d4<-moran.test(ccg.spoly$PER4,listw2U(knear4nb),randomisation=FALSE)

moran.d1.knn4<- lm.morantest(step.model.d1,listw2U(knear4nb))
moran.d2.knn4<- lm.morantest(step.model.d2,listw2U(knear4nb))
moran.d3.knn4<- lm.morantest(step.model.d3,listw2U(knear4nb))
moran.d4.knn4<- lm.morantest(step.model.d4,listw2U(knear4nb))

require(spatialreg)
lag.model.d1<-lagsarlm(scale(PER1) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
lag.model.d2<-lagsarlm(scale(PER2) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
lag.model.d3<-lagsarlm(scale(PER3) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
lag.model.d4<-lagsarlm(scale(PER4) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))

err.model.d1<-errorsarlm(scale(PER1) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
err.model.d2<-errorsarlm(scale(PER2) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
err.model.d3<-errorsarlm(scale(PER3) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
err.model.d4<-errorsarlm(scale(PER4) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
summary(lag.model.d1,Nagelkerke=T)

require(GWmodel)
dist<-gw.dist(coordinates(ccg.spoly))
reg1.mod = as.formula(scale(PER1) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06))
reg2.mod = as.formula(scale(PER2) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06))
reg3.mod = as.formula(scale(PER3) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06))
reg4.mod = as.formula(scale(PER4) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06))

gwr.bwd.f1 <- bw.gwr(reg1.mod, ccg.spoly, approach="AICc", kernel="bisquare", adaptive=TRUE, dMat=dist)
gwr.bwd.f2 <- bw.gwr(reg1.mod, ccg.spoly, approach="AICc", kernel="bisquare", adaptive=TRUE, dMat=dist)
gwr.bwd.f3 <- bw.gwr(reg1.mod, ccg.spoly, approach="AICc", kernel="bisquare", adaptive=TRUE, dMat=dist)
gwr.bwd.f4 <- bw.gwr(reg1.mod, ccg.spoly, approach="AICc", kernel="bisquare", adaptive=TRUE, dMat=dist)

mod.GWR.f1 <- gwr.basic(reg1.mod, data = ccg.spoly, bw = gwr.bwd.f1, kernel="bisquare", adaptive=TRUE, dMat=dist)
mod.GWR.f2 <- gwr.basic(reg2.mod, data = ccg.spoly, bw = gwr.bwd.f2, kernel="bisquare", adaptive=TRUE, dMat=dist)
mod.GWR.f3 <- gwr.basic(reg3.mod, data = ccg.spoly, bw = gwr.bwd.f3, kernel="bisquare", adaptive=TRUE, dMat=dist)
mod.GWR.f4 <- gwr.basic(reg4.mod, data = ccg.spoly, bw = gwr.bwd.f4, kernel="bisquare", adaptive=TRUE, dMat=dist)

new.gwr.m3.d1<-mod.GWR.f1$SDF
new.gwr.m3.d2<-mod.GWR.f2$SDF
new.gwr.m3.d3<-mod.GWR.f3$SDF
new.gwr.m3.d4<-mod.GWR.f4$SDF

mod.MGWR.f1 <- gwr.multiscale(scale(PER1)~scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06)
                              ,data = ccg.spoly, max.iterations = 30,
                              criterion="CVR", kernel = "gaussian", adaptive=T,  
                              bws0=c(0,0,0,0,0,0,0),
                              dMats=list(dist,dist,dist,dist,dist,dist,dist),
                              verbose = F, hatmatrix = F, predictor.centered=rep(T, 4))
mod.MGWR.f2 <- gwr.multiscale(scale(PER2)~scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06)
                              ,data = ccg.spoly, max.iterations = 30,
                              criterion="CVR", kernel = "gaussian", adaptive=T,  
                              bws0=c(0,0,0,0,0,0,0),
                              dMats=list(dist,dist,dist,dist,dist,dist,dist),
                              verbose = F, hatmatrix = F, predictor.centered=rep(T, 4))
mod.MGWR.f3 <- gwr.multiscale(scale(PER3)~scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06)
                              ,data = ccg.spoly, max.iterations = 30,
                              criterion="CVR", kernel = "gaussian", adaptive=T,  
                              bws0=c(0,0,0,0,0,0,0),
                              dMats=list(dist,dist,dist,dist,dist,dist,dist),
                              verbose = F, hatmatrix = F, predictor.centered=rep(T, 4))
mod.MGWR.f4 <- gwr.multiscale(scale(PER4)~scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06)
                              ,data = ccg.spoly, max.iterations = 30,
                              criterion="CVR", kernel = "gaussian", adaptive=T,  
                              bws0=c(0,0,0,0,0,0,0),
                              dMats=list(dist,dist,dist,dist,dist,dist,dist),
                              verbose = F, hatmatrix = F, predictor.centered=rep(T, 4))

new.MGWR.m3.d1<-mod.GWR.f1$SDF
new.MGWR.m3.d2<-mod.GWR.f2$SDF
new.MGWR.m3.d3<-mod.GWR.f3$SDF
new.MGWR.m3.d4<-mod.GWR.f4$SDF
