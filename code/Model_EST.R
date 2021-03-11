###Loading the packages for spatial regression model in R
require(sp)
require(sf)
require(spdep)
require(maptools)
require(raster)
require(rgdal)

###Importing the input data 
ccg.spoly<-readRDS("ccg.spoly.geojson")
ccg.spoly$H_06<-ccg.spoly$H_04+ccg.spoly$H_05 ##Updating the variable of share of population in bad health; H_04 (bad health) and H_05 (very bad health)

###Building the OLS models: d1;24 March, d2;31 March, d3; 7 April, and d4; 14 April 2020.
step.model.d1<-lm(formula = scale(PER1) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly)
step.model.d2<-lm(formula = scale(PER2) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly)
step.model.d3<-lm(formula = scale(PER3) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly)
step.model.d4<-lm(formula = scale(PER4) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly)

##Set up a spatial weight matrix using a nubmer of nearest neighbours (K=4)
require(RANN)
require(knn)
coords <- coordinates(ccg.spoly)
knear4 <- knn2nb(knearneigh(coords, k=4))
knear4nb <- nb2listw(knear4, style="W")        

##Check the spatial autocorrelation using a spatial weight matrix
global.moran.d1<-moran.test(ccg.spoly$PER1,listw2U(knear4nb),randomisation=FALSE)
global.moran.d2<-moran.test(ccg.spoly$PER2,listw2U(knear4nb),randomisation=FALSE)
global.moran.d3<-moran.test(ccg.spoly$PER3,listw2U(knear4nb),randomisation=FALSE)
global.moran.d4<-moran.test(ccg.spoly$PER4,listw2U(knear4nb),randomisation=FALSE)

##Check the spatial autocorelation in OLS model residuals using a spatial weight matrix
moran.d1.knn4<- lm.morantest(step.model.d1,listw2U(knear4nb))
moran.d2.knn4<- lm.morantest(step.model.d2,listw2U(knear4nb))
moran.d3.knn4<- lm.morantest(step.model.d3,listw2U(knear4nb))
moran.d4.knn4<- lm.morantest(step.model.d4,listw2U(knear4nb))

###Spatial autoregressive model
require(spatialreg)

##Lagrange Multiplier Test - simple diagnostic tests for spatial lag dependence and spatial error autocorrelation, respectively
lm.test.d1<-lm.LMtests(step.model.d1, listw2U(knear4nb), test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))
lm.test.d2<-lm.LMtests(step.model.d2, listw2U(knear4nb), test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))
lm.test.d3<-lm.LMtests(step.model.d3, listw2U(knear4nb), test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))
lm.test.d4<-lm.LMtests(step.model.d4, listw2U(knear4nb), test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))

##Building the spatial lag model
lag.model.d1<-lagsarlm(scale(PER1) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
lag.model.d2<-lagsarlm(scale(PER2) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
lag.model.d3<-lagsarlm(scale(PER3) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
lag.model.d4<-lagsarlm(scale(PER4) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))

##Building the spatial error model
err.model.d1<-errorsarlm(scale(PER1) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
err.model.d2<-errorsarlm(scale(PER2) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
err.model.d3<-errorsarlm(scale(PER3) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
err.model.d4<-errorsarlm(scale(PER4) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06), data = ccg.spoly,listw2U(knear4nb))
summary(lag.model.d1,Nagelkerke=T) ##retaining the pseudo R2 for spatial autoregressive models

###GW model
require(GWmodel)
dist<-gw.dist(coordinates(as(ccg.spoly,"Spatial"))) ##Calculate a distance vector for GW model calibration points

##Model specifications
reg1.mod = as.formula(scale(PER1) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06))
reg2.mod = as.formula(scale(PER2) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06))
reg3.mod = as.formula(scale(PER3) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06))
reg4.mod = as.formula(scale(PER4) ~ scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06))

##Retaining the bandwidths for GWR models - would be get 155 neighbourhoods in consecutive 4 Tuesdays (24 March, 31 March, 7 April, and 14 April)
gwr.bwd.d1 <- bw.gwr(reg1.mod, as(ccg.spoly,"Spatial"), approach="AICc", kernel="bisquare", adaptive=TRUE, dMat=dist)
gwr.bwd.d2 <- bw.gwr(reg2.mod, as(ccg.spoly,"Spatial"), approach="AICc", kernel="bisquare", adaptive=TRUE, dMat=dist)
gwr.bwd.d3 <- bw.gwr(reg3.mod, as(ccg.spoly,"Spatial"), approach="AICc", kernel="bisquare", adaptive=TRUE, dMat=dist)
gwr.bwd.d4 <- bw.gwr(reg4.mod, as(ccg.spoly,"Spatial"), approach="AICc", kernel="bisquare", adaptive=TRUE, dMat=dist)

##Building the GWR model - using Adaptive bi-square kernel
mod.GWR.d1 <- gwr.basic(reg1.mod, data = as(ccg.spoly,"Spatial"), bw = gwr.bwd.f1, kernel="bisquare", adaptive=TRUE, dMat=dist)
mod.GWR.d2 <- gwr.basic(reg2.mod, data = as(ccg.spoly,"Spatial"), bw = gwr.bwd.f2, kernel="bisquare", adaptive=TRUE, dMat=dist)
mod.GWR.d3 <- gwr.basic(reg3.mod, data = as(ccg.spoly,"Spatial"), bw = gwr.bwd.f3, kernel="bisquare", adaptive=TRUE, dMat=dist)
mod.GWR.d4 <- gwr.basic(reg4.mod, data = as(ccg.spoly,"Spatial"), bw = gwr.bwd.f4, kernel="bisquare", adaptive=TRUE, dMat=dist)

##Exporting the outcomes of GWR model estimation results into SpatialPolygonsDataFrame
new.gwr.m3.d1<-mod.GWR.d1$SDF
new.gwr.m3.d2<-mod.GWR.d2$SDF
new.gwr.m3.d3<-mod.GWR.d3$SDF
new.gwr.m3.d4<-mod.GWR.d4$SDF

##Building the MGWR model; bandwidth calibration starting from GWR bandwidth (BW=155)
mod.MGWR.d1 <- gwr.multiscale(reg1.mod,as(ccg.spoly,"Spatial"),criterion="dCVR", kernel = "bisquare", adaptive=T,  
                              bws0=c(155,155,155,155,155,155,155),dMats=list(dist,dist,dist,dist,dist,dist,dist),
                              verbose = T, hatmatrix = T)
mod.MGWR.d2 <- gwr.multiscale(reg2.mod,as(ccg.spoly,"Spatial"), criterion="dCVR", kernel = "bisquare", adaptive=T,  
                              bws0=c(155,155,155,155,155,155,155),dMats=list(dist,dist,dist,dist,dist,dist,dist),
                              verbose = T, hatmatrix = T)
mod.MGWR.d3 <- gwr.multiscale(scale(PER3)~scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06)
                              ,as(ccg.spoly,"Spatial"), criterion="dCVR", kernel = "bisquare", adaptive=T,  
                              bws0=c(155,155,155,155,155,155,155),dMats=list(dist,dist,dist,dist,dist,dist,dist),
                              verbose = T, hatmatrix = T)
mod.MGWR.d4 <- gwr.multiscale(scale(PER4)~scale(NOT_SPE) + scale(POP_DEN)+ scale(INC_80) + scale(slf_mpl)+scale(SG_C1)+scale(H_06)
                              ,as(ccg.spoly,"Spatial"), criterion="dCVR", kernel = "bisquare", adaptive=T,  
                              bws0=c(155,155,155,155,155,155,155),dMats=list(dist,dist,dist,dist,dist,dist,dist),
                              verbose = T, hatmatrix = T)
							  
##Exporting the outcomes of MGWR model estimation results into SpatialPolygonsDataFrame
new.MGWR.m3.d1<-mod.GWR.d1$SDF
new.MGWR.m3.d2<-mod.GWR.d2$SDF
new.MGWR.m3.d3<-mod.GWR.d3$SDF
new.MGWR.m3.d4<-mod.GWR.d4$SDF
