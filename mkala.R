setwd("D:/current/wc5")

library(sdm)
library(dismo)
library(dplyr)
library(tidyr)
library(mapview)
library(raster)
library(jsonlite)
library(devtools)
library(tidyverse)
library(sp)

spt3 <- gbif("Aloe","pembana",download = T,geo = T,sp=F)
spt3
write.csv(spt3, file = "spt3.csv")

sp <- read.csv("D:/current/wc5/combined_species.csv")

coordinates(sp) <- ~lon+lat

Biom <-list.files("C:/Users/Hp/Documents/ArcGIS/submission/XG/extent2asci", pattern = ".asc",full.names = TRUE)

Biom

current <- stack(Biom)

names(current)

plot(current[[2]])
plot(sp[sp$species == 'Aloe ballyi', ], col = 'blue', pch = 8, add = TRUE)
plot(sp[sp$species == 'Aloe boscawenii', ], col = 'brown', pch = 8, add = T)
plot(sp[sp$species == 'Aloe classenii', ], col = 'RED', pch = 8, add = T)
plot(sp[sp$species == 'Aloe pembana', ], col = 'purple', pch = 8, add = T)
plot(sp[sp$species == 'Aloe penduliflora', ], col = 'yellow', pch = 8, add = T)

#-------------------
library(usdm)
v1 <- vifstep(current)
v2<- vifcor(current,th=0.9)
v1

v2

biom <- exclude(current,v1)

#####################################################################################################
#######Step for identifying min and max points for the species to help id developing specific area of extent#
min_max <- read.csv("D:/current/wc5/combined_species.csv")
table(min_max$species)
min(min_max$lat)
min(min_max$lon)
max(min_max$lat)
max(min_max$lon)

#######this step is for croping your desired species to the exact point of area studY#####
biom_crop <- crop(biom[[9]], c(34, 40, -8, 0))
plot(biom_crop)

plot(sp[sp$species == 'Aloe ballyi', ], col = 'blue', pch = 8, add = TRUE)
plot(sp[sp$species == 'Aloe boscawenii', ], col = 'red', pch = 8, add = T)
plot(sp[sp$species == 'Aloe classenii', ], col = 'brown', pch = 8, add = T)
plot(sp[sp$species == 'Aloe pembana', ], col = 'green', pch = 8, add = T)
plot(sp[sp$species == 'Aloe penduliflora', ], col = 'yellow', pch = 8, add = T)

points(sp)

points(sp,cex=0.5,pch=16)

proj4string(sp)<- projection(raster())

mapView(sp)


#points(ssP4[ssP4$species == 'Aloe classenii', ], col = 'red', pch = 16) NB;No need to add true but do it in the firts line.

#How to save the Raster after cropping it to your study area####
writeRaster(x = biom_crop, filename = 'D:/current/wc5/biosm.tif')

check <- raster::stack('D:/current/wc5/biosm.tif')

### This command helps in identifying more areas with more species coordinates incase species you have are limited.
#Then go further ahead using google earth to obtain the coordinates#
aloe_c <- dismo::gbif('Aloe', 'pembana')

aloe_c$cloc
###______-------------------------###
points(sp,cex=0.5,pch=16)

proj4string(sp)<- projection(raster())

mapView(sp)


#----------------------------------------------------------
aloe_b <- dismo::gbif('Aloe', 'ballyi')

aloe_b$cloc

#----------------------------------------------------------

aloe_c<- dismo::gbif('Aloe', 'penduliflora')

aloe_c$cloc
#----------------------------------------------------------



#-------------------------------
#Two methods for cropping data one by uing all the stack biom or specify.

library(sdm)

#clim_crop <- raster::crop(biom, c(34, 40, -8, 0))
#  (2) d <- sdmData(species~., ssP4,predictors = clim_crop[[c(1:13)]], bg = list(method="gRandom",n=1000))

#plot(clim_crop)
#-------------------------------Method--------------------------------1
library(sdm)

d <- sdmData(species~., sp,predictors = biom, bg = list(method="gRandom",n=1000))

getmethodNames()

m <- sdm(species~., d, methods =c("glm","BRT","Maxent","rf",'mars'),replication=c('sub','boot'),test.p=30,n=2)
m

gui(m)

g <- predict(m, biom, 'predictions29.img')
g

plot(g[[c(1,5,10,15.20)]])

enm <- ensemble(m,biom,'enms.img55',setting = list(method='weights',stat="AUC"))

plot(enm)

writeRaster(enm,"new_mkala24.tif",options = c("TWS=YES"))
raster("new_mkala24.tif") -> enm
plot(enm)

#####method 2------------------------------------------------------------------------------
ssp5 <- read.csv("D:/current/wc5/species.csv")
coordinates(ssp5) <- ~lon+lat
class(ssp5)

plot(biom)

d2 <- sdmData(ballyi+classenii+penduliflora~., ssp5, predictors = biom, 
              bg = list(method="gRandom",n=1000))


getmethodNames()
###################-----------------------method 2---------------------------------------------------
m5 <- sdm(ballyi+classenii+penduliflora~., d2, methods =c("glm","maxent","BRT","rf","mars"),
          replication=c('sub','boot'),test.p=30,n=2)

m5

gui(m5)
---------------------------------------------------------------------##########################
g <- predict(m5, biom, 'predictions1.img')
g

plot(g[[c(1,5,10,15.20)]])

enm <- ensemble(m5,biom,'enmk5.img',setting = list(method='weights',stat="AUC"))

plot(enm)

writeRaster(enm,"new_mkala5.tif",options = c("TWS=YES"))
raster("new_mkala5.tif") -> enm
plot(enm)

writeRaster(enm,"new_mkala5.asc",format ="ascii")

read.asciigrid("new_mkala5.asc") -> grid1

plot(grid1)


#---------------------------------------------------------------------------


biof8570 <- raster::getData("CMIP5", var='bio', res=5, rcp=85, model='AC', year=70)

BIOm557 <-list.files("C:/Users/Hp/Documents/ArcGIS/submission/XG/8570asci", pattern = ".asc",full.names = TRUE)

BIOm557

BIOm557future <- stack(BIOm557)

names (BIOm557future)

names(biom)

names (BIOm557future) <- names(biom)

enf <- ensemble(m5,BIOm557future,'ens8570.img',setting = list(method='weights',stat="AUC"))

plot(enf)

cl <-  colorRampPalette(c("red", "white", "blue"),space = "Lab")

plot(enf,col=cl(200))

writeRaster(enf,"new_mkala857.tif",options = c("TWS=YES"))
raster("new_mkala857.tif") -> enf1
plot(enf1)

writeRaster(enf1,"new_mkala857.asc",format ="ascii")

read.asciigrid("new_mkala857.asc") -> grid1

plot(grid1)


plot(stack(enm,enf1))

mapview(stack(enm,enf1))

ch <- enf1-enm

plot(ch)

getEvaluation(m5,stat ='AUC')

ev <- getEvaluation(m5,stat = c('AUC','TSS', 'threshold'),opt = 2)

mean(ev$threshold)

pa <- raster(enm)

pa[] <- ifelse(enm[] >=  0.2739444, 1, 0)

pa

paf <- raster(enf)

paf[] <- ifelse(enf[] >=   0.2739444, 1, 0)

plot(paf)


pac <- paf-pa

plot(pac)

cl <- colorRampPalette(c("red", "darkgreen", "blue"))
cl <- colorRampPalette(c("red", "gray", "blue"))

plot(pac,col=cl(3))

plot(pac)

#------------------------
rcurve(m5)

getVarImp(m5)

plot(getVarImp(m5))


niche(biom,enm,n=c('bio4','bio13'),col=cl(200))

