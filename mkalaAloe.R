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
library(ggplot2)

spt3 <- gbif("Aloe","ballyi",download = T,geo = T,sp=F)
spt3
write.csv(spt3, file = "spt3.csv")

sp <- read.csv("D:/current/wc5/combined_species.csv")

coordinates(sp) <- ~lon+lat

Biom <-list.files("C:/Users/Hp/Documents/ArcGIS/submission/XG/2asc", pattern = ".asc",full.names = TRUE)

Biom

current <- stack(Biom)

names(current)
plot(Biom)

plot(current[[13]])
plot(sp[sp$species == 'Aloe ballyi', ], col = 'purple', pch = 8, add = TRUE)
plot(sp[sp$species == 'Aloe classenii', ], col = 'blue', pch = 8, add = T)
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
biom_crop <- crop(biom[[13]], c(34, 40, -5, 0))
plot(biom_crop)

plot(sp[sp$species == 'Aloe ballyi', ], col = 'blue', pch = 8, add = TRUE)
plot(sp[sp$species == 'Aloe classenii', ], col = 'brown', pch = 8, add = T)
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
aloe_c <- dismo::gbif('Aloe', 'ballyi')

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
#Two methods for cropping data one by using all the stack biom or specify.

library(sdm)

#clim_crop <- raster::crop(biom, c(34, 40, -8, 0))
#  (2) d <- sdmData(species~., ssP4,predictors = clim_crop[[c(1:13)]], bg = list(method="gRandom",n=1000))

#plot(clim_crop)
##### end of first analysis##################################################################
# Second analysis one can just read the the first functions and continue with the this second analysis to analyse each species.
# But for easy follow  would like to start from this point 
setwd("D:/current/wc5")
getwd()
library(raster)
library(sdm)
library(dismo)
library(dplyr)
library(tidyr)
library(mapview)
library(raster)
library(jsonlite)
library(tidyverse)
library(ggplot2)
sp <- read.csv("D:/current/wc5/sp1.csv")

head(sp)
spt <- sp %>% select(lon, lat)
spt$species <- 1
spt <- spt[,c("lon", "lat","species")] 

head(spt)

coordinates(spt)<- c("lon", "lat")

class(sp)

head(sp)

#--------------------
#bio <- raster::getData('worldclim',var='bio',res=5)

#bio

#names

Biom <-list.files("C:/Users/Hp/Documents/ArcGIS/submission/XG/EXTENT ASCI", pattern = ".asc",full.names = TRUE)

Biom

current <- stack(Biom)

names(current)

plot(current[[9]])

plot(sp[sp$species == 'Aloe ballyi', ], col = 'purple', pch = 8, add = TRUE)
plot(sp[sp$species == 'Aloe boscawenii', ], col = 'brown', pch = 8, add = T)
plot(sp[sp$species == 'Aloe classenii', ], col = 'blue', pch = 8, add = T)
plot(sp[sp$species == 'Aloe pembana', ], col = 'purple', pch = 8, add = T)
plot(sp[sp$species == 'Aloe penduliflora', ], col = 'yellow', pch = 8, add = T)


#-------------------
library(usdm)
v1 <- vifstep(current)
v2<- vifcor(current,th=0.9)
v1

v2

biom <- exclude(current,v1)

plot(biom)


plot(biom[[9]])

plot(spt[sp$species == 'Aloe ballyi', ], col = 'red', pch = 8, add = TRUE)

points(spt)

points(sp,cex=0.5,pch=8)

proj4string(sp)<- projection(raster())

mapView(sp)


#-------------------------------
library(sdm)

d <- sdmData(species~., spt,predictors = biom, bg = list(method="gRandom",n=1000))

getmethodNames()

m <- sdm(species~., d, methods =c("glm","BRT","Maxent","rf",'mars'),replication=c('sub','boot'),test.p=30,n=2)
m

gui(m)

g <- predict(m, biom, 'predictions29.img')
g

plot(g[[c(1,5,10,15.20)]])

enm <- ensemble(m,biom,'enms5.img',setting = list(method='weights',stat="AUC"))


plot(enm)

writeRaster(enm,"new_mkala15.tif",options = c("TWS=YES"))
raster("new_mkala15.tif") -> enm
plot(enm)

writeRaster(enm,"new_mkala15.asc",format ="ascii")

read.asciigrid("new_mkala15.asc") -> grid1

plot(grid1)


#---------------------------------------------------------------------------


#biof4570  <- raster::getData("CMIP5", var='bio', res=5, rcp=85, model='AC', year=50)

#names (biof4570 )

# this step change biof85files to asci files using arc map, and save in a new file(biof4570 )


biof4570 <-list.files("C:/Users/Hp/Documents/ArcGIS/submission/XG/4570asci", pattern = ".asc",full.names = TRUE)

biof4570 

BIOm455future <- stack(biof4570 )

names (BIOm455future)

names(biom)

names (BIOm455future) <- names(biom)

enmf <- ensemble(m,BIOm455future,'enmsf6.img',setting = list(method='weights',stat="AUC"))

plot(enmf)

cl <-  colorRampPalette(c("red", "white", "blue"),space = "Lab")

plot(enmf,col=cl(200))

writeRaster(enmf,"new_mkala16.tif",options = c("TWS=YES"))
raster("new_mkala16.tif") -> enmf1
plot(enmf1)

writeRaster(enmf1,"new_mkala16.asc",format ="ascii")

read.asciigrid("new_mkala16.asc") -> grid1

plot(grid1)


plot(stack(enm,enmf))

mapview(stack(enm,enmf))

ch <- enmf-enm

writeRaster(ch,"new_mkalac.asc",format ="ascii")

read.asciigrid("new_mkalac.asc") -> ch1

plot(ch1)

getEvaluation(m,stat ='AUC')

ev <- getEvaluation(m,stat = c('AUC','TSS', 'threshold'),opt = 2)

mean(ev$threshold)

pa <- raster(enm)

pa[] <- ifelse(enm[] >=     0.2010524, 1, 0)

plot(pa)

paf <- raster(enmf)

paf[] <- ifelse(enmf[] >=   0.2010524, 1, 0)

plot(paf)


pac <- paf-pa


cl <- colorRampPalette(c("red", "gray", "blue"))

plot(pac,col=cl(3))

plot(pac)

#------------------------
rcurve(m)

plot(rcurve(m))

getVarImp(m)

plot(getVarImp(m))


niche(biom,enm,n=c('bio13','PET'),col=cl(200))
