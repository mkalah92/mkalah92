setwd("C:/Users/Hp/Documents/sp/africana")
library(sdm)
library(dismo)
library(dplyr)
library(tidyr)
library(mapview)
library(raster)
library(jsonlite)
sp <- gbif("Hydnora","africana",download = T,geo = T,sp=F)
sp
write.csv(sp, file = "sp.csv")

######## After writing csv file remove extra information to remain with lat and long.
######## Then filter the data to remove duplicated coordinates. 

sp2 <- read.csv("C:/Users/Hp/Documents/sp/africana/sp.csv")

#Species thinning to a distance of preference

setwd("C:/Users/Hp/Documents/sp/thinningsp")
getwd()

#Packages
library(raster)
library(spThin)
library(dplyr)

#Read file
sp <- read.csv("C:/Users/Hp/Documents/sp/africana/sp.csv")

#Check table structure
head(sp)

#The spThin applied below can also thin multiple species at same time
e <- split(sp, sp$species)
names <- names(e)

lapply(seq_along(e),
       function(x){
         thin(
           e[[x]],
           lat.col = "lat",
           long.col = "lon",
           spec.col = "species",
           thin.par= 5,
           reps=5,
           locs.thinned.list.return = FALSE,
           write.files = TRUE,
           max.files = 5,
           out.dir="C:/Users/Hp/Documents/sp/africana/thinningsp/spthin",
           out.base = x,
           write.log.file = TRUE,
           log.file = "spatial_thin_log.txt",
           verbose = TRUE)
       })

filenames <- list.files(pattern="*thin1.csv")
H_thin <- do.call("rbind",lapply(filenames,FUN=function(files){ read.csv(files)}))
write.csv(H_thin, file= "Her_th.csv",row.names = F)

# SPEcies distribution command
#SDM_C1_SDM_Biogeoinformatics_YT_Naimi
#https://www.youtube.com/watch?v=83dMS3bcjJM

setwd("C:/Users/PC/Documents/host/host")
getwd()

install.packages("raster")
install.packages("sdm")
install.packages("dplyr")
install.packages("tidyr")
install.packages("mapview")
install.packages()

library(raster)
library(sdm)
library(dismo)
library(dplyr)
library(tidyr)
library(mapview)
library(raster)
library(jsonlite)

library(ggplot2)

spp <- read.csv("C:/Users/PC/Documents/host/host/xanthophloea.csv")

head(spp)

sppt <- spp %>% select(lon,lat)
sppt$species <- 1
sppt <- sppt[,c("lon", "lat","species")] 

head(sppt)

coordinates(sppt)<- c("lon","lat")

class(sppt)

head(sppt)

#--------------------
#bio <- raster::getData('worldclim',var='bio',res=5)

#bio

#names

biom <-list.files("C:/Users/PC/Documents/host/host/asci5", pattern = ".asc",full.names = TRUE)

biom

current <- stack(biom)


names(current)


#-------------------
library(usdm)
v1 <- vifstep(current)
v2<- vifcor(current,th=0.9)
v1

v2

biom <- exclude(current,v2)

plot(biom)


plot(biom[[13]])

points(sppt)

points(sppt,cex=0.5,pch=16)

proj4string(sppt)<- projection(raster())

mapView(sppt)


#-------------------------------
library(sdm)

d <- sdmData(species~., sppt,predictors = biom, bg = list(method="gRandom",n=1000))

getmethodNames()

m <- sdm(species~., d, methods =c("glm","BRT","Maxent","rf",'mars'),replication=c('sub','boot'),test.p=30,n=2)
m

gui(m)

g <- predict(m, biom, 'predictions29.img')
g

plot(g[[c(1,5,10,15.20)]])

enm <- ensemble(m,biom,'enms451.img',setting = list(method='weights',stat="AUC"))

plot(enm)

writeRaster(enm,"new_mkala275.tif",options = c("TWS=YES"))
raster("new_mkala275.tif") -> enm
plot(enm)

writeRaster(enm,"new_mkala426.asc",format ="ascii")

read.asciigrid("new_mkala426.asc") -> grid1

plot(grid1)


#---------------------------------------------------------------------------


#biof4570 <- raster::getData("CMIP5", var='bio', res=5, rcp=85, model='AC', year=50)

#names (biof8550)

# this step change biof85files to asci files using arc map, and save in a new file(biof8550)

biof8570 <- stack(list.files(path = "C:/Users/PC/Documents/host/host/8570ascii",pattern = ".asc$",full.names = TRUE))

names(biof8570)
names(biom)


names (biof8570) <- names(biom)

names (biof8570)

enmf <- ensemble(m,biof8570,'enmsf6855.img',setting = list(method='weights',stat="AUC"))

plot(enmf)

cl <-  colorRampPalette(c("red", "white", "blue"),space = "Lab")

plot(enmf,col=cl(200))

writeRaster(enmf,"new_mkala2664.tif",options = c("TWS=YES"))
raster("new_mkala2664.tif") -> enmf1
plot(enmf1)

writeRaster(enmf1,"new_mkala6553.asc",format ="ascii")

read.asciigrid("new_mkala6553.asc") -> grid1

plot(grid1)


plot(stack(enm,enmf))

mapview(stack(enm,enmf))

ch <- enmf-enm

plot(ch)

getEvaluation(m,stat ='AUC')

ev <- getEvaluation(m,stat = c('AUC','TSS', 'threshold'),opt = 2)

mean(ev$threshold)

pa <- raster(enm)

pa[] <- ifelse(enm[] >= 0.2532589, 1, 0)

pa

paf <- raster(enmf)

paf[] <- ifelse(enmf[] >=   0.2532589, 1, 0)

plot(paf)


pac <- paf-pa

plot(pac)

cl <- colorRampPalette(c("red", "gray", "blue"))

plot(pac,col=cl(3))

plot(pac)

#------------------------
rcurve(m)

plot(rcurve(m))

getVarImp(m)

plot(getVarImp(m))


niche(biom,enm,n=c('PE','AI'),col=cl(200))

