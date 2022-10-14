### Spthin procedure used
setwd("D:/ArcGIS/submission/sdmJ/milie")
library(sdm)
library(dismo)
library(dplyr)
library(tidyr)
library(mapview)
library(raster)
library(jsonlite)
sp <- gbif("Trichoneura","eleusinoides",download = T,geo = T,sp=F)
sp
write.csv(sp, file = "sp.csv")

######## After writing csv file remove extra information to remain with lat and long.
######## Then filter the data to remove duplicated coordinates. 

sp2 <- read.csv("C:/Users/Hp/Documents/sp/africana/sp.csv")

#Species thinning to a distance of preference

setwd("C:/Users/Hp/Documents/sp/thinningsp")
getwd()
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

##Analaysis for the species starts here after spthin########

setwd("C:/Users/Hp/Documents/ArcGIS/submission/host/modelingre")
getwd()
current<- list.files(path = "WCmaskc", pattern = ".asc",full.names = TRUE)
library(raster)
biom <- stack(current)

library(sdm)
library(dismo)
library(dplyr)
library(tidyr)
library(mapview)
library(rgdal)

spp <- read.csv("C:/Users/Hp/Documents/ArcGIS/submission/host/modelingre/sp1.csv")

head(spp)

sppt <- spp %>% select(lon,lat)
sppt$species <- 1
sppt <- sppt[,c("lon", "lat","species")] 

head(sppt)

coordinates(sppt)<- c("lon", "lat")

class(sppt)

head(sppt)

#--------------------

biom
names

plot(biom)

plot(biom[[1]])

points(sppt)

points(sppt,cex=0.5,pch=16)

proj4string(sppt)<- projection(raster())

mapView(sppt)


#-------------------------------
library(sdm)

d <- sdmData(species~., sppt,predictors = biom, bg = list(method="gRandom",n=1000))


getmethodNames()

m <- sdm(species~., d, methods =c("glm","maxent","rbf","svm",'mars'),replication=c('sub','boot'),test.p=30,n=2)
m

gui(m)


en <- ensemble(m,biom,'ens.img54',setting = list(method='weights',stat="AUC"))

plot(en)

writeRaster(en,"new_mkala22.tif",options = c("TWS=YES"))
raster("new_mkala22.tif") -> en1
plot(en1)

writeRaster(en1,"new_mkala33.asc",format ="ascii")

read.asciigrid("new_mkala33.asc") -> grid1

plot(grid1)


#---------------------------------------------------------------------------


# this step change biof85files to asci files using arc map, and save in a new file(biof8550)



biof8570 <- stack(list.files(path = "biof8570maskc", pattern = ".asc",full.names = TRUE))

names(biof8570)
names(biom)


names (biof8570) <- names(biom)

names (biof8570)


enf <- ensemble(m,biof8570,'ensf2.img',setting = list(method='weights',stat="AUC"))

plot(enf)

cl <-  colorRampPalette(c("red", "white", "blue"),space = "Lab")

plot(enf,col=cl(200))

writeRaster(enf,"new_mkala31.tif",options = c("TWS=YES"))
raster("new_mkala31.tif") -> enf1
plot(enf1)

writeRaster(enf1,"new_mkala31.asc",format ="ascii")

read.asciigrid("new_mkala31.asc") -> grid1

plot(grid1)


plot(stack(en,enf1))

mapview(stack(en,enf))

ch <- enf-en

plot(ch)

getEvaluation(m,stat ='AUC')

ev <- getEvaluation(m,stat = c('AUC','TSS', 'threshold'),opt = 2)

mean(ev$threshold)

pa <- raster(en)

pa[] <- ifelse(en[] >=   0.6325388, 1, 0)

pa

paf <- raster(enf)

paf[] <- ifelse(enf[] >=   0.6325388, 1, 0)

plot(paf)


pac <- paf-pa

plot(pac)

cl <- colorRampPalette(c("red", "darkgreen", "blue"))
cl <- colorRampPalette(c("red", "gray", "blue"))

plot(pac,col=cl(3))

plot(pac)

#------------------------
rcurve(m)

getVarImp(m)

plot(getVarImp(m))


niche(biom,en,n=c('bio15','bio13'),col=cl(200))


