library(tidyverse)
library(rgeos)
library(scales)
library(fasterize)
projection <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
ext <- extent(-19, 52, -35, 30)
assign(paste0("elev_", "raw"), raster("elevation.tif"))
assign(paste0("elev_", "projected"),
projectRaster(elev_raw, crs=projection))
assign(paste0("elev_final"), elev_projected)       
occ_count(taxonKey = animalkey$kingdomkey)
occ_count()
#########################################################################
downloading GBIF data
#########################################################################
setwd("K:/SDM/Coordinates")
#########################################################################
library(dismo)
Spp<-gbif("Nymphaea","ampla",download = T,geo = T,sp = F)
class(Spp)
Spp$
  is.na(Spp$lon)
which(is.na(Spp$lon))
X<-which(is.na(Spp$lon))
Spp<-Spp[-X,]
X<-which(is.na(Spp$lat))
X
Spp$species<-13
Spp<-Spp[,c('lon','lat','species')]
coordinates(Spp)<-~lon + lat
DF<-data.frame(Spp)
write.csv(DF,file = "data.frame(sp)")
######################################################################################################################
#setting working directory, to where my data will be stored/the path will also be used to determine where the raster 
file conversion output will be stored
setwd("K:/SDM/Climatedata/Climatedataasc/Biodataasc")
######################################################################################################################
#Raster file conversion (the first path is the tiff file path and second is where the asc file fomat for maxcent input 
is stored)
################################################################################
setwd("K:/AFNYR/Bioclims")

#packages
library(rJava)
library(ENMeval)
library(raster)
library(dismo)
library(MASS)
library(sp)
library(rgdal)
library(ggplot2)
library(knitr)
library(spocc)
library(ENMeval)
library(ENMeval)
library(dismo)
file <- paste(system.file(package="dismo"), "K:/ENMprac/occ.csv", sep="")





	#convert the tiff fomat layers to ascii fomat
bio_19 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_19.tif", package="raster")
writeRaster(bio_19, filename="bio_19.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_18 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_18.tif", package="raster")
writeRaster(bio_18, filename="bio_18.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_17 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_17.tif", package="raster")
writeRaster(bio_17, filename="bio_17.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_16 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_16.tif", package="raster")
writeRaster(bio_16, filename="bio_16.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_15 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_15.tif", package="raster")
writeRaster(bio_15, filename="bio_15.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_14<- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_14.tif", package="raster")
writeRaster(bio_14, filename="bio_14.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_13 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_13.tif", package="raster")
writeRaster(bio_13, filename="bio_13.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_12 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_12.tif", package="raster")
writeRaster(bio_12, filename="bio_12.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_11 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_11.tif", package="raster")
writeRaster(bio_11, filename="bio_11.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_10 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_10.tif", package="raster")
writeRaster(bio_10, filename="bio_10.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_9 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_9.tif", package="raster")
writeRaster(bio_9, filename="bio_9.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_8 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_8.tif", package="raster")
writeRaster(bio_8, filename="bio_8.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_7 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_7.tif", package="raster")
writeRaster(bio_7, filename="bio_7.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_6 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_6.tif", package="raster")
writeRaster(bio_6, filename="bio_6.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_5 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_5.tif", package="raster")
writeRaster(bio_5, filename="bio_5.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_4 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_4.tif", package="raster")
writeRaster(bio_4, filename="bio_4.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_3 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_3.tif", package="raster")
writeRaster(bio_3, filename="bio_3.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_2 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_2.tif", package="raster")
writeRaster(bio_2, filename="bio_2.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
bio_1 <- raster("K:/AFNYR/Bioclims/wc2.1_5m_bio_1.tif", package="raster")
writeRaster(bio_1, filename="bio_1.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
elev <- raster("K:/AFNYR/Bioclims/wc2.1_5m_elev.tif", package="raster")
writeRaster(bio_1, filename="elev.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)


bio future<-raster::getData('CMIP5',var='bio',res=10,rcp=45,year=70,model='CC')


#read the ascii layer fomat 
bio_1 <- raster("bio_1.asc")
bio_2 <- raster("bio_2.asc")
bio_3 <- raster("bio_3.asc")
bio_4 <- raster("bio_4.asc")
bio_5 <- raster("bio_5.asc")
bio_6 <- raster("bio_6.asc")
bio_7 <- raster("bio_7.asc")
bio_8 <- raster("bio_8.asc")
bio_9 <- raster("bio_9.asc")
bio_10 <- raster("bio_10.asc")
bio_11 <- raster("bio_11.asc")
bio_12 <- raster("bio_12.asc")
bio_13 <- raster("bio_13.asc")
bio_14 <- raster("bio_14.asc")
bio_15 <- raster("bio_15.asc")
bio_16 <- raster("bio_16.asc")
bio_17 <- raster("bio_17.asc")
bio_18 <- raster("bio_18.asc")
bio_19 <- raster("bio_19.asc")
elev <- raster("elev.asc")

#stack them together and crop to your prefered location
#biolayers <- stack(bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, elev)
#names(biolayers)
#extent(-19,52,-35,35)
#e<-extent(-19,52,-35,35)
#names<-crop(biolayers,e)
#names
#plot(names)

#Crop layers to your desired extent?location
extent(-19,52,-35,35) 
e<-extent(-19,52,-35,35)
plot(env<-crop(bio_1,e))
plot(env<-crop(bio_2,e))
plot(env<-crop(bio_3,e))
plot(env<-crop(bio_4,e))
plot(env<-crop(bio_5,e))
plot(env<-crop(bio_6,e))
plot(env<-crop(bio_7,e))
plot(env<-crop(bio_8,e))
plot(env<-crop(bio_9,e))
plot(env<-crop(bio_10,e))
plot(env<-crop(bio_11,e))
plot(env<-crop(bio_12,e))
plot(env<-crop(bio_13,e))
plot(env<-crop(bio_14,e))
plot(env<-crop(bio_15,e))
plot(env<-crop(bio_16,e))
plot(env<-crop(bio_17,e))
plot(env<-crop(bio_18,e))
plot(env<-crop(bio_19,e))
plot(env<-crop(elev,e))

#Plot the croped layers
lay1<-plot(env<-crop(bio_1,e))
#Crop the layers and save a copy to the working directory. (Change the file name to avoid overwriting the previous layers).
library(raster)
writeRaster(env<-crop(bio_1,e), filename="cbio_1.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_2,e), filename="cbio_2.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_3,e), filename="cbio_3.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_4,e), filename="cbio_4.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_5,e), filename="cbio_5.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_6,e), filename="cbio_6.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_7,e), filename="cbio_7.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_8,e), filename="cbio_8.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_9,e), filename="cbio_9.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_10,e), filename="cbio_10.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_11,e), filename="cbio_11.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_12,e), filename="cbio_12.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_13,e), filename="cbio_13.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_14,e), filename="cbio_14.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_15,e), filename="cbio_15.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_16,e), filename="cbio_16.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_17,e), filename="cbio_17.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_18,e), filename="cbio_18.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(bio_19,e), filename="cbio_19.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
writeRaster(env<-crop(elev,e), filename="celev.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)

#load the croped layers 
library(raster)

cbio_1 <- raster("cbio_1.asc")
cbio_2 <- raster("cbio_2.asc")
cbio_3 <- raster("cbio_3.asc")
cbio_4 <- raster("cbio_4.asc")
cbio_5 <- raster("cbio_5.asc")
cbio_6 <- raster("cbio_6.asc")
cbio_7 <- raster("cbio_7.asc")
cbio_8 <- raster("cbio_8.asc")
cbio_9 <- raster("cbio_9.asc")
cbio_10 <- raster("cbio_10.asc")
cbio_11 <- raster("cbio_11.asc")
cbio_12 <- raster("cbio_12.asc")
cbio_13 <- raster("cbio_13.asc")
cbio_14 <- raster("cbio_14.asc")
cbio_15 <- raster("cbio_15.asc")
cbio_16 <- raster("cbio_16.asc")
cbio_17 <- raster("cbio_17.asc")
cbio_18 <- raster("cbio_18.asc")
cbio_19 <- raster("cbio_19.asc")
celev <- raster("celev.asc")

#now you can resample your rasters to be exactly like your target raster
cbio_1 <- resample(cbio_1, cbio_1)
cbio_2 <- resample(cbio_2, cbio_1)
cbio_3 <- resample(cbio_3, cbio_1)
cbio_4 <- resample(cbio_4, cbio_1)
cbio_5 <- resample(cbio_5, cbio_1)
cbio_6 <- resample(cbio_6, cbio_1)
cbio_7 <- resample(cbio_7, cbio_1)
cbio_8 <- resample(cbio_8, cbio_1)
cbio_9 <- resample(cbio_9, cbio_1)
cbio_10 <- resample(cbio_10, cbio_1)
cbio_11 <- resample(cbio_11, cbio_1)
cbio_12 <- resample(cbio_12, cbio_1)
cbio_13 <- resample(cbio_13, cbio_1)
cbio_14 <- resample(cbio_14, cbio_1)
cbio_15 <- resample(cbio_15, cbio_1)
cbio_16 <- resample(cbio_16, cbio_1)
cbio_17 <- resample(cbio_17, cbio_1)
cbio_18 <- resample(cbio_18, cbio_1)
cbio_19 <- resample(cbio_19, cbio_1)
celev <- resample(celev, cbio_1)

#testing if all layers are the same
#bbox(cbio_1); ncol(cbio_1); nrow(cbio_1) ; res(cbio_1)# include all ohers down


writeRaster(cbio_1, "rcbio_1.asc", overwrite = TRUE)
writeRaster(cbio_2, "rcbio_2.asc", overwrite = TRUE)
writeRaster(cbio_3, "rcbio_3.asc", overwrite = TRUE)
writeRaster(cbio_4, "rcbio_4.asc", overwrite = TRUE)
writeRaster(cbio_5, "rcbio_5.asc", overwrite = TRUE)
writeRaster(cbio_6, "rcbio_6.asc", overwrite = TRUE)
writeRaster(cbio_7, "rcbio_7.asc", overwrite = TRUE)
writeRaster(cbio_8, "rcbio_8.asc", overwrite = TRUE)
writeRaster(cbio_9, "rcbio_9.asc", overwrite = TRUE)
writeRaster(cbio_10, "rcbio_10.asc", overwrite = TRUE)
writeRaster(cbio_11, "rcbio_11.asc", overwrite = TRUE)
writeRaster(cbio_12, "rcbio_12.asc", overwrite = TRUE)
writeRaster(cbio_13, "rcbio_13.asc", overwrite = TRUE)
writeRaster(cbio_14, "rcbio_14.asc", overwrite = TRUE)
writeRaster(cbio_15, "rcbio_15.asc", overwrite = TRUE)
writeRaster(cbio_16, "rcbio_16.asc", overwrite = TRUE)
writeRaster(cbio_17, "rcbio_17.asc", overwrite = TRUE)
writeRaster(cbio_18, "rcbio_18.asc", overwrite = TRUE)
writeRaster(cbio_18, "rcbio_19.asc", overwrite = TRUE)
writeRaster(celev, "rcelev.asc", overwrite = TRUE)

# obtain resampled layers info-(Arcinfo)/testing if resambling was succesful
rcbio_1 <- raster("rcbio_1.asc")
bbox(rcbio_1); ncol(rcbio_1); nrow(rcbio_1) ; res(rcbio_1)
rcbio_2 <- raster("rcbio_2.asc")
bbox(rcbio_2); ncol(rcbio_2); nrow(rcbio_2) ; res(rcbio_2)
rcbio_3 <- raster("rcbio_3.asc")
bbox(rcbio_3); ncol(rcbio_3); nrow(rcbio_3) ; res(rcbio_3)
rcbio_4 <- raster("rcbio_4.asc")
bbox(rcbio_4); ncol(rcbio_4); nrow(rcbio_4) ; res(rcbio_4)
rcbio_5 <- raster("rcbio_5.asc")
bbox(rcbio_5); ncol(rcbio_5); nrow(rcbio_5) ; res(rcbio_5)
rcbio_6 <- raster("rcbio_6.asc")
bbox(rcbio_6); ncol(rcbio_6); nrow(rcbio_6) ; res(rcbio_6)
rcbio_7 <- raster("rcbio_7.asc")
bbox(rcbio_7); ncol(rcbio_7); nrow(rcbio_7) ; res(rcbio_7)
rcbio_8 <- raster("rcbio_8.asc")
bbox(rcbio_8); ncol(rcbio_8); nrow(rcbio_8) ; res(rcbio_8)
rcbio_9 <- raster("rcbio_9.asc")
bbox(rcbio_9); ncol(rcbio_9); nrow(rcbio_9) ; res(rcbio_9)
rcbio_10 <- raster("rcbio_10.asc")
bbox(rcbio_10); ncol(rcbio_10); nrow(rcbio_10) ; res(rcbio_10)
rcbio_11 <- raster("rcbio_11.asc")
bbox(rcbio_11); ncol(rcbio_11); nrow(rcbio_11) ; res(rcbio_11)
rcbio_12 <- raster("rcbio_12.asc")
bbox(rcbio_12); ncol(rcbio_12); nrow(rcbio_12) ; res(rcbio_12)
rcbio_13 <- raster("rcbio_13.asc")
bbox(rcbio_13); ncol(rcbio_13); nrow(rcbio_13) ; res(rcbio_13)
rcbio_14 <- raster("rcbio_14.asc")
bbox(rcbio_14); ncol(rcbio_14); nrow(rcbio_14) ; res(rcbio_14)
rcbio_15 <- raster("rcbio_15.asc")
bbox(rcbio_15); ncol(rcbio_15); nrow(rcbio_15) ; res(rcbio_15)
rcbio_16 <- raster("rcbio_16.asc")
bbox(rcbio_16); ncol(rcbio_16); nrow(rcbio_16) ; res(rcbio_16)
rcbio_17 <- raster("rcbio_17.asc")
bbox(rcbio_17); ncol(rcbio_17); nrow(rcbio_17) ; res(rcbio_17)
rcbio_18 <- raster("rcbio_18.asc")
bbox(rcbio_18); ncol(rcbio_18); nrow(rcbio_18) ; res(rcbio_18)
rcbio_19 <- raster("rcbio_19.asc")
bbox(rcbio_19); ncol(rcbio_19); nrow(rcbio_19) ; res(rcbio_19)
rcelev <- raster("rcelev.asc")
bbox(rcelev); ncol(rcelev); nrow(rcelev) ; res(rcelev)


#rcbio_1 to rcbio_19 & elev are our working biolayers (untill we isolate the ujncorrelated layers

  #test for collineality
  #stack the layers
biolayers <- stack(rcbio_1, rcbio_2, rcbio_3, rcbio_4, rcbio_5, rcbio_6, rcbio_7, rcbio_8, rcbio_9, rcbio_10, rcbio_11, rcbio_12, rcbio_13, rcbio_14, rcbio_15, rcbio_16, rcbio_17, rcbio_18, rcbio_19, rcelev)
names(biolayers)

#tools for testing corelation of layers

#install.packages("usdm")
library(usdm)
  #one method
vfs<-vifstep(biolayers)
vfs
  #second method
vfc<-vifcor(biolayers,0.75)
vfc

 #load african map
library("raster")
shp <- shapefile("K:/AFNYR/Analysis/Shapefiles1/Africa.shx")
shp
plot(shp)


rcbio_1 <- raster("rcbio_1.asc")
rcbio_2 <- raster("rcbio_2.asc")
rcbio_3 <- raster("rcbio_3.asc")
# <- raster("rcbio_4.asc")
#rcbio_5 <- raster("rcbio_5.asc")
#rcbio_6 <- raster("rcbio_6.asc")
rcbio_7 <- raster("rcbio_7.asc")
rcbio_8 <- raster("rcbio_8.asc")
rcbio_9 <- raster("rcbio_9.asc")
rcbio_10 <- raster("rcbio_10.asc")
rcbio_11 <- raster("rcbio_11.asc")
rcbio_12 <- raster("rcbio_12.asc")
rcbio_13 <- raster("rcbio_13.asc")
#rcbio_14 <- raster("rcbio_14.asc")
#rcbio_15 <- raster("rcbio_15.asc")
#rcbio_16 <- raster("rcbio_16.asc")
#rcbio_17 <- raster("rcbio_17.asc")
rcbio_18 <- raster("rcbio_18.asc")
rcbio_19 <- raster("rcbio_19.asc")
rcelev <- raster("rcelev.asc")
env <- stack(rcbio_1, rcbio_2, rcbio_3, rcbio_7, rcbio_8, rcbio_9, rcbio_10, rcbio_11, rcbio_12, rcbio_13, rcbio_18, rcbio_19, rcelev)
