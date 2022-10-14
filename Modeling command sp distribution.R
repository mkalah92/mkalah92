setwd("D:/Three anona")
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

install.packages ('foreign')
install.packages ('magrittr')
install.packages ('rasterVis')
install.packages ('rgdal')
install.packages ('gridExtra')


# data processing
library(foreign) 
# for reading dbfs
library(dplyr)
library(magrittr)
library(tidyr) 
library(ggplot2)
library(gridExtra) 
# to arrange grid plots
#https://www.youtube.com/watch?v=83dMS3bcjJM&ab_channel=Biogeoinformatics
# spatial
library(raster)
library(rasterVis)
library(rgdal)
library(dismo) 


# data location
url<-"http://qgis.org/downloads/data/qgis_sample_data.zip"

mydir<-"c:\\Users/Hp/Desktop/ann/maxent"
temp<-tempfile(tmpdir=mydir, fileext=".zip")
download.file(url, temp)
unzip(temp, exdir=mydir)
unlink(temp) #delete the zip file

# Grab the name of the file path
fpath<-list.files(path = mydir, full.names = TRUE, pattern = "qgis_sample_data")
fpath<-gsub("/", "\\\\", fpath)

# Read in landcover raster
landusepath<-paste(fpath, "raster\\landcover.img", sep="\\")
landuse.raw<-raster(landusepath)
plot(landuse.raw, axes=FALSE)

vals<-unique(values(landuse.raw))
recl<-matrix(c(vals, c(0, rep(1, 6), 9, 1,1, 13)),ncol=2)
recl

landuse<-reclassify(landuse.raw, rcl=recl)
plot(landuse, legend=FALSE, axes=FALSE)

# Regions polygon shapefile
regionpath<-paste(fpath, "shapefiles", sep="\\")
region<-readOGR(dsn=regionpath, layer="regions") 

# we will use ggplot to plot the regions
ggplot()+geom_polygon(data=region,  aes(x=long, y=lat, group=group), 
                      fill="cadetblue", color="grey")+
  coord_equal()+xlim(c(-5000000, 5000000))+ylim(c(1000000, 8000000))

# Create a subset with our regions of interest
myregions<-c( "Anchorage", "Yukon-Koyukuk", "North Slope")
region.sm<-region[region$NAME_2 %in% myregions,]

# let's map those pieces so you can see the result. Since I just
# want the raster with no legend/axes etc I'm creating a function
# to strip the plot

nakedMap<-function(dat, title=""){
  gplot(dat)+geom_tile(aes(fill=value))+
    ggtitle(title)+
    coord_equal()+ 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) +
    theme(line = element_blank(),
          line = element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          legend.position = "none")
}

# crop, rasterize and mask 
cr<-crop(landuse, region.sm)
fr<-rasterize(region.sm, cr)
lr<-mask(x=cr, mask=fr)


cr.plot<-nakedMap(cr, title="Cropped")
fr.plot<-nakedMap(fr, title="Regions")
lr.plot<-nakedMap(lr, title="Masked")

grid.arrange(cr.plot, fr.plot, lr.plot, ncol=3) 

#centroids for the labels
centroids<-cbind(coordinates(region.sm), region.sm@data)
names(centroids)[1:2]<-c("x", "y")
Env <- load_var(system.file('extdata',  package = 'SSDM'), categorical = 'sSUBSTRATE', verboe = FALSE)
# use gplot (not ggplot) from rasterVis
# geom_tile adds the raster, geom_polygon adds the regions
# geom_text adds the labels at the centroids
gplot(lr)+
  geom_tile(aes(fill=factor(value, labels=c("Water", "Green", "Shrubland", "Urban"))), alpha=0.8)+
  scale_fill_manual(values = c("steelblue3", "forestgreen", "ghostwhite", "red"),
                    name= "Land use code")+
  geom_polygon(data=region.sm, aes(x=long, y=lat, group=group), 
               fill=NA,color="grey50", size=1)+
  geom_text(data=centroids, aes(x=x, y=y, label=NAME_2), fontface="bold")+
  coord_equal()

# Extract the values of the landcover raster for each zone. 
# This produces a list of raster cells for each region

# You can do the same calculation using the full state raster
# 
ext<-extract(raster, region.sm, method='simple')# this takes a little time
ext<-extract(lr, region.sm, method='simple')
class(ext)  # a list
## [1] "list"
length(ext) # three elements, a vector of land use values for each region
## [1] 3

# Function to tabulate land use by region and return 
# a data.frame
tabFunc<-function(indx, extracted, region, regname) {
  dat<-as.data.frame(table(extracted[[indx]]))
  dat$name<-region[[regname]][[indx]]
  return(dat)
}


# run through each region and compute a table of the count
# of raster cells by land use. Produces a list (see below)
tabs<-lapply(seq(ext), tabFunc, ext, region.sm, "NAME_2")
tabs
## [[1]]
##   Var1 Freq      name
## 1    0  241 Anchorage
## 2    1 4260 Anchorage
## 3   13   37 Anchorage
## 
## [[2]]
##   Var1   Freq        name
## 1    0   8819 North Slope
## 2    1 226463 North Slope
## 
## [[3]]
##   Var1   Freq          name
## 1    0   6069 Yukon-Koyukuk
## 2    1 347948 Yukon-Koyukuk
## 3    9  27052 Yukon-Koyukuk

# assemble into one data frame
tabs<-do.call("rbind",tabs )

# name the land uses
tabs$Var1<-factor(tabs$Var1, levels=c(0,1,9,13), labels=c("Water", "Green", "Shrubland", "Urban"))

# use the spread function from tidyr to make nicer
tabs%>%
  group_by(name) %>% # group by region
  mutate(totcells=sum(Freq), # how many cells overall
         percent.area=round(100*Freq/totcells,2)) %>% #cells by landuse/total cells
  dplyr::select(-c(Freq, totcells)) %>% # there is a select func in raster so need to specify
  spread(key=Var1, value=percent.area, fill=0) # make wide format
## Source: local data frame [3 x 5]
## 
##            name Water Green Shrubland Urban
## 1     Anchorage  5.31 93.87       0.0  0.82
## 2   North Slope  3.75 96.25       0.0  0.00
## 3 Yukon-Koyukuk  1.59 91.31       7.1  0.00






#use package gridExtra







#remove.packages("raster")
#if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

#BiocManager::install("raster")


#remove.packages("dismo")

#if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

#BiocManager::install("dismo")


#install.packages("tripack", dependencies = TRUE, INSTALL_opts = '--no-lock')

#options ("repos" = c ("CRAN" = "https://cran.rstudio.com"))

#install.packages ('dismo')

#installed.packages("tripack", dependencies=TRUE, INSTALL_opts = c('--no-lock')).



spt3 <- gbif("Aloe","penduliflora",download = T,geo = T,sp=F)
spt3
write.csv(spt3, file = "spt3.csv")


######## After writing csv file remove extra information to remain with lat and long.
######## Then filter the data to remove duplicated coordinates. 

#sp2 <- read.csv("C:/Users/Hp/Documents/sp/africana/sp.csv")

#Species thinning to a distance of preference

#getwd()

#Packages
library(spThin)
library(dplyr)

#Read file
sp <- read.csv("D:/Three anona/combined.csv")

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
           out.dir="D:/Three anona/study1",
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

setwd("D:/Three anona/study1")
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

#library(ggplot2)

#installAll() 
fnames <- list.files()
csv <- lapply(fnames, read.csv)
result <- do.call(rbind, csv)

ssp3 <- list.files(pattern = ".csv",full.names = T)%>%
  map_df(~read.csv(.,col.names = cols(.default = "c")))


# spp <- read.csv("C:/Users/Hp/Desktop/ann/maxent/spt1.csv")

sp <- read.csv("D:/Three anona/combined.csv")

head(sp)

library(sp)
library(raster)
coordinates(sp)<- c("lon","lat")

class(sp)


#--------------------
#biom <- raster::getData('worldclim',var ='bio',res= 5)

#bioclimdat <- raster::getData(name = "worldclim", var = "bio", res = 2.5)

#min_max <- read.csv("C:/Users/Hp/Desktop/ann/maxent/combined_species.csv")
#table(min_max$species)
#min(min_max$lat)
#min(min_max$lon)
#max(min_max$lat)
#max(min_max$lon)

Biom

current <- stack(Biom)

names(current)

##plot(current[[3]])
#plot(biom_crop[[3]])

#plot(biom2[[3]])
points(spp3)

#plot(ssp3[ssp3$species == 'Aloe ballyi', ], col = 'blue', pch = 24, add = TRUE)
#plot(ssp3[ssp3$species == 'Aloe classenii', ], col = 'red', pch = 16, add = T)
#plot(ssp3[ssp3$species == 'Aloe penduliflora', ], col = 'black', pch = 12, add = T)

#points(ssp3[ssp3$species == 'Aloe classenii', ], col = 'red', pch = 16) NB;No need to add true but do it in the firts line.

#writeRaster(x = biom_crop, filename = 'C:/Users/Hp/Desktop/ann/maxent/bios.tif')

#check <- raster::stack('C:/Users/Hp/Desktop/ann/maxent/bios.tif')

 #aloe_c <- dismo::gbif('Aloe', 'classenii')

 #aloe_c$cloc

#points(sppt,cex=0.5,pch=16)

#proj4string(sppt)<- projection(raster())

#mapView(sppt)

#aloe_c <- dismo::gbif('Aloe', 'classenii')

#bio

#names

biom <-list.files("D:/Three anona/NEWasci", pattern = ".asc",full.names = TRUE)

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

d <- sdmData(sp~b15+NDVI+f(categoric1)+f(categoric2)+coords(x+y),train=df) 

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

writeRaster(enm,"new_mkala34.asc",format ="ascii")

read.asciigrid("new_mkala34.asc") -> grid1

plot(grid1)


#---------------------------------------------------------------------------


#biof4570 <- raster::getData("CMIP5", var='bio', res=5, rcp=85, model='AC', year=50)

#names (biof8550)

# this step change biof85files to asci files using arc map, and save in a new file(biof8550)

biof4570 <- stack(list.files(path = "C:/Users/Hp/Documents/ArcGIS/submission/XG/biomasci/4570asci",pattern = ".asc$",full.names = TRUE))

names(biof4570)
names(biom)


names (biof4570) <- names(biom)

names (biof4570)

enmf <- ensemble(m,biof4570,'enmsf3.img',setting = list(method='weights',stat="AUC"))

plot(enmf)

cl <-  colorRampPalette(c("red", "white", "blue"),space = "Lab")

plot(enmf,col=cl(200))

writeRaster(enmf,"new_mkala337.tif",options = c("TWS=YES"))
raster("new_mkala337.tif") -> enmf1
plot(enmf1)

writeRaster(enmf1,"new_mkala337.asc",format ="ascii")

read.asciigrid("new_mkala337.asc") -> grid1

plot(grid1)


plot(stack(enm,enmf))

mapview(stack(enm,enmf))

ch <- enmf-enm

plot(ch)

getEvaluation(m,stat ='AUC')

ev <- getEvaluation(m,stat = c('AUC','TSS', 'threshold'),opt = 2)

mean(ev$threshold)

pa <- raster(enm)

pa[] <- ifelse(enm[] >=  0.24214, 1, 0)

pa

paf <- raster(enmf)

paf[] <- ifelse(enmf[] >=  0.24214, 1, 0)

plot(paf)


pac <- paf-pa

plot(pac)

cl <- colorRampPalette(c("red", "darkgreenm", "blue"))
cl <- colorRampPalette(c("red", "gray", "blue"))

plot(pac,col=cl(3))

plot(pac)

#------------------------
rcurve(m)

plot(rcurve(m))

getVarImp(m)

plot(getVarImp(m))


niche(biom,enm,n=c('PE','bio9'),col=cl(200))



#biom <-list.files("C:/Users/PC/Documents/host/host/asci5", pattern = ".asc",full.names = TRUE)

#biom

#current <- stack(biom)


#names(current)

 #-------------------------------
library(sdm)

d <- sdmData(species~., sppt,predictors = biom, bg = list(method="gRandom",n=1000))


getmethodNames()

m <- sdm(species~., d, methods =c("glm","maxent","BRT","RF"),replication=c('sub','boot'),test.p=30,n=2)
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

