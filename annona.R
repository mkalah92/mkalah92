#### set working directory####
# https://damariszurell.github.io/SDM-Intro/
setwd("D:/Three anona")

#Install packages ##
install.packages("geodata")
install.packages("remotes")
install.packages("terra")
##call the packages as below####
library(sdm)
library(usdm)
library(dismo)
library(tidyr)
library(mapview)
library(raster)
library(jsonlite)
library(devtools)
library(tidyverse)
library(sp)
library(maxnet)
#Read your CSV data as below and follow other steps##

ssp5 <- read.csv("D:/Three anona/combined.csv")

coordinates(ssp5) <- ~lon+lat

######don't run this it will overwrite all your data###
#ssp5 <- ssP %>% select(lon,lat)

#ssp5$species <- 3

#ssp5 <- ssp54[,c("lon", "lat","species")] 

#head(ssp5)

# install package sp and raster also and call it as below#

# class(ssp5)
# nrow(ssp5)

#--------------------
##Downloading variables fro m worldclim# 
#biom <- raster::getData('worldclim',var ='bio',res= 5)

#bioclimdat <- raster::getData(name = "worldclim", var = "bio", res = 2.5)

##-----------------------------------------------------------------------------
#ssP4 <- read.csv("D:/current/wc5/combined_species.csv")

###Read your worldclim files + addition envr variables in asc format which have same extent#

ENv <-list.files("D:/Three anona/NEWasci", pattern = ".asc",full.names = TRUE)

ENv

current <- stack(ENv)

names(current)

#-------------------
extracted <- raster::extract(current, ssp5)
v0  <- vifstep(extracted)

biom <- exclude(current, v0)
# 
# plot(biom[[2]])
# 
# plot(ssp5[ssp5$species == 'Asteranthe asterias', ], col = 'blue', pch = 24, add = TRUE)
# plot(ssp5[ssp5$species == 'Uvaria kirkii Oliv', ], col = 'red', pch = 16, add = T)
# plot(ssp5[ssp5$species == 'Uvariodendron kirkii Verdc.', ], col = 'black', pch = 12, add = T)
# #######Step for identifying min and max points for the species to help id developing specific area of extent#
# #min_max <- read.csv("D:/Three anona/combined.csv")
# #table(min_max$species)
#min(min_max$lat)
#min(min_max$lon)
#max(min_max$lat)
#max(min_max$lon)

#######this step is for croping your desired species to the exact point of area studY#####
#biom_crop <- crop(current[[13]], c(29, 42, -16, 0))
#plot(current[[3]])
#plot(biom_crop)
# 
# plot(ssp5[ssp5$species == 'Asteranthe asterias', ], col = 'blue', pch = 24, add = TRUE)
# plot(ssp5[ssp5$species == 'Uvaria kirkii Oliv', ], col = 'red', pch = 16, add = T)
# plot(ssp5[ssp5$species == 'Uvariodendron kirkii Verdc.', ], col = 'black', pch = 12, add = T)

#points(ssP4[ssP4$species == 'Asteranthe asterias', ], col = 'red', pch = 16) NB;No need to add true but do it in the firts line.

#How to save the Raster after cropping it to your study area####
#writeRaster(x = biom_crop, filename = 'C:/Users/Hp/Desktop/ann/maxent/bios.tif')

#check <- raster::stack('C:/Users/Hp/Desktop/ann/maxent/bios.tif')

### This command helps in identifying more areas with more species coordinates incase species you have are limited.
#Then go further ahead using google earth to obtain the coordinates#
#aloe_c <- dismo::gbif('uvaria', 'kirkii')

#aloe_c$cloc
###______-------------------------###
# points(ssp5,cex=0.5,pch=16)
# 
# proj4string(ssp5)<- projection(raster())
# 
# mapView(ssp5)
# 
# #----------------------------------------------------------
# #aloe_b <- dismo::gbif('Asteranthe', 'asterias')
# 
# aloe_b$cloc
# 
# #----------------------------------------------------------
# 
# #aloe_c<- dismo::gbif('Uvariodendron', 'kirkii')
# 
# aloe_c$cloc
# #----------------------------------------------------------
# 
# points(ssp5)
# 
# points(ssp5,cex=0.5,pch=16)
# 
# proj4string(ssp5)<- projection(raster())
# 
# mapView(ssp5)

#-------------------------------
# Two methods for cropping data one by uing all the stack biom or specify.

# (1) clim_crop <- raster::crop(biom, c(34, 40, -8, 0))
#  (2) d <- sdmData(species~., ssP4,predictors = clim_crop[[c(1:13)]], bg = list(method="gRandom",n=1000))

#d <- sdmData(sp~b15+NDVI+f(categoric1)+f(categoric2)+coords(x+y),train=df)

# Prepare training data ---------------------------------------------------

species <- as.data.frame(ssp5) # This is the species occurrence dataframe
predictors <- as.data.frame(extracted) # Tssi is the predictor layers dataframe.

# Combining the two above into one training dataframe

training <- cbind(species, predictors)

# Dropping multicollinear variables

training2 <- training %>% select(-c('bio6', 'bio11', 'bio1', 'bio7', 
                                    'bio12', 'bio5', 'bio16'))

# table(training2$dom_lu.asc) # Checking the number of records of each land use class

b <- as.data.frame(randomPoints(mask = biom[[1]], n = 1000, p = ssp5))
b <- b %>% rename(lon = x, lat = y)
b <- b %>% mutate(species = 0) %>% 
  select(species, lon, lat)
# Now we extract raster values for the 1000 pseudoabsence points

pseudo <- b
coordinates(pseudo) <- ~lon+lat
pseudo_extracted <- as.data.frame(raster::extract(biom, pseudo))
background <- cbind(b, pseudo_extracted)

# Getting pseudabsence points
# Sample random points from the raster for each species
# Thin the random points 
# Add them as a test dataframe in the sdmData function


# # Convert categorical variables as factors in advance
 training3 <- training2
training3$dom_lu <- as.factor(training3$dom_lu)
 str(training3) # Check whether all your categorical variables are stored as factors
 d3 <- sdmData(species~.+coords(lon+lat), training3) 
# # We have all the three species in one column so species is enough 
# # Otherwise specify their names separated with "+"
 d3

getmethodNames()

training2 <- training2 %>% mutate(species = case_when(species == "Asteranthe asterias" ~ "Asteranthe",
                                                      species == "Uvaria kirkii Oliv" ~ "Uvaria",
                                                      species == "Uvariodendron kirkii Verdc." ~ "Kirkii")) %>% 
  rename(population = pop_density2015.2000) %>%
  rename(et0 = et0_v3_yr) %>% 
  rename(ai = ai_v3_yr) %>% 
  rename(land_cover = dom_lu)

names(background) <- names(training2)
dim(training2)
dim(background)
background <- background %>% mutate(species = 0) # Zero denotes psedoabsence
coordinates(background) <- ~ lon+lat
coordinates(training2) <- ~lon+lat

d2 <- sdmData(species ~., training2, bg = background) # Specify factor variables explicitly in the formula
d2
m4 <- sdm(Asteranthe + Uvaria +Kirkii~.,d2, methods =c("rf","MAXENT"),
          replication=c('sub','boot'),test.p=30,n=2)
m4

getVarImp(m4)

# Plot roc for the species 

roc(m4, species = "Asteranthe", smooth = T)
roc(m4, species = "Kirkii", smooth = T)
roc(m4, species = "Uvaria", smooth = T)

# Save the model in the working directory
write.sdm(x = m4, filename = "sdm_all_species", overwrite = T)
m5 <- read.sdm(filename = 'sdm_all_species.sdm')
m5

gui(m5)
names(biom)<- names(training2[2:11])# kulainisha majina  yaendane kutoka kwa biom na yenye yako kwa training2.

g <- predict(m2, biom, 'predictions29.img')
g

plot(g[[c(1,5,10,15.20)]])

enm <- ensemble(m5,biom,'Uvariacu70.img',setting = list(id=1:40,method='weighted',stat="AUC"))

# First species = 1:16
# Second species = 17:32
# Third species = 33:48

plot(enm)


writeRaster(enm,"Uvariacu.tif",options = c("TWS=YES"))
raster("Uvariacu.tif") -> enm
plot(enm)

writeRaster(enm,"Uvariacu.asc",format ="ascii")

read.asciigrid("Uvariacu.asc") -> grid1

plot(grid1)


#---------------------------------------------------------------------------
# download downscaled cmp5 for future
#remotes::install_github("rspatial/geodata")


#biof4550 <- raster::getData("CMIP5", var='bio', res=2.5, rcp=45, model='AC', year=50)
#biof4570 <- raster::getData("CMIP5", var='bio', res=2.5, rcp=45, model='AC', year=70)
#biof8550 <- raster::getData("CMIP5", var='bio', res=2.5, rcp=85, model='AC', year=50)
#biof8570 <- raster::getData("CMIP5", var='bio', res=2.5, rcp=85, model='AC', year=70)
#getwd()
#names (biof8550)

biof4570 <- stack(list.files(path = "D:/Three anona/ACCESS/ACC/ASCI370_2060_2080",pattern = ".asc$",full.names = TRUE))
#A_ssp370_60_2 <- raster("D:/Three anona/Bios_croped/370ssp4160/A_ssp370_2041-2060.tif")
#plot(A_ssp370_60_2)

#writeRaster(A_ssp370_60_2, "D:/Three anona/Bios_croped/370ssp4160/A_ssp370_2041-2060.asc", format="ascii")
#A_ssp370_60 <- raster("D:/Three anona/Bios_croped/370ssp4160/A_ssp370_2041-2060.asc")
names(biof4570)
names(biom)
#enmf <- ensemble(m3,A_ssp370_60,'asteranthe3.tif',setting = list(method='weights',stat="AUC"))

names (biof4550) <- names(biom)

names (biof4550)

enmf <- ensemble(m5,biof4550,'Uvariafu.img',setting = list(id= 9:16,method='weighted',stat='AUC',opt=2))

plot(enmf)

cl <-  colorRampPalette(c("red", "white", "blue"),space = "Lab")

plot(enmf,col=cl(200))

writeRaster(enmf,"Uvariafu.tif",options = c("TWS=YES"))
raster("Uvariafu.tif") -> enmf1
plot(enmf1)

writeRaster(enmf1,"Uvariafu.asc",format ="ascii")

read.asciigrid("Uvariafu.asc") -> grid1

plot(grid1)

plot(stack(enm,enmf))

mapview(stack(enm,enmf))

ch <- enmf-enm

writeRaster(ch,"new_mkalac.asc",format ="ascii")

read.asciigrid("new_mkalac.asc") -> ch1

plot(ch1)

getEvaluation(m5,stat ='AUC')

ev <- getEvaluation(m5,stat = c('AUC','TSS', 'threshold'),opt = 2)

mean(ev$threshold)

pa <- raster(enm)

pa[] <- ifelse(enm[] >=     0.2509915, 1, 0)

plot(pa)

paf <- raster(enmf)

paf[] <- ifelse(enmf[] >=   0.2509915, 1, 0)

plot(paf)


pac <- paf-pa


cl <- colorRampPalette(c("red", "gray", "blue"))

plot(pac,col=cl(3))

plot(pac)

#------------------------

rcurve(m3,id = 1:8,method=maxent, smooth = T)
plot(rcurve(m3))

getVarImp(m5)

plot(getVarImp(m5))


niche(biom,enm,n=c('bio2','bio17'),col=cl(200))



