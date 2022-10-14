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
###################################
# Install Wallace from GitHub
install.packages("remotes")
remotes::install_github("wallaceEcoMod/wallace")
# Load Wallace
library(wallace)
# Run Wallace
run_wallace()

