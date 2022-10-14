
setwd("C:/Users/Hp/Desktop/NCBI submission/NCBIsubmission")
install.packages("devtools")
path.package('rlang')
remove.packages(usethis)
remove.packages(rlang)
remove.packages(rlang)
install.packages("usethis")
library(devtools)
library(usethis)
library(usethis)
library(dplyr)
install_github("TARobison/ReFernment")
#add the above commands if this is your first time using ReFernment

library("ReFernment")

install.packages("usethis", repos=c("http://rstudio.org/_packages",
                                         "http://cran.rstudio.com",dependencies=TRUE))
genomes <- c("Pellaea_truncata", "Aleuritopteris_albom")

# declare a vector containing the path to the input GFF3 files and another vector to the location where we would like the output files to be saved.

gffFolder <- "C:/Users/Hp/Desktop/NCBI submission/NCBIsubmission/files/"

outputFolder <- "C:/Users/Hp/Desktop/NCBI submission/NCBIsubmission/gbr/"
gbFolderPath <- "C:/Users/Hp/Desktop/NCBI submission/NCBIsubmission/saina/"
fastaFolderPath <- "C:/Users/Hp/Desktop/NCBI submission/NCBIsubmission/fasta/"

#we simply call the ReFernment function and wait for it to finish. This can take several minutes if you have a large number of plastomes. Note that the operation of ReFernment is the same if you are also providing gb and/or fasta files, just add the required variables.
ReFernment(gffFolder, fastaFolderPath, gbFolderPath, outputFolder, genomes)

#As ReFernment runs, it may produce one or more of the following warning messages:

# There are a high number of edited Stops ( [number] ) in [geneName] manually check to make sure frame is correct
# There seems to be a problem with the protein sequence of ( [protein name] ) please check its sequence in the protein fasta manually

