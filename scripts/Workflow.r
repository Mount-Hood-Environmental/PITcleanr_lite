# Authors: Mark Roes
#
# Purpose: PITcleanr_lite workflow
#
#Created: August 16, 2022
#Modified: 
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load packages
#-----------------------------
#Code below automatically loads and/or installs required packages from CRAN
options(repos=structure(c(cran="https://ftp.osuosl.org/pub/cran/")))  
packages <- c("plyr","tidyverse","magrittr", "lubridate","esquisse","readr","janitor") 
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}  
lapply(packages, require, character.only=TRUE)

#-----------------------------
# load required functions
#-----------------------------
source("R/addDirection2.r")
source("R/compress2.r")
source("R/nodeConfig.r")
source("R/readTagData.r")

#-----------------------------
# Read in and compress data
#-----------------------------

obs_all = readTagData()

config = nodeConfig()

obs_clean = compress2(obs_all, config)

#Write out cleaned and compressed data
write_csv(obs_clean, paste0('output/TagObs_compressed_', Sys.Date(),'.csv'))

#-----------------------------
# Add directionality
#-----------------------------

obs_direct = addDirection2()

write_csv(obs_direct,paste0('output/TagObs_Directionality_', Sys.Date(),'.csv') )
