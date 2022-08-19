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
packages <- c("plyr","tidyverse","magrittr", "lubridate","esquisse","readr","janitor","nhdplusTools") 
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
write_csv(obs_clean, paste0('output/TagObs_Compressed_', Sys.Date(),'.csv'))

#-----------------------------
# Add directionality
#-----------------------------

obs_direct = addDirection2()

write_csv(obs_direct,paste0('output/TagObs_Directionality_', Sys.Date(),'.csv') )

#End of primary workflow


#-----------------------------
# Script below is in development
#-----------------------------


#Automating the directionality process -----

#Obtaining geo points for all node locations
source("R/queryPtagisMeta.R")
source("R/queryInterrogationMeta.R")
source("R/queryInterrogationConfig.R")
source("R/queryMRRMeta.R")
nodedat = queryPtagisMeta()
write_csv(nodedat, "input/metadata/ptagis_node_locs.csv")

#Read in this data with the site metadata
nodes_pt = read_csv("input/metadata/ptagis_node_locs.csv")
nodes_bl = read_csv("input/metadata/site_metadata.csv")
node_names = read_csv("input/metadata/node_config.csv")

nodes_bl %<>%
  mutate(reader = as.character(reader)) %>%
  left_join(node_names, by = c("reader"))

#bind together and filter to detections
node_locs = nodes_pt %>%
  rename(node = site_code,
         lat = latitude,
         lon = longitude) %>%
  select(node, lat, lon) %>%
  bind_rows(nodes_bl %>%
              select(node, lat, lon)) %>%
  filter(node %in% obs_clean$node) %>%
  distinct()

#Building flowlines



#Creating parent-child relationships

#Getting directionality