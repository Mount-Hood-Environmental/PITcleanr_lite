# Authors: Mark Roes
#
# Purpose: PITcleanr_lite workflow
#
# Created: August 16, 2022
#   Last modified: Oct 12, 2022 by Mark Roes
#
# Notes:

# clear environment
rm(list = ls())

#------------------------
# load packages

# the following automatically loads and/or installs the required package from R CRAN
options(repos = structure(c(cran="https://ftp.osuosl.org/pub/cran/")))  

# list of packages for use w/ PITcleanr_lits
packages <- c("plyr","tidyverse","magrittr", "lubridate","esquisse","readr","janitor","nhdplusTools", "sf", "ggraph", "readxl") 

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}  
lapply(packages, require, character.only=TRUE)

#------------------------
# load required functions
source("R/addDirectionWrap.r")
source("R/compressWrap.r")
source("R/nodeConfig.r")
source("R/readTagData.r")
source("R/obsWide.r")

#------------------------
# Read in and compress data

obs_all = readTagData(filter.test.tags = TRUE, filter.to.ptagis = TRUE) # This reads data from the "input" folder. Be sure that files within the folder have appropriate naming conventions.
#PTAGIS files must include "PTAGIS" in the filename
#Biologic files must include "BIOLOGIC" in the filename
#log files must begin with the node name (ex. "NODENAME_xyz.log")
#Submersible files must begin with the node name and include "SUB" in the filename (ex. "NODENAME_SUB_xyz.xlsx" or "SUB2_xyz.xlsx")


obs_clean = compressWrap(obs_all, attributes_list = c("event_length_mm","event_weight_g")) #Cleans and compresses PIT tag observation data using PTAGIS data and the "site_metadata" configuration file

#------------------------
# Write out cleaned and compressed data
write_csv(obs_clean, paste0('output/TagObs_Compressed_', Sys.Date(),'.csv'))
obs_wide = obsWide(obs_clean)
write_csv(obs_wide, paste0('output/TagObs_Wide_',Sys.Date(),'.csv'))


#Components below are currently in development as of 12/13/22 - functionality is not guaranteed
#------------------------
# Add directionality
obs_direct = addDirectionWrap(group_nodes = TRUE, build_diagram = TRUE, generate_map = FALSE, downstream_site = "HYC", direction = 'd')
write_csv(obs_direct,paste0('output/TagObs_Directionality_', Sys.Date(),'.csv') )

# End of primary workflow


# ---- Additional outputs ----
# Get last detection for each tag to determine final paths
lastobs_direct = obs_direct %>%
  mutate(path.len = str_length(path)) %>%
  group_by(tag_code) %>%
  slice_max(max_det) %>%
  slice_max(path.len) %>%
  ungroup() %>%
  select(-path.len)

write_csv(lastobs_direct,
  paste0('output/TagObs_FinalPaths_', Sys.Date(), '.csv'))
