# Authors: Mark Roes
#
# Purpose: PITcleanr_lite workflow
#
# Created: August 16, 2022
#   Last modified: Jan 23, 2023 by Mark Roes
#
# Notes:

# clear environment
rm(list = ls())

#------------------------
# load required functions and packages
source("R/packageLoadInstall.r")
source("R/readTagData.r")
source("R/compressWrap.r")
source("R/obsWide.r")
source("R/nodeConfig.r")
source("R/addDirectionWrap.r")
source("R/finalPaths.R")

packageLoadInstall()

#------------------------
# Read in and compress data

obs_all = readTagData(filter_test_tags = TRUE,
                      filter_to_ptagis = TRUE) 
# This reads data from the "input" folder. Be sure that files within the folder have appropriate naming conventions.
#PTAGIS files must include "PTAGIS" in the filename
#Biologic files must include "BIOLOGIC" in the filename
#log files must begin with the node name (eg. "NODENAME_xyz.log")
#Submersible files must begin with the node name and include "SUB" in the filename (eg. "NODENAME_SUB_xyz.xlsx" or "SUB2_xyz.xlsx")


obs_clean = compressWrap(obs_all, attributes_list = c("event_length_mm","event_weight_g"))

#------------------------
# Write out cleaned and compressed data
write_csv(obs_clean, paste0('output/TagObs_Compressed_', Sys.Date(),'.csv'))

#Observations pivoted wide
obs_wide = obsWide(obs_clean)

write_csv(obs_wide, paste0('output/TagObs_Wide_',Sys.Date(),'.csv'))

#------------------------
# Add directionality
obs_direct = addDirectionWrap(generate_map = FALSE, downstream_site = "HYC")

write_csv(obs_direct,paste0('output/TagObs_Directionality_', Sys.Date(),'.csv') )

final_paths = finalPaths(obs_direct, calculate_restoration_use = TRUE)

write_csv(final_paths,
  paste0('output/TagObs_FinalPaths_', Sys.Date(), '.csv'))
 