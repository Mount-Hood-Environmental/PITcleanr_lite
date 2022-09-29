# Authors: Mark Roes
#
# Purpose: PITcleanr_lite workflow
#
# Created: August 16, 2022
#   Last modified: September 29, 200 by Mike A. 
#
# Notes:

# clear environment
rm(list = ls())

#------------------------
# load packages

# the following automatically loads and/or installs the required package from R CRAN
options(repos = structure(c(cran="https://ftp.osuosl.org/pub/cran/")))  

# list of packages for use w/ PITcleanr_lits
packages <- c("plyr","tidyverse","magrittr", "lubridate","esquisse","readr","janitor","nhdplusTools", "sf", "ggraph") 

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}  
lapply(packages, require, character.only=TRUE)

#------------------------
# load required functions
source("R/addDirectionWrap.r")
source("R/compress2.r")
source("R/nodeConfig.r")
source("R/readTagData.r")
source("R/obsWide.r")

#------------------------
# Read in and compress data
obs_all = readTagData() # Suggested comment by BO:"This reads data in from the "PITcleaner_lite/input/PTAGIS_data" folder. Be sure this exists and contains the two necessary tag queries."
config = nodeConfig()
obs_clean = compress2(obs_all, config)

#------------------------
# Write out cleaned and compressed data
write_csv(obs_clean, paste0('output/TagObs_Compressed_', Sys.Date(),'.csv'))
obs_wide = obsWide(obs_clean)
write_csv(obs_wide, paste0('output/TagObs_Wide_',Sys.Date(),'.csv'))

#------------------------
# Add directionality
obs_direct = addDirectionWrap(group_nodes = T, build_diagram = T, generate_map = F, downstream_site = "HYC", direction = 'd')
write_csv(obs_direct,paste0('output/TagObs_Directionality_', Sys.Date(),'.csv') )

#------------------------
# End of primary workflow


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

# END SCRIPT  
