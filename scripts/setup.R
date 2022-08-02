# Authors: Mike Ackerman
#
# Purpose: Start to explore the complete OTG dataset and begin
# to roll up measurements into habitat reach metrics
#
# Initially created: April 12, 2022
#   Last Modified: April 21, 2022
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# install PITcleanr
#-----------------------------
remotes::install_github("KevinSee/PITcleanr@develop",
                        build_vignettes = T,
                        force = T)
library(PITcleanr)
browseVignettes("PITcleanr")

#-----------------------------
# load necessary libraries
#-----------------------------


