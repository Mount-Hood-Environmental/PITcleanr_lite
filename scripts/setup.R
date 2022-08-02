# Authors: Mike Ackerman
#
# Purpose: 
#
# Initially created: August 2, 2022
#   Last Modified: 
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


