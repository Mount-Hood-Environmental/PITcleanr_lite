# Authors: Mark Roes
#
# Purpose: Basic outline to determine data formats and functions necessary for PITcleanr-lite
#
# Initially created: August 9, 2022
#   Last Modified: 
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load packages
#-----------------------------
#Code below automatically loads and/or installs required packages from CRAN
options(repos=structure(c(cran="https://ftp.osuosl.org/pub/cran/")))  
packages <- c("plyr","tidyverse","readxl", "lubridate") 
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}  
lapply(packages, require, character.only=TRUE)

#-----------------------------
# load required functions
#-----------------------------


#-----------------------------
# load data inputs
#-----------------------------
#Mark data
mark = ldply(list.files('input/mark', pattern = '*tagging_detail.*\\.csv', full.names = T), read_csv) %>%
  mutate(`Mark Date MMDDYYYY` = mdy(`Mark Date MMDDYYYY`))

recap = ldply(list.files('input/mark', pattern = '*recapture_detail.*\\.csv', full.names = T), read_csv) %>%
  mutate(`Mark Date MMDDYYYY` = mdy(`Mark Date MMDDYYYY`))

#Observation data
#biomark-obtained observations
obs_bm = ldply(list.files('input/observation', pattern = '^0LL_tag.*\\.csv', full.names = T), read_csv)

#Ptagis-obtained observations
obs_pt = ldply(list.files('input/observation', pattern = '*tag_history.*\\.csv', full.names = T), read_csv)

#read in site data
locs = ldply(list.files('input/site', pattern = '^Litz_Locations*\\.csv', full.names = T), read_csv)

meta = ldply(list.files('input/site', pattern = '^site_metadata.*\\.xlsx', full.names = T), read_excel)


#-----------------------------
# Clean mark data
#-----------------------------
#Mark data is all unique tag codes from the mark df, along with 'recap' tag codes that are not present in the mark df. These were tagged above the RSTs in the mark df

#find # of tag codes in recap but not mark
length(setdiff(recap$`Tag Code`,mark$`Tag Code`))

dim(filter(recap, `Tag Code` %in% setdiff(recap$`Tag Code`,mark$`Tag Code`) & `Recap Count` ==1))

#So we want to pull out the first instance of each 'recap' that is actually a new tag record

#filter down the recap data set to the columns and records to join to the mark data
mark_all = recap %>%
  filter(`Tag Code` %in% setdiff(recap$`Tag Code`,mark$`Tag Code`)) %>%
  group_by(`Tag Code`) %>%
  arrange(`Mark Date MMDDYYYY`) %>%
  slice(1L) %>%
  ungroup() %>%
  select(names(mark)[c(1,2,4:13)]) %>%
  bind_rows(mark)
  
  
#-----------------------------
# Join observations
#-----------------------------


#-----------------------------
# Join site info
#-----------------------------
