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
packages <- c("plyr","tidyverse","readxl", "lubridate","esquisse","readr","janitor") 
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}  
lapply(packages, require, character.only=TRUE)

#-----------------------------
# load required functions
#-----------------------------
source("R/compress.r")

#-----------------------------
# load data inputs
#-----------------------------
#Mark data
mark = ldply(list.files('input/mark', pattern = '*tagging_detail.*\\.csv', full.names = T), read_csv) %>%
  mutate(`Mark Date MMDDYYYY` = mdy(`Mark Date MMDDYYYY`))

recap = ldply(list.files('input/mark', pattern = '*recapture_detail.*\\.csv', full.names = T), read_csv) %>%
  mutate(`Mark Date MMDDYYYY` = mdy(`Mark Date MMDDYYYY`))

#Observation data
#biologis-obtained observations
obs_bl = ldply(list.files('input/observation', pattern = '^0LL_tag.*\\.csv', full.names = T), read_csv)

#Ptagis-obtained observations
obs_pt = ldply(list.files('input/observation', pattern = '^Lemhi_TagHist.*\\.csv', full.names = T), read_csv)

#list of test tags
test_tags = ldply(list.files('input/mark', pattern = '^test_tags.*\\.csv', full.names = T), read_csv)

#read in site data
meta = ldply(list.files('input/site', pattern = '^site_metadata.*\\.xlsx', full.names = T), read_excel)


#-----------------------------
# Clean mark data
#-----------------------------

#CODE CHUNK BELOW NOT NECESSARY FOR PTAGIS-FRONTLOADED PROCESS ---- 
#Mark data is all unique tag codes from the mark df, along with 'recap' tag codes that are not present in the mark df. These were tagged above the RSTs in the mark df

#find # of tag codes in recap but not mark
length(setdiff(recap$`Tag Code`,mark$`Tag Code`))

dim(filter(recap, `Tag Code` %in% setdiff(recap$`Tag Code`,mark$`Tag Code`) & `Recap Count` ==1))

all_tags = unique(c(mark$`Tag Code`,recap$`Tag Code`))
write_csv(tibble(taglist), file = 'input/mark/Full_TagList.csv')

#So we want to pull out the first instance of each 'recap' that is actually a new tag record

#filter down the recap data set to the columns and records to join to the mark data
#This provides the initial tag list and allows us to identify the initial observations of recaps
mark_all = recap %>%
  filter(`Tag Code` %in% setdiff(recap$`Tag Code`,mark$`Tag Code`)) %>%
  group_by(`Tag Code`) %>%
  arrange(`Mark Date MMDDYYYY`) %>%
  slice(1L) %>%
  ungroup() %>%
  select(names(mark)[c(1,2,4:13)]) %>%
  bind_rows(mark)
  
  
#-----------------------------
# Clean and join observation data
#-----------------------------
obs_pt = obs_pt %<>%
  janitor::clean_names() %>%
  mutate(tag_type = "Fish") %>%
  select(-tag_type)

obs_bl_test = obs_bl %>%
  filter(tag %in% obs_pt$tag_code) %>% 
  left_join(meta %>%
              select(reader,site_code, site_type, array_type), by = c("reader")) %>%
  rename(tag_code = tag,
         `Event Date Time Value` = detected,
         `Event Site Code Value` = reader,
         `Antenna ID` = antenna,
        ) %>%
  mutate(`CTH Count` = 1) %>%
  left_join(obs_pt %>%
              select(tag_code, mark_species_name, mark_rear_type_name),
            by = c("tag_code"))
  # you'll need to add this column
  mutate(`Event Type Name` = "Observation") %>%
  janitor::clean_names() %>%
  mutate(antenna_group_configuration_value = 1) %>% #This list is for test tags and defines each tag as Test_Tag or Fish
  mutate(tag_type = ifelse(tag_code %in% test_tags$test_tags,
                           "Test_Tag",
                           "Fish"))

obs_all =

#-----------------------------
# Join site info
#-----------------------------
