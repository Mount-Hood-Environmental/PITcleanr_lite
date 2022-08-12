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
source("R/addDirection.r")
source("R/compress.r")
source("R/buildNodeOrder.r")

#-----------------------------
# load data inputs
#-----------------------------
#Mark data
mark = ldply(list.files('input/mark', pattern = '*tagging_detail.*\\.csv', full.names = T), read_csv)

recap = ldply(list.files('input/mark', pattern = '*recapture_detail.*\\.csv', full.names = T), read_csv)

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

obs_pt %<>%
  janitor::clean_names() %>%
  mutate(tag_type = ifelse(tag_code %in% test_tags$test_tags,
                           "Test_Tag",
                           "Fish"),
         event_date_time_value = mdy_hms(event_date_time_value))

obs_bl_test = obs_bl %>%
  filter(tag %in% obs_pt$tag_code) %>%
  left_join(meta %>%
              select(reader,site_code, site_type, array_type), by = c("reader")) %>%
  rename(tag_code = tag,
         event_date_time_value = detected,
         event_site_code_value = reader,
         antenna_id = antenna) %>%
  mutate(cth_count = 1,
         event_type_name = "Observation",
         antenna_group_configuration_value = 1,
         event_site_type_description = "Instream Remote Detection System") %>%
  janitor::clean_names() %>%
  left_join(obs_pt %>%
              select(tag_code, mark_species_name, mark_rear_type_name),
            by = c("tag_code")) %>%
  mutate(tag_type = ifelse(tag_code %in% test_tags$test_tags,
                           "Test_Tag",
                           "Fish")) %>%
  select(-site, -site_code)

obs_all = obs_bl_test %>%
  bind_rows(obs_pt) %>%
  filter(tag_type == "Fish")

#-----------------------------
# Create configuration for compress()
#-----------------------------
config = obs_all %>%
  # first, for example, '16' '17' '18' are re-coded into a single node 'Upstream Array'
  mutate(node = ifelse(event_site_code_value %in% c('16','17'),
                       'SSR Entrance',NA),
         node = ifelse(event_site_code_value %in% c('18'),
                       'SSR Exit', node),
         # so on with these 4
         node = ifelse(event_site_code_value %in% c('01','02','03','04'),
                       'Large HRSC', node),
         node = ifelse(event_site_code_value %in% c('05','06','07','08','09','10','11','12'),
                       'SSC Entrance', node),
         node = ifelse(event_site_code_value %in% c('13','14','15'),
                       'SSC Exit', node)) %>%
  # need a column called "config_id"
  mutate(config_id = 1) %>%
  select(site_code = event_site_code_value,
         antenna_id,
         node,
         config_id) %>%
  distinct()

#-----------------------------
# compress()
#-----------------------------

obs_clean = compress(obs_all, ignore_event_vs_release = T, configuration = config, units = "secs") %>%
  mutate(duration = str_replace_all(duration, "secs", ''),
         travel_time = str_replace_all(travel_time, "secs", '')) %>%
  mutate(duration_sec = as.double(duration),
         travel_time_sec = as.double(travel_time)) %>%
  mutate(residency_hr = travel_time_sec/3600) %>%
  select(-duration, -travel_time)



#-----------------------------
# add directionality
#-----------------------------
parent_child <- tibble(parent = c("Upstream Array",
                                  "Middle Array",
                                  "Downstream Array")) %>%
  mutate(child = lead(parent)) %>%
  filter(!is.na(child))

buildNodeOrder(parent_child)

# this adds direction to each observation (forward/backward/etc.)
direct_df = addDirection(obs_clean,
                         parent_child)