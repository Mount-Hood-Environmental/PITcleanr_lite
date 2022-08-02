# Author: Nick Porter
# Purpose: Create a interface to allow for PITcleanr to be used with Biomark data
# Created: 10/27/21
# Last Modified: 8/2/22

#-----------------------------------------------------------------

rm(list = ls())
# load a couple packages

#to install PITcleanr (if you don't have it)
#remotes::install_github("KevinSee/PITcleanr")
require(tidyverse)
require(lubridate)
require(esquisse)
library(janitor)
library(PITcleanr)
library(readr)

#-----------------------------------------------------------------
# read in the raw data, and rename some columns
raw_data = read_csv("Library/CloudStorage/OneDrive-SharedLibraries-MerckSharp&Dohme,Corp/Biomark Radio Telemetry - Documents/General/PCSRF/Data/Biologic Downloads/Litz_download_07_31_22.csv") %>% #your file pathway
  # raw_data = read_csv("Litz_download_01_19_22.csv") %>% #your file pathway
  rename(`Tag Code` = tag,
         `Event Date Time Value` = detected,
         `Event Site Code Value` = reader,
         `Antenna ID` = antenna) %>%
  
  # you'll need to add this column
  mutate(`Event Type Name` = "Observation") %>%
  janitor::clean_names() %>%
  mutate(antenna_group_configuration_value = 1) %>% #This list is for test tags and defines each tag as Test_Tag or Fish
  mutate(tag_type = ifelse(tag_code %in% c("3E7.0000001D01","3E7.0000001D02","3E7.0000001D03",
                                           "3E7.0000001D04","3E7.0000001D05","3E7.0000001D06",
                                           "3E7.0000001D07","3E7.0000001D08","3E7.0000001D09",
                                           "3E7.0000001D10","3E7.0000001D11","3E7.0000001D12",
                                           "3E7.0000001D13","3E7.0000001D14","3E7.0000001D15",
                                           "3E7.0000001D16","3E7.0000001D17","3E7.0000001D18",
                                           "3E7.0000001D19","3E7.0000001D20","3D9.1C2D6EFF96",
                                           "3DD.003BE8FA6F","3D9.1C2D7084E1"), 
                           "Test_Tag",
                           "Fish"))

glimpse(raw_data)

tag_list = raw_data %>%
 distinct(tag_code)

#setwd("Merck Sharp & Dohme, Corp/Biomark Radio Telemetry - Documents/General/PCSRF/Data")

#write_csv(tag_list, "unique_tag_list.csv")

tags_per_site = raw_data %>%
  group_by(event_site_code_value) %>%
  distinct(tag_code) %>%
  count(tag_code) %>%
  summarise(sum(n))

Tagging_Detail_LITZ <- read_csv("Library/CloudStorage/OneDrive-SharedLibraries-MerckSharp&Dohme,Corp/Biomark Radio Telemetry - Documents/General/PCSRF/Data/Lemhi_Marks/Tagging_Detail_Lemhi_1.1.2020_7.12.2022.csv") %>%
  janitor::clean_names()

Tagging_Detail_Join = Tagging_Detail_LITZ #%>%
  #select(tag_code,mark_site_name,mark_date_mmddyyyy, species_name, run_name, length_mm, weight_g, capture_method_code)

Full_DF = raw_data %>%
  left_join(Tagging_Detail_Join, by = "tag_code")

Summary_distinct_tags = Full_DF%>%
  group_by(species_name) %>%
  #summarise(min(length_mm),max(length_mm),mean(length_mm))
  distinct(tag_code) %>%
  count(tag_code) %>%
  summarise(sum(n))

working_data = Full_DF %>%
  filter(tag_type == "Fish")

# create a configuration file
# This sets up your grouping of antennas into an array e.g. upstream which is made up of 3 antennas spanning the river
config = working_data %>%
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
########################

# compress the data
comp_df = compress(working_data,ignore_event_vs_release = T, configuration = config, units = "secs") #%>%

comp_df$duration <- str_replace_all(comp_df$duration, "secs", '')#removes "secs" from duration
comp_df$travel_time <- str_replace_all(comp_df$travel_time, "secs", '')#removes "secs" from Travel Time

comp_df = comp_df %>%
  mutate(duration_sec = as.double(duration)) %>%
  mutate(travel_time_sec = as.double(travel_time)) %>%
  left_join(Tagging_Detail_Join, by = "tag_code") 

comp_df_filtered = comp_df %>%
  select(tag_code,species_name,node,slot,event_type_name,n_dets,min_det,max_det,duration_sec,travel_time_sec,length_mm,weight_g,capture_method_code,mark_site_name,mark_date_mmddyyyy, release_date_mmddyyyy)

residency_df = comp_df_filtered %>%
  mutate(residency_hr = travel_time_sec/3600) %>%
  select(tag_code,species_name,node,slot,event_type_name,n_dets,min_det,max_det,duration_sec,travel_time_sec,residency_hr,length_mm,weight_g,capture_method_code,mark_site_name,mark_date_mmddyyyy, release_date_mmddyyyy)







#----------------------------------------------
# for directionality, you'll need a parent-child table
# fish move from parent to child
# you can hand-code this however you want
parent_child <- tibble(parent = c("Upstream Array",
                                  "Middle Array",
                                  "Downstream Array")) %>%
  mutate(child = lead(parent)) %>%
  filter(!is.na(child))

buildNodeOrder(parent_child)

# this adds direction to each observation (forward/backward/etc.)
direct_df = addDirection(comp_df,
                         parent_child)
direct_df

tabyl(direct_df,
      direction)

View(parent_child)

tags_per_site = working_data %>%
  group_by(antenna) %>%
  distinct(tag) %>%
  count(tag) %>%
  summarise(sum(n))

Overall_distinct_tags = Clean_Litz %>%
  #filter(!antenna == "HRSC1") %>%
  #filter(!antenna == "HRSC1.1") %>%
  distinct(tag)


setwd("Merck Sharp & Dohme, Corp/Biomark Radio Telemetry - Documents/General/PCSRF/Data/Biologic Downloads")

write.csv(Overall_distinct_tags, "Litz_tags.csv")

Summary_distinct_tags = Overall_distinct_tags%>%
  count(tag) %>%
  summarise(sum(n))

#esquisser(tags_per_site)

investigation = Clean_Litz %>%
  filter(!antenna == "HRSC1") %>%
  filter(!antenna == "HRSC1.1") %>%
  distinct() %>%
  arrange(tag,date,antenna)