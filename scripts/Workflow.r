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
packages <- c("plyr","tidyverse","magrittr", "lubridate","esquisse","readr","janitor","nhdplusTools", "sf") 
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
source("R/queryFlowlines.R")
source("R/buildParentChild.R")
source("R/findDwnstrmSite.R")

nodedat = queryPtagisMeta()
write_csv(nodedat, "input/metadata/ptagis_node_locs.csv")

#Read in this data with the site metadata
nodes_pt = read_csv("input/metadata/ptagis_node_locs.csv")
nodes_bl = read_csv("input/metadata/site_metadata.csv")
node_names = read_csv("input/metadata/node_config.csv")

nodes_bl %<>%
  mutate(reader = as.character(reader)) %>%
  left_join(node_names, by = c("reader"))

#bind together and filter to detection sites
node_locs = nodes_pt %>%
  mutate(node = site_code) %>%
  rename(lat = latitude,
         lon = longitude) %>%
  select(node, lat, lon) %>%
  bind_rows(nodes_bl %>%
              select(node, site_code, lat, lon)) %>%
  filter(node %in% obs_clean$node) %>%
  distinct() %>%
  group_by(node) %>%
  slice_head(n=1) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Building flowlines

flow = node_locs %>%
  mutate(site_code = node) %>%
  queryFlowlines(root_site_code = "SSC Exit",
                 min_strm_order = 2,
                 dwnstrm_sites = T,
                 dwn_min_stream_order_diff = 2)
flowlines = bind_rows(flow$flowlines,
            flow$dwn_flowlines)


flowlines_plot = flowlines %>%
ggplot() +
  geom_sf(aes(color = as.factor(StreamOrde),
              size = StreamOrde)) +
  scale_color_viridis_d(direction = -1,
                        option = "D",
                        end = 0.8) +
  scale_size_continuous(range = c(0.2, 1.2),
                        guide = 'none') +
  geom_sf(data = flowlines$basin,
          fill = NA,
          lwd = 2) +
  geom_sf(data = node_locs,
          size = 4,
          color = "black") +
  geom_sf_label(data = node_locs,
                aes(label = node),
                size = 2) +
  theme_bw() +
  theme(axis.title = element_blank()) +
  labs(color = "Stream\nOrder")
  
flowlines_plot

#Creating parent-child relationships
parent_child = buildParentChild(node_locs, flowlines)


#Getting directionality