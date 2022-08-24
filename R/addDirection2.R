#' @title Wrapper for addDirection and buildNodeOrder from PITcleanr 
#'
#' @description 
#'
#' @author Mark Roes
#'
#' @param tagdata object name of cleaned and compressed tag observation data
#' @param nodeDir_loc file location of node_direction file, default is 'input/metadata'
#' @param filename name of node direction file, default is 'node_direction.csv'

#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom magrittr %<>%
#' @importFrom tidyr replace_na
#' @export
#' @return a tibble



addDirection2 = function(tagdata = obs_clean,
                         nodeDir_loc = 'input/metadata/',
                         filename = 'node_direction.csv',
                         group_nodes = T,
                         build_diagram = T,
                         generate_map = T,
                         downstream_site = "HYC",
                         dwnstrm_sites = T){
  
  source('R/addDirection.R')
  source('R/buildNodeOrder.R')
  source('R/buildPaths.R')
  source('R/listParents.R')
  source('R/addParentChildNodes.R')
  source("R/buildNodeGraph.R")
  source("R/plotNodes.R")
  
  nodeDir_file = list.files(path = nodeDir_loc, pattern = 'node_direction.csv', full.names = T)
  
  dir_dat = read_csv(nodeDir_file, show_col_types = F)
  
  if(group_nodes == T){
    dir_dat %<>%
      select(parent_group, child_group) %>%
      rename(parent = parent_group,
             child = child_group) %>%
      distinct()

  }
  
  
  if(build_diagram == T){
  #Output path diagram
  flow_dia = plotNodes(dir_dat)
  
  flow_dia 
  
  tiff(filename = paste0('output/figures/Node_Diagram_', Sys.Date(),'.tiff'), width = 4, height = 4, units = 'in', res = 300, compression = 'lzw')
  flow_dia
  dev.off()
  }
  
  if(generate_map == T){
  #Obtaining geo points for all node locations
  source("R/queryPtagisMeta.R")
  source("R/queryInterrogationMeta.R")
  source("R/queryInterrogationConfig.R")
  source("R/queryMRRMeta.R")
  source("R/queryFlowlines.R")
  # source("R/buildParentChild.R")
  source("R/findDwnstrmSite.R")
  
  nodes_pt = queryPtagisMeta()

  #Read in this data with the site metadata
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
    queryFlowlines(root_site_code = downstream_site,
                   min_strm_order = 2,
                   dwnstrm_sites = dwnstrm_sites,
                   dwn_min_stream_order_diff = 2)
  
  flowlines = rbind(flow$flowlines,
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
    geom_sf(data = flow$basin,
            fill = NA,
            lwd = 2) +
    geom_sf(data = node_locs,
            size = 4,
            color = "black") +
    geom_sf_label(data = node_locs,
                  aes(label = node),
                  size = 2) +
    theme_bw() +
    theme(axis.title = element_blank(),
          plot.margin=unit(c(1,1,1,1),'cm'))+
    labs(color = "Stream\nOrder")
  
  flowlines_plot
  
  tiff(filename = paste0("output/figures/flowlines_plot_",Sys.Date(),'.tiff'), res = 300, compression = 'lzw', width = 8, height = 8, units = 'in')
  flowlines_plot
  dev.off()
  }
  

  buildNodeOrder(dir_dat, direction = 'd')
  
  
  
  out = addDirection(compress_obs = tagdata, parent_child = dir_dat)
  
  print(c("Files found and imported:", nodeDir_file))
  
  return(out)
}