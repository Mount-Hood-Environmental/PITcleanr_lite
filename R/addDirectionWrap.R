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



addDirectionWrap = function(tagdata = obs_clean,
                         group_nodes = FALSE, #currently unused
                         build_diagram = FALSE, #currently unused
                         generate_map = FALSE,
                         downstream_site = 'HYC',
                         dwnstrm_sites = TRUE){
  
  source('R/addDirection_lite.R')
  source("R/buildNodeGraph.R")
  source("R/plotNodes.R")
  

  if(generate_map == T){

    source("R/queryFlowlines.R")
    source("R/findDwnstrmSite.R")
    
    
    #bind together and filter to detection sites
    node_locs = tagdata %>%
      select(node, latitude, longitude) %>%
      distinct() %>%
      st_as_sf(coords = c("longitude", "latitude"),
               crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    #Building flowlines
    
    flow = node_locs %>%
      rename(site_code = node) %>%
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
    
    ggsave(paste0("output/figures/flowlines_plot_",Sys.Date(),'.tiff'),
           flowlines_plot,
           width = 8,
           height = 8,
           units = 'in',
           dpi = 300)
  }
  
  # if(group_nodes == T){
  #   tagdata %<>%
  #     left_join(rbind(rkm_dat %>%
  #                       select(parent, parent_group) %>%
  #                       rename(node = parent,
  #                              node_group = parent_group),
  #                       rkm_dat %>%
  #                       select(child, child_group) %>%
  #                       rename(node = child,
  #                              node_group = child_group)) %>%
  #                       distinct()
  #                 , by = 'node') %>%
  #     mutate(node_id = node,
  #            node = ifelse(is.na(node_group), node, node_group)) %>%
  #     #filter(!is.na(node_group)) %>%
  #     #mutate(node = node_group) %>%
  #     select(-node_group)
  # 
  #     
  #   
  #   rkm_dat %<>%
  #     select(parent_group, child_group) %>%
  #     rename(parent = parent_group,
  #            child = child_group) %>%
  #     distinct()
  # }
  
  # if(build_diagram == T){
  # #Output path diagram
  #   
  #   flow_dia = tagdata %>%
  #     dplyr::select(node, rkm) %>%
  #     distinct() %>%
  #     group_by(rkm) %>%
  #     dplyr::summarise(x = row_number(node),
  #                      node = node) %>%
  #     ungroup() %>%
  #     ggplot()+
  #     geom_text(aes(x = x, y = rkm, label = node))+
  #     theme_bw()+
  #     theme(panel.grid = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.text.x = element_blank(),
  #           axis.ticks.x = element_blank())+
  #     labs(y = "RKM")
  # 
  # ggsave(paste0('output/figures/Node_Diagram_', Sys.Date(),'.tiff'),
  #        flow_dia,
  #        width = 4,
  #        height = 4,
  #        units = 'in',
  #        dpi = 300)
  # 
  # }
  



    out = addDirection_lite(compress_obs = tagdata) %>%
      mutate(path_len = str_count(path, " ")) %>%
      group_by(tag_code, node, slot) %>%
      slice_max(max_det) %>%
      slice_max(path_len) %>%
      ungroup() %>%
      select(-path_len) %>%
      mutate(min_det_date = date(min_det),
             min_det_time = format(min_det, "%H:%M:%S"),
             max_det_date = date(max_det),
             max_det_time = format(max_det, "%H:%M:%S")
      )

  
  
  return(out)
}