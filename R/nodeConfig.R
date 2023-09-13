#' @title create node configuration object
#'
#' @description The function creates a node configuration file for compress
#'
#' @author Mark Roes
#'
#' @param config_path Folder where node config data is located, default is 'input/metadata'
#' @param config_name name of config data file, default is 'node_config'
#' @param tagdata object name of tag data, default is obs_all

#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom magrittr %<>%
#' @importFrom tidyr replace_na
#' @export
#' @return a tibble

nodeConfig = function(config_path = 'config',
                        config_name = 'site_metadata',
                        tagdata = obs_all){
  
  config_files = list.files(path = config_path, pattern = paste0('*',config_name,'.*\\.csv'), full.names = T)
  
    ptagis_meta = queryPtagisMeta() %>%
      filter(site_code %in% tagdata$event_site_code_value) %>%
      select(site_code, latitude, longitude, rkm_total) %>%
      distinct() %>%
      rename(rkm = rkm_total) %>%
      mutate(event_site_code_value = site_code)
    
    if(length(config_files > 0)){
      
      site_meta = read_csv(config_files, show_col_types = F) %>%
        janitor::clean_names() %>%
        mutate(reader = as.character(reader_number)) %>%
        rename(event_site_code_value = reader_name)
      
      meta = bind_rows(site_meta, ptagis_meta)
    }else{
  meta = ptagis_meta
    
}
    config = tagdata %>%
    left_join(meta, by = 'event_site_code_value') %>%
    mutate(antenna_id = ifelse(is.na(antenna_id), antenna_number, antenna_id),
       antenna_group_configuration_value = ifelse(is.na(antenna_group_configuration_value), 1, antenna_group_configuration_value)
    ) %>%
      select(site_code = event_site_code_value,
             node = event_site_code_value,
             latitude,
             longitude,
             rkm,
             antenna_id,
             antenna_group_configuration_value,
             restoration_site,
             restoration_entrance_or_exit,
             restoration_interior
             ) %>%
      distinct() %>%
      mutate(restoration_site = ifelse(is.na(restoration_site), 'No',restoration_site),
             restoration_entrance_or_exit = ifelse(is.na(restoration_entrance_or_exit), 'No', restoration_entrance_or_exit),
             restoration_interior = ifelse(is.na(restoration_interior), 'No', restoration_interior))
    
    
    #print(c("Files found and imported:", config_files))
    return(config)
  }
