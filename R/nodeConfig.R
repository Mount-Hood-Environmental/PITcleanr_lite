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
  
  if(length(config_files > 0)){
    
    site_meta = read_csv(config_files, show_col_types = F) %>%
      janitor::clean_names() %>%
      mutate(reader = as.character(reader_number)) %>%
      mutate(key = paste0(site_code, '_', reader))
    
    config = tagdata %>%
      left_join(site_meta, by = 'key') %>%
      mutate(node = ifelse(is.na(node), event_site_code_value, node),
             config_id = 1) %>%
      select(site_code = event_site_code_value,
             antenna_id,
             node,
             config_id) %>%
      distinct()
    
    #print(c("Files found and imported:", config_files))
    return(config)
  }
}