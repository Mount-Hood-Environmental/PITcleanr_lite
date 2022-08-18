#' @title create note configuration object
#'
#' @description The function creates a node configuration file for compress
#'
#' @author Mark Roes
#'
#' @param config_loc Folder where node config data is located, default is 'input/metadata'
#' @param config_name name of config data file, default is 'node_config'
#' @param tagdata object name of tag data, default is obs_all

#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom magrittr %<>%
#' @importFrom tidyr replace_na
#' @export
#' @return a tibble

nodeConfig = function(config_loc = 'input/metadata',
                        config_name = 'node_config',
                        tagdata = obs_all){
  
  read_csv_quiet = function(x){
    out = read_csv(x, show_col_types = F)
    
  }
  
  config_files = list.files(path = config_loc, pattern = paste0('*',config_name,'.*\\.csv'), full.names = T)
  
  if(length(config_files > 0)){
    
    node_dat = ldply(config_files, read_csv_quiet)
    
    config = tagdata %>%
      left_join(node_dat, by = c('event_site_code_value' = 'reader')) %>%
      mutate(node = ifelse(is.na(node), event_site_code_value, node),
             config_id = 1) %>%
      select(site_code = event_site_code_value,
             antenna_id,
             node,
             config_id) %>%
      distinct()
    
    print(c("Files found and imported:", config_files))
    return(config)
  }
}