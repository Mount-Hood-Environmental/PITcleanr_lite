#' @title Read in PTAGIS and Biologic data
#'
#' @description The function reads in all required tag history, detection, and metadata files for use in the PITcleanr_lite workflow
#'
#' @author Mark Roes
#'
#' @param ptagis_loc Folder where PTAGIS data is located, default is 'input/PTAGIS_data'
#' @param ptagis_name Naming convention for ptagis observation data, default is that the filename includes the string 'TagHist'
#' @param biologic_loc Folder where Biologic data is located, default is 'input/biologic_data
#' @param biologic_name Naming convention for Biologic observation data, default is that the filename includes the string 'biologic'
#' @param meta_loc Folder where metadata such as biologic array and tags to be filtered out are stored, default is 'input/metadata'

#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom magrittr %<>%
#' @importFrom tidyr replace_na
#' @export
#' @return a tibble

readTagData = function(ptagis_loc = 'input/PTAGIS_data',
                       ptagis_name = 'TagHist',
                       biologic_loc = 'input/biologic_data',
                       biologic_name = 'biologic',
                       meta_loc = 'input/metadata'){
  
  #Read in metadata
  #Site metadata
  if(length(list.files(path = meta_loc, pattern = '*site_metadata.*\\.csv') > 0)){
    
    meta = ldply(list.files(meta_loc, pattern = '*site_metadata.*\\.csv', full.names = T), read_csv) %>%
      mutate(reader = as.character(reader))
  }
  
  #Tags to filter out
  if(length(list.files(path = meta_loc, pattern = '^filter_tags.*\\.csv') > 0)){
    
    filter_tags = ldply(list.files(meta_loc, pattern = '^filter_tags.*\\.csv', full.names = T), read_csv)
  }
  
  #Read in PTAGIS data
  if(length(list.files(path = ptagis_loc, pattern = paste0('*',ptagis_name,'.*\\.csv')) > 0)){
    
    obs_pt = ldply(list.files(ptagis_loc, pattern = paste0('*',ptagis_name,'.*\\.csv'), full.names = T), read_csv) %>%
    janitor::clean_names() %>%
    mutate(event_date_time_value = mdy_hms(event_date_time_value))
    
    if(exists("filter_tags")){
      obs_pt %<>%
        mutate(tag_type = ifelse(tag_code %in% filter_tags$tag_num,
                                 "Test_Tag",
                                 "Fish"))
    }
  }
  
  #Read in Biologic data
  if(length(list.files(path = biologic_loc, pattern = paste0('*',biologic_name,'.*\\.csv')) > 0)){
    
    obs_bl = ldply(list.files(biologic_loc, pattern = paste0('*',biologic_name,'.*\\.csv'), full.names = T), read_csv) %>%
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
                  select(tag_code, mark_species_name, mark_rear_type_name) %>%
                  distinct(),
                by = c("tag_code")) %>%
      select(-site, -site_code)
    
    if(exists("filter_tags")){
      obs_bl %<>%
        mutate(tag_type = ifelse(tag_code %in% filter_tags$tag_num,
                                 "Test_Tag",
                                 "Fish"))
    }
    
    obs_all = obs_bl %>%
      bind_rows(obs_pt) %>%
      filter(tag_type == "Fish")
  }
  
  if(!exists("obs_bl")){
    obs_all = obs_pt
    
  }
  
  return(obs_all)

}