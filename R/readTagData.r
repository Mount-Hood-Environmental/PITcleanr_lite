#' @title Read in PTAGIS, Biologic, log, and submersible data
#'
#' @description The function reads in all required tag history, detection, and metadata files for use in the PITcleanr_lite workflow
#'
#' @author Mark Roes
#'
#' @param data_path Folder where tag obs data is located, default is 'input'
#' @param ptagis_name Naming convention for ptagis observation data, default is that the filename includes the string 'TagHist'
#' @param biologic_name Naming convention for Biologic observation data, default is that the filename includes the string 'biologic'
#' @param logfile_name Naming convention for untransformed logfile data in *.txt format
#' @param subfile_name Naming convention for data obtained by submersibles
#' @param config_path Folder where metadata such as biologic array and tags to be filtered out are stored, default is 'configuration'

#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom magrittr %<>%
#' @importFrom tidyr replace_na
#' @export
#' @return a tibble

readTagData = function(data_path = 'input',
                       ptagis_name = 'PTAGIS',
                       biologic_name = 'BIOLOGIC',
                       sub_name = 'SUB',
                       config_path = 'config',
                       filter.test.tags = TRUE,
                       filter.to.ptagis = TRUE){
  
  read_csv_quiet = function(x){
    out = read_csv(x, show_col_types = F)
    
  }
  
  #list files####
  meta_files = list.files(path = config_path, pattern = '*site_metadata.*\\.csv', full.names = T)
  filtertag_files = list.files(config_path, pattern = '^filter_tags.*\\.csv', full.names = T)
  ptagis_files = list.files(data_path, pattern = paste0('*',ptagis_name,'.*\\.csv'), full.names = T)
  bl_files = list.files(data_path, pattern = paste0('*',biologic_name,'.*\\.csv'), full.names = T)
  log_files = list.files(data_path, pattern = paste0('*\\.log'), full.names = T)
  sub_files = list.files(data_path, pattern = paste0('*',sub_name,'.*\\.xlsx'), full.names = T)
  
  #site metadata####
  if(length(meta_files > 0)){
    
    meta = ldply(meta_files, read_csv_quiet) %>%
      janitor::clean_names() %>%
      mutate(reader = as.character(reader_number)) %>%
      mutate(key = paste(site_code,reader_number, sep ="-"))
  }
  

  
  #PTAGIS data####
  if(length(ptagis_files > 0)){
    
    obs_pt = ldply(ptagis_files, read_csv_quiet) %>%
    janitor::clean_names() %>%
    mutate(event_date_time_value = mdy_hms(event_date_time_value))
    
    obs_all = obs_pt
  }
  
  #Biologic data####
  if(length(bl_files > 0)){
    
    read_bl = function(x){
    out = read_csv_quiet(x) %>%
      mutate(key = paste(site, as.numeric(reader),sep="-")) %>%
      left_join(meta %>%
      select(reader_name, key), by = c("key")) %>%
      rename(tag_code = tag,
             event_date_time_value = detected,
             event_site_code_value = reader_name,
             antenna_id = antenna,
             reader_id = reader
             ) %>%
      mutate(cth_count = 1,
             event_type_name = "Observation",
             antenna_group_configuration_value = 1,
             event_site_type_description = "Instream Remote Detection System") %>%
      janitor::clean_names()
    
    
    
    return(out)
    }
    
    obs_bl = ldply(bl_files, read_bl) %>%
      left_join(obs_pt %>%
                  select(tag_code, mark_species_name, mark_rear_type_name) %>%
                  distinct(),
                by = c("tag_code")) %>%
      select(-site)
    
    if(filter.to.ptagis == TRUE){
      obs_bl %<>% filter(tag_code %in% obs_pt$tag_code)
    }
    
    obs_all %<>%
      bind_rows(obs_bl)
  }
  
  #log files####
  if(length(log_files) > 0){

    read_log_file = function(x){
      #node_name = str_replace(logfile_names[1], pattern=".*input/([^-])_.*", replacement = "\\1")
      
      node_name = sub("_.*","",sub(".*/","" , x))
      out = read.table(x, fill = T) %>%
        filter(V1 == "TAG:") %>%
        select(V3:V6) %>%
        mutate(cth_count = 1,
               event_type_name = "Observation",
               antenna_group_configuration_value = 1,
               event_site_type_description = "Instream Remote Detection System",
               event_date_time_value = mdy_hms(paste0(V4,V5)),
               event_site_code_value = node_name
               ) %>%
        rename(tag_code = V6) %>%
        select(tag_code:event_site_code_value)
        
      return(out)
    }
    obs_log = ldply(log_files, read_log_file) %>%
      left_join(obs_pt %>%
                  select(tag_code, mark_species_name, mark_rear_type_name) %>%
                  distinct(),
                by = c("tag_code"))
    
    if(filter.to.ptagis == TRUE){
      obs_log %<>% filter(tag_code %in% obs_pt$tag_code)
    }
    
    obs_all %<>%
      bind_rows(obs_log)
  }
  
  #submersible files####
  if(length(sub_files) > 0){
    
    read_sub_file = function(x){
      node_name = sub("_.*","",sub(".*/","" , x))
      
      out = read_excel(x) %>%
        janitor::clean_names() %>%
        mutate(event_date_time_value = mdy_hms(paste0(scan_date,scan_time)),
               event_site_code_value = node_name,
               cth_count = 1,
               event_type_name = "Observation",
               antenna_group_configuration_value = 1,
               event_site_type_description = "Instream Remote Detection System"
               ) %>%
        rename(tag_code = hex_tag_id) %>%
        select(reader_id:tag_code, event_date_time_value:event_site_type_description)
      
      return(out)
    }
    
    obs_sub = ldply(sub_files, read_sub_file)%>%
      left_join(obs_pt %>%
                  select(tag_code, mark_species_name, mark_rear_type_name) %>%
                  distinct(),
                by = c("tag_code"))
    
    if(filter.to.ptagis == TRUE){
      obs_sub %<>% filter(tag_code %in% obs_pt$tag_code)
    }
      
    
    obs_all %<>%
      bind_rows(obs_sub)
  }
  
  #Tags to filter out####
  if(length(filtertag_files > 0)){
    
    filter_tags = ldply(filtertag_files, read_csv_quiet)
    

    obs_all %<>%
      mutate(test_tag = ifelse(tag_code %in% filter_tags$tag_num,
                               "yes",
                               "no"))
    
    if(filter.test.tags == TRUE){
      obs_all %<>%
      filter(test_tag == "no") %>%
      select(-test_tag)
    }
  }
  
  #outputs####
  print(c("-CONFIGURATION FILES-",
          meta_files,
          filtertag_files,
          "",
          "-TAG FILES-",
          ptagis_files,
          bl_files,
          log_files,
          sub_files,
          "",
          "-TAG INFORMATION-",
          ifelse(exists("obs_pt"),paste0("Unique PTAGIS codes: ",length(unique(obs_pt$tag_code))),""),
          ifelse(exists("obs_bl"),paste0("Matching tags from Biologic: ", length(unique(obs_bl$tag_code))),""),
          ifelse(exists("obs_log"), paste0("Matching tags from log files: ", length(unique(obs_log$tag_code))),""),
          ifelse(exists("obs_sub"), paste0("Matching tags from submersibles: ", length(unique(obs_sub$tag_code))),"")))
  
  return(obs_all)

}