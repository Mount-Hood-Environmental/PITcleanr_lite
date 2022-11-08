#' @title Wrapper for PITcleanr compress
#'
#' @description summarizes data further than compress
#'
#' @author Mark Roes
#'
#' @param tagdata DF/tibble where all observation data is stored
#' @param config configuration for compress

#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom magrittr %<>%
#' @importFrom tidyr replace_na
#' @export
#' @return a tibble

compressWrap = function(tagdata = obs_all){
  
  source('R/compress.R')
  source('R/readCTH.r')
  source('R/qcTagHistory.r')
  source('R/queryPtagisMeta.r')
  source('R/queryInterrogationConfig.r')
  source('R/queryInterrogationMeta.r')
  source('R/queryMRRMeta.r')
  
  site_meta = nodeConfig()
  
  ptagis_meta = queryPtagisMeta() %>%
    filter(site_code %in% tagdata$event_site_code_value) %>%
    select(site_code, latitude, longitude, rkm_total) %>%
    distinct() %>%
    rename()
  
  config = bind_
  
    
  
  out = compress(ptagis_file = tagdata, configuration = config, ignore_event_vs_release = T, units = "secs") %>%
    mutate(duration = str_replace_all(duration, "secs", ''),
           travel_time = str_replace_all(travel_time, "secs", '')) %>%
    mutate(duration_sec = as.double(duration),
           travel_time_sec = as.double(travel_time)) %>%
    mutate(residency_hr = travel_time_sec/3600) %>%
    select(-duration, -travel_time) %>%
    mutate(min_det_date = date(min_det),
           min_det_time = format(min_det, "%H:%M:%S"),
           max_det_date = date(max_det),
           max_det_time = format(max_det, "%H:%M:%S")
    )
  
  return(out)
  
}