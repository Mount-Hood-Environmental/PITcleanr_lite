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

compressWrap = function(tagdata = obs_all,
                        attributes_list = c("event_length_mm","event_weight_g")){
  
  source('R/compress.R')
  source('R/readCTH.r')
  source('R/qcTagHistory.r')
  source('R/queryPtagisMeta.r')
  source('R/queryInterrogationConfig.r')
  source('R/queryInterrogationMeta.r')
  source('R/queryMRRMeta.r')
  
  config = nodeConfig()
  

  
  out = compress(ptagis_file = tagdata, configuration = config, ignore_event_vs_release = T, units = "secs") %>%
    mutate(duration = str_replace_all(duration, "secs", ''),
           travel_time = str_replace_all(travel_time, "secs", '')) %>%
    mutate(duration_hr = as.double(duration)/3600,
           travel_time_hr = as.double(travel_time)/3600) %>%
    rename(residency_hr = duration_hr) %>%
    select(-duration, -travel_time) %>%
    mutate(min_det_date = date(min_det),
           min_det_time = format(min_det, "%H:%M:%S"),
           max_det_date = date(max_det),
           max_det_time = format(max_det, "%H:%M:%S")) %>%
    left_join(tagdata %>%
                select(tag_code, event_site_code_value, event_date_time_value, all_of(attributes_list)) %>%
                mutate(min_det_date = date(event_date_time_value)) %>%
                select(-event_date_time_value) %>%
                distinct(),
              relationship = "many-to-many",
              by = c("tag_code","node" = "event_site_code_value", "min_det_date"),
              ) %>%
    left_join(config %>%
                select(node, rkm, latitude, longitude, restoration_site, restoration_entrance_or_exit, restoration_interior) %>%
                distinct())
  
  return(out)
  
}