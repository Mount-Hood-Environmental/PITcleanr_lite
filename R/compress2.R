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

compress2 = function(tagdata = NULL,
                     config = NULL){
  
  source('R/compress.R')
  
  out = compress(ptagis_file = tagdata, configuration = config, ignore_event_vs_release = T, units = "secs") %>%
    mutate(duration = str_replace_all(duration, "secs", ''),
           travel_time = str_replace_all(travel_time, "secs", '')) %>%
    mutate(duration_sec = as.double(duration),
           travel_time_sec = as.double(travel_time)) %>%
    mutate(residency_hr = travel_time_sec/3600) %>%
    select(-duration, -travel_time)
  
  return(out)
  
}