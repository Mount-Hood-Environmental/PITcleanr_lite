#' @title obsWide
#'
#' @description 
#'
#' @author Mark Roes
#'
#' @param obs_compressed compressed tag observations

#' @import dplyr
#' @import tidyr 
#' @export
#' @return a tibble


obsWide = function(obs_compressed){
  
  out = obs_compressed %>%
    pivot_wider(names_from = node, values_from = n_dets) %>%
    select(-c(slot:max_det_time)) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    group_by(tag_code)%>%
    summarise(across(everything(), sum)) %>%
    ungroup()
  
  return(out)
}
