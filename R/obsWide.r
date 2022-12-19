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


obsWide = function(obs_compressed = obs_clean){
  
  order = nodeConfig() %>%
    select(node, rkm) %>%
    distinct() %>%
    arrange(desc(rkm))
  
  out = obs_compressed %>%
    group_by(tag_code, node) %>%
    summarise(dets = sum(n_dets)) %>%
    ungroup() %>%
    pivot_wider(names_from = node, values_from = dets) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    select(tag_code, order$node)


  return(out)
}
