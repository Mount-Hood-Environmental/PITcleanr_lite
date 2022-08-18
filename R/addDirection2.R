#' @title Wrapper for addDirection and buildNodeOrder from PITcleanr 
#'
#' @description 
#'
#' @author Mark Roes
#'
#' @param tagdata object name of cleaned and compressed tag observation data
#' @param node_loc file location of node_direction file, default is 'input/metadata'
#' @param filename name of node direction file, default is 'node_direction.csv'

#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom magrittr %<>%
#' @importFrom tidyr replace_na
#' @export
#' @return a tibble



addDirection2 = function(tagdata = obs_clean,
                         node_loc = 'input/metadata',
                         filename = 'node_direction.csv'){
  
  source('R/addDirection.R')
  source('R/buildNodeOrder.R')
  source('R/buildPaths.R')
  source('R/listParents.R')
  
  node_dat = read_csv(paste0(node_loc, '/', filename))
  
  parent_child = node_dat %>%
    mutate(child = lead(parent)) %>%
    filter(!is.na(child))
  
  buildNodeOrder(parent_child)
  
  out = addDirection(tagdata, parent_child)
  
  return(out)
}