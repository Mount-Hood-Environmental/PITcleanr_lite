#' @title Add Direction
#'
#' @description Based on the compressed PIT tag observations, and a
#' parent-child table, determines the direction of movement leading to
#' each detection.
#'
#' @author Kevin See, modified by Mark Roes in 2022
#'
#' @param compress_obs The result of `compress()`.
#' @inheritParams buildPaths
#'
#' @import dplyr
#' @export
#' @return a tibble
#' @examples addDirection()

addDirection_lite = function(compress_obs = NULL) {

  stopifnot(!is.null(compress_obs))
  
  getPaths = function(x){
    
    out = x %>%
      arrange(tag_code, slot) %>%
      group_by(tag_code) %>%
      mutate(path = ifelse(slot == 1, node
                                       ,ifelse(node == word(lag(node),-1),lag(node)
                                               ,paste(lag(node), node, sep = " "
                                               )
                                       )
      )
                   ,direction = ifelse(slot == 1, "start"
                                 ,ifelse(lag(rkm) > rkm, "downstream"
                                         ,ifelse(lag(rkm) < rkm, "upstream"
                                                 ,ifelse(lag(rkm) == rkm & lag(node) == node, "no movement"
                                                         ,ifelse(lag(rkm) == rkm, "lateral", "unknown")
                                                         )
                                                 )
                                         )
                                 )
             ) %>%
      ungroup()
      
    return(out)
  }
  
  # determine direction of movement
  obs_direct = compress_obs %>%
    getPaths() 
    

  return(obs_direct)
}
