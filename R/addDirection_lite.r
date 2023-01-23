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
      #group_by(tag_code) %>%
      dplyr::mutate(path = sapply(1:nrow(.),
                                  function(x) paste(node[(x - .$slot[x]+1):x], collapse = ' '))
                   ,direction = ifelse(slot == 1, "start"
                                 ,ifelse(lag(rkm) > rkm, "downstream"
                                         ,ifelse(lag(rkm) < rkm, "upstream"
                                                 ,ifelse(lag(rkm) == rkm & lag(node) == node, "no movement"
                                                         ,ifelse(lag(rkm) == rkm, "lateral", "unknown")
                                                         )
                                                 )
                                         )
                                 )
             )
    
    return(out)
  }
  
  # determine paths and direction of movement
  obs_direct = compress_obs %>%
    getPaths() 
    

  return(obs_direct)
}
