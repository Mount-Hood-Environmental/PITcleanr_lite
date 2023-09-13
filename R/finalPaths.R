#' @title Subsets fish direction data to last observation, providing final paths and restoration use
#'
#' @description 
#'
#' @author Mark Roes
#'
#' @param obs_direct object name of compressed and directional tag data from pitclear_lite


finalPaths = function(obs_direct = obs_direct, calculate_restoration_use = TRUE){
  
  if(calculate_restoration_use == TRUE){
  out = obs_direct %>%
    #obs_in_resto
    left_join(obs_direct %>%
                filter(restoration_site == "Yes") %>%
                mutate(obs_in_resto = "Yes") %>%
                dplyr::select(tag_code, obs_in_resto) %>%
                distinct()) %>%
    mutate(obs_in_resto = ifelse(is.na(obs_in_resto),"No",obs_in_resto)) %>%
    #restoration_use_hrs
    left_join(obs_direct %>%
                arrange(tag_code, slot) %>%
                group_by(tag_code) %>%
                mutate(interior_use_hr = ifelse(restoration_interior == "Yes", residency_hr,0),
                       travel_use_hr = ifelse(slot > 1 &
                                                restoration_interior == "Yes" &
                                                lag(restoration_entrance_or_exit) == "Yes" |
                                                slot > 1 &
                                                restoration_interior == "Yes" &
                                                lag(restoration_interior) == "Yes" |
                                                slot > 1 &
                                                restoration_entrance_or_exit == "Yes" &
                                                lag(restoration_interior) == "Yes",
                                              travel_time_hr, 0)) %>%
                dplyr::summarise(total_restoration_use_hr = sum(interior_use_hr, na.rm = T) + sum(travel_use_hr, na.rm = T) , 0) %>%
                ungroup()) %>%
    mutate(path.len = str_count(path, " ")) %>%
    group_by(tag_code) %>%
    slice_max(max_det) %>%
    slice_max(path.len) %>%
    ungroup() %>%
    #rename last detections and pare down columns
    select(tag_code,
           node,
           max_det_date,
           max_det_time,
           path,
           direction,
           obs_in_resto,
           total_restoration_use_hr) %>%
    rename(final_det_date = max_det_date,
           final_det_time = max_det_time,
           last_node = node,
           final_path = path,
           final_movement_direction = direction,
           entered_restoration_site = obs_in_resto) %>%
    #bring in first detection datetime
    left_join(obs_direct %>%
                group_by(tag_code) %>%
                slice_min(min_det) %>%
                select(tag_code, node, min_det_date, min_det_time) %>%
                rename(first_node = node,
                       first_det_date = min_det_date,
                       first_det_time = min_det_time) %>%
                distinct()) %>%
    select(tag_code,
           first_node,
           first_det_date,
           first_det_time,
           last_node,
           final_det_date,
           final_det_time,
           final_path,
           final_movement_direction,
           entered_restoration_site,
           total_restoration_use_hr)
  }else{
    out = obs_direct %>%
      mutate(path.len = str_count(path, " ")) %>%
      group_by(tag_code) %>%
      slice_max(max_det) %>%
      slice_max(path.len) %>%
      ungroup() %>%
      #rename last detections and pare down columns
      select(tag_code,
             node,
             max_det_date,
             max_det_time,
             path,
             direction) %>%
      rename(final_det_date = max_det_date,
             final_det_time = max_det_time,
             last_node = node,
             final_path = path,
             final_movement_direction = direction) %>%
      #bring in first detection datetime
      left_join(obs_direct %>%
                  group_by(tag_code) %>%
                  slice_min(min_det) %>%
                  select(tag_code, node, min_det_date, min_det_time) %>%
                  rename(first_node = node,
                         first_det_date = min_det_date,
                         first_det_time = min_det_time)) %>%
      select(tag_code,
             first_node,
             first_det_date,
             first_det_time,
             last_node,
             final_det_date,
             final_det_time,
             final_path,
             final_movement_direction)
    
    
  }
  
  return(out)
}