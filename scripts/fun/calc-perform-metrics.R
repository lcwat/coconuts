## CALCULATE PERFORMANCE METRICS
## for a given subject, calculate performance metrics of distance and time

##-----------------------------------------------------------------
##   calculate perform metrics
##-----------------------------------------------------------------

# pass in the df with time and location in each level
# and can indicate specific level or get total 
calc_perform_metrics <- function(location_df) {
  # add distances
  loc_w_distance <- location_df |>
    mutate(
      distance_from_last = if_else(
        # don't calc distance between diff subj sessions
        location_df$subject == dplyr::lag(location_df$subject, default = 0),
        # don't calc distance between diff levels
        if_else(
          location_df$level == dplyr::lag(location_df$level, default = ""), 
          sqrt(
            (location_df$x - dplyr::lag(location_df$x, default = 0))^2 + 
              (location_df$y - dplyr::lag(location_df$y, default = 0))^2
          ),
          0
        ),
        0
      )
    )
  
  # now summarize by level find time spent in level and distance traveled
  sum_by_level <- loc_w_distance |> 
    group_by(subject, level) |> 
    summarize(
      total_dist_in_lvl = sum(distance_from_last, na.rm = T), 
      total_time_in_lvl = max(time) - min(time)
    )
  
  return(sum_by_level)
}
