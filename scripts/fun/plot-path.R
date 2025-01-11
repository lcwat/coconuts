## PLOT THE PATH OF THE FORAGER ON A GIVEN LEVEL
## this function will allow you to visualize the path of the forager and what
## coconuts were collected and how many times during the playthrough

# set font
library(showtext)

# add font if not already in showtext
# font_add_google("Alegreya Sans")

showtext_auto()

showtext_opts(dpi = 300)

plot_path <- function(subject, level, location_df, forage_df, level_df) {
  # define level
  level_string <- paste("_level", level, sep = "_")
  
  # plot the path of a given subject on a certain level
  toplot <- location_data |> 
    filter(subject == 1 & level == level_string)
  
  # count the number of collections for each coconut
  counts <- forage_data |> 
    filter(subject == 1 & level == level_string) |> 
    group_by(x, y) |> 
    summarize(
      count = n()
    )
  
  lvl <- level_df
  
  merge <- left_join(lvl, counts, join_by(x == x, y == y))
  
  # what sizes are on level determine whether or not point scaling is used
  if(length(unique(merge$point_value)) < 2) {
    # make plot
    p <- ggplot() +
      
      # plot the path
      geom_path(
        data = toplot, aes(x = x, y = y), 
        linewidth = .5, alpha = .6, color = "lightblue3"
      ) +
      
      # plot the coconuts sized accordingly and colored by count
      geom_point(
        data = merge, 
        aes(x = x, y = y, size = as.factor(point_value), color = count)
      ) +
      
      scale_size_discrete(
        guide = "none"
      ) +
    
      scale_color_viridis_c(
        "Coconut\ncollections",
        option = "mako",
        begin = 0, end = .9, direction = -1,
        na.value = "grey90"
      ) +
      
      theme_void() +
      
      theme(
        title = element_text(family = "Alegreya Sans", face = "bold"), 
        text = element_text(family = "Alegreya Sans")
      )
    
  } else {
    # make plot
    p <- ggplot() +
      
      # plot the path
      geom_path(
        data = toplot, aes(x = x, y = y), 
        linewidth = .5, alpha = .6, color = "lightblue3"
      ) +
      
      # plot the coconuts sized accordingly and colored by count
      geom_point(
        data = merge, 
        aes(x = x, y = y, size = as.factor(point_value), color = count)
      ) +
      
      # define size range for mixed levels, check to see if only one size is 
      # represented 
      scale_size_discrete(
        guide = "none", 
        range = c(1, 4)
      ) +
    
      scale_color_viridis_c(
        "Coconut\ncollections",
        option = "mako",
        begin = 0, end = .9, direction = -1,
        na.value = "grey90"
      ) +
      
      theme_void() +
      
      theme(
        title = element_text(family = "Alegreya Sans", face = "bold"), 
        text = element_text(family = "Alegreya Sans")
      )
  }
  
  return(p)
}
