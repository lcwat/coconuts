## PLOT THE PATH OF THE FORAGER ON A GIVEN LEVEL
## this function will allow you to visualize the path of the forager and what
## coconuts were collected and how many times during the playthrough

# set font
library(showtext)

# add font if not already in showtext
font_add_google("Alegreya Sans")

showtext_auto()

showtext_opts(dpi = 100)

# add colors
clrs <- NatParksPalettes::natparks.pals("SmokyMtns")

plot_path <- function(
    subj, level, level_df = obj_location_data,
    location_df = location_data, forage_df = forage_data, 
    data_type = "subject"
  ) {
  # can choose to either plot subject or simulated data
  if(data_type == "subject") {
    # define level for filtering data and for defining which df to use
    level_string <- paste("_level", level, sep = "_")
    
    # plot the path of a given subject on a certain level
    toplot <- location_df |> 
      dplyr::filter(subject == subj & level == level_string)
    
    # filter to level of interest and merge corresponding obj locations
    forage_lvl <- forage_df |> 
      dplyr::filter(subject == subj & level == level_string)
    
    lvl <- level_df |> 
      dplyr::filter(level == level_string)
    
    merge <- left_join(forage_lvl, lvl, join_by(x == x, y == y))
    
    # count the number of collections for each coconut
    count_dem <- merge |> 
      group_by(obj_ID) |> 
      summarize(
        count = n()
      )
    
    # remerge to obj location data
    counts <- left_join(lvl, count_dem, join_by(obj_ID == obj_ID))
    
    # get numbers to create useful breaks on legend
    # brks <- unique(counts$count)
    # 
    # brks <- brks[!is.na(brks)]
    brks <- c(5, 10, 15, 20, 25, 30)
    
    # what sizes are on level determine whether or not point scaling is used
    if(length(unique(counts$point_value)) < 2) {
      # make plot
      p <- ggplot() +
        
        # plot the path
        geom_path(
          data = toplot, aes(x = x, y = y), 
          linewidth = .5, alpha = .6, color = "lightblue3"
        ) +
        
        # plot the coconuts sized accordingly and colored by count
        geom_point(
          data = counts, 
          aes(x = x, y = y, size = as.factor(point_value), color = count), 
          size = 3.5
        ) +
        
        scale_size_discrete(
          guide = "none"
        ) +
        
        scale_color_gradient2(
          "Coconut\ncollections",
          low = clrs[3], 
          mid = clrs[2], 
          high = clrs[1], 
          midpoint = 15, 
          breaks = brks
        ) +
        
        # scale_color_viridis_c(
        #   "Coconut\ncollections",
        #   option = "mako",
        #   begin = 0, end = .9, direction = -1,
        #   breaks = brks,
        #   na.value = "grey90"
        # ) +
        
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
          data = counts, 
          aes(x = x, y = y, size = as.factor(point_value), color = count)
        ) +
        
        # define size range for mixed levels, check to see if only one size is 
        # represented 
        scale_size_discrete(
          guide = "none", 
          range = c(2, 6)
        ) +
        
        lims(x = c(-30, 30), y = c(-30, 30)) +
        
        scale_color_gradient2(
          "Coconut\ncollections",
          low = clrs[3], 
          mid = clrs[2], 
          high = clrs[1], 
          midpoint = 15, 
          breaks = brks, 
          limits = c(1, 30), 
          na.value = clrs[7]
        ) +
        
        # scale_color_viridis_c(
        #   "Coconut\ncollections",
        #   option = "mako",
        #   begin = 0, end = .9, direction = -1,
        #   breaks = brks,
        #   midpoint = 15,
        #   na.value = "grey90"
        # ) +
        
        theme_void() +
        
        theme(
          title = element_text(family = "Alegreya Sans", face = "bold", size = 24), 
          text = element_text(family = "Alegreya Sans", size = 18), 
          plot.margin = margin(.1, .1, .1, .1, "in")
        )
    }
  } else if(data_type == "simulation") {
    # define level for filtering data and for defining which df to use
    level_string <- paste("_level", level, sep = "_")
    
    # plot the path of a given subject on a certain level
    toplot <- location_df |> 
      dplyr::filter(subject == subj & level == level_string)
    
    # filter to level of interest and merge corresponding obj locations
    forage_lvl <- forage_df |> 
      dplyr::filter(subject == subj & level == level_string)
    
    lvl <- level_df |> 
      dplyr::filter(level == level_string)
    
    merge <- left_join(forage_lvl, lvl, join_by(x == x, y == y))
    
    # count the number of collections for each coconut
    count_dem <- merge |> 
      group_by(obj_ID) |> 
      summarize(
        count = n()
      )
    
    # remerge to obj location data
    counts <- left_join(lvl, count_dem, join_by(obj_ID == obj_ID))
    
    # get numbers to create useful breaks on legend
    brks <- unique(counts$count)
    
    brks <- brks[!is.na(brks)]
    
    # what sizes are on level determine whether or not point scaling is used
    if(length(unique(counts$point_value)) < 2) {
      # make plot
      p <- ggplot() +
        
        # plot the path
        geom_path(
          data = toplot, aes(x = x, y = y), 
          linewidth = .5, alpha = .6, color = "lightblue3"
        ) +
        
        # plot the coconuts sized accordingly and colored by count
        geom_point(
          data = counts, 
          aes(x = x, y = y, size = as.factor(point_value), color = count), 
          size = 3.5
        ) +
        
        scale_size_discrete(
          guide = "none"
        ) +
        
        scale_color_viridis_c(
          "Coconut\ncollections",
          option = "mako",
          begin = 0, end = .9, direction = -1,
          breaks = brks,
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
          data = counts, 
          aes(x = x, y = y, size = as.factor(point_value), color = count)
        ) +
        
        # define size range for mixed levels, check to see if only one size is 
        # represented 
        scale_size_discrete(
          guide = "none", 
          range = c(2, 6)
        ) +
        
        scale_color_viridis_c(
          "Coconut\ncollections",
          option = "mako",
          begin = 0, end = .9, direction = -1,
          breaks = brks,
          na.value = "grey90"
        ) +
        
        theme_void() +
        
        theme(
          title = element_text(family = "Alegreya Sans", face = "bold"), 
          text = element_text(family = "Alegreya Sans")
        )
    }
  } else {
    stop("Please provide valid data type: either 'subject' or 'simulation'.")
  }
 
  
  return(p)
}
