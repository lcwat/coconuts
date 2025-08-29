library(tidyverse)


# load data and join together with obj_IDs
forage_data <- read_csv("data/piloting/3-4-25-forage-piloting-data.csv")

practice_data <- forage_data |> 
  filter(level != "_tutorial" & subject == 1 | subject == 8)

obj_location_data <- read_csv("data/arrangements/object-location-data.csv")

practice_data <- left_join(
  practice_data, obj_location_data, by = join_by(x == x, y == y, level == level)
)

# exploratory calculations

# -------------------------------------------------------------------------

# turning angle
# points
x1 <- 0
x2 <- 15
y1 <- 0
y2 <- -10

heading_angle <- atan2((y2-y1), (x2-x1))

x3 <- 17
y3 <- -10

next_heading <- atan2((y3-y2), (x3-x2))

turning_angle <- next_heading - heading_angle

x4 <- -18
y4 <- -12

other_heading <- atan2((y4-y2), (x4-x2))

turning_angle <- other_heading - heading_angle

# they usually do a cosine transformation of the turning angle to seemingly constrain 
# the range between -1 and 1. reaches asymptote when turning angle is 0
cos(turning_angle)

calculate_turning_angle <- function(previous_heading, curr_x, curr_y, next_x, next_y) {
  ta <- atan2((next_y-curr_y), (next_x-curr_x)) - previous_heading
  
  return(ta)
}

calculate_turning_angle(heading_angle, x2, y2, x4, y4)

# loop through available points to find all possible turning angles
points <- matrix(c(-10,-10,3,4,6,-10,-4,7,8,8,-9,10), nrow = 6, ncol = 2)

ta_vector <- vector("numeric", length = length(points[,1]))

for(i in seq_along(points[,1])) {
  # use heading angle from previous move to find turning angles
  ta <- atan2((points[i,2]-y2), (points[i,1]-x2)) - heading_angle
  
  ta_vector[i] <- ta
}


# -------------------------------------------------------------------------

# function to loop through dataframe and calculate ta's
turning_angle_matrix <- function(previous_heading, curr_obj_id, used_obj_id, obj_df) {
  # create empty vector
  ta_vec <- vector("numeric", nrow(obj_df))
  
  curr_x <- obj_df$x[curr_obj_id]
  curr_y <- obj_df$y[curr_obj_id]
  
  # loop through the object location data for this level and calculate angle
  for(i in seq_along(obj_df$obj_ID)) {
    # calculate angle unless same obj
    if(i == curr_obj_id) {
      ta_vec[i] = NA
    }
    else {
      ta_vec[i] <- atan2(obj_df$y[i]-curr_y, obj_df$x[i]-curr_x) - previous_heading
    }
  }
  
  # create vector of used vs unused obj
  used_vec <- rep(0, nrow(obj_df))
  used_vec[used_obj_id] <- 1
  
  df <- data.frame(
    obj_ID = obj_df$obj_ID, turn_angle = ta_vec, used = used_vec
  )
  
  return(df)
}

# function to create all distance matrices for all levels
create_distance_matrices <- function(obj_df = obj_location_data) {
  # unique levels
  uni_lvls <- unique(obj_df$level)
  
  matrix_list <- vector("list", length(uni_lvls))
  
  for(i in seq_along(uni_lvls)) {
    # filter for level
    lvl_locations <- obj_df |> 
      filter(level == paste("_level_", i, sep=""))
    
    m <- matrix(
      data = 0, ncol = length(lvl_locations$obj_ID), nrow = length(lvl_locations$obj_ID)
    )
    
    for(j in seq_along(lvl_locations$obj_ID)) {
      for(k in seq_along(lvl_locations$obj_ID)) {
        dist_bn <- sqrt(
          (lvl_locations$x[j]-lvl_locations$x[k])^2 +
            (lvl_locations$y[j]-lvl_locations$y[k])^2
        )
        
        m[j,k] <- dist_bn
      }
    }
    
    matrix_list[[i]] <- m
  }
  
  return(matrix_list)
}

# store list of dist matrices for each level
dist_m_list <- create_distance_matrices()

# function to add turning angles to expand foraging data grid
add_turning_angles_and_distance <- function(
    foraging_df, obj_locations = obj_location_data, dist_matrices = dist_m_list
  ) {
  # create tibble to store final expanded df
  expanded_df <- tibble(
    subject = numeric(), level = character(), collection_no = numeric(), 
    obj_ID = numeric(), point_value = numeric(), turning_angle = numeric(),
    distance = numeric(), used = numeric(), time = numeric()
  )
  
  # subjects
  subjects <- unique(foraging_df$subject)
  # levels
  levels <- seq(1,10,1)
  
  # loop through subjects
  for(i in seq_along(subjects)) {
    subj_id <- subjects[i]

    subj_forage_df <- foraging_df |>
      filter(subject == subj_id)

    # loop through levels
    for(j in seq_along(levels)) {
      level_id <- paste("_level_", levels[j], sep="")
      
      # filter forage data
      lvl_forage_df <- subj_forage_df |>
        filter(level == level_id)
      
      # filter level data
      obj_location_lvl <- obj_locations |>
        filter(level == level_id)
      
      # grab level dist matrix from list
      dist_matrix <- dist_matrices[[levels[j]]]
      
      # init the collect df that tracks obj collected and timestamp for respawn
      collect_df <- tibble(
        obj_ID = numeric(),
        time_respawned = numeric()
      )
        
      # loop through forage data
      for(k in 1:(length(lvl_forage_df$obj_ID)-1)) {
        if(k == 1) {
          # find first heading angle from spawn and first collected nut
          heading_angle <- atan2(lvl_forage_df$y[k], lvl_forage_df$x[k])
        }
        else {
          # find heading by comparing previous location to current 
          heading_angle <- atan2(
            lvl_forage_df$y[k]-lvl_forage_df$y[k-1], lvl_forage_df$x[k]-lvl_forage_df$x[k-1]
          )
        }
        
        # create angle matrix
        angle_matrix <- turning_angle_matrix(
          heading_angle, lvl_forage_df$obj_ID[k], lvl_forage_df$obj_ID[k+1], 
          obj_location_lvl
        )
        
        # create table with info
        angles <- tibble(
          subject = rep(subj_id, nrow(obj_location_lvl)), 
          level = rep(level_id, nrow(obj_location_lvl)), collection_no = k, 
          obj_ID = angle_matrix$obj_ID, 
          point_value = obj_location_lvl$point_value, 
          turning_angle = angle_matrix$turn_angle, 
          distance = dist_matrix[,lvl_forage_df$obj_ID[k]], 
          used = angle_matrix$used, 
          time = rep(lvl_forage_df$time[k], nrow(obj_location_lvl))
        )
        
        ## collection/respawning
        # track collection
        collect_df <- collect_df |> 
          add_row(
            obj_ID = lvl_forage_df$obj_ID[k], time_respawned = lvl_forage_df$time[k]+5
          )
        
        # determine if should respawn
        to_respawn <- collect_df |>
          filter(time_respawned <= lvl_forage_df$time[k]) |>
          pull(obj_ID)

        # check if should alter before filtering out collected obj
        if(length(to_respawn) > 0) {
          # reset row
          collect_df <- collect_df |>
            filter(!obj_ID %in% to_respawn)
        }
        
        # find obj to filter out
        to_remove <- collect_df |> 
          filter(obj_ID != 0) |> 
          pull(obj_ID)
        
        # remove obj no longer in play
        angles <- angles |> 
          filter(!obj_ID %in% to_remove)
        
        expanded_df <- rbind(expanded_df, angles)
      }
      print(paste("Finished level", j))
    }
    print(paste("Finished subject", i))
  }
  
  return(expanded_df)
}

newdata <- add_turning_angles_and_distance(practice_data)

newdata |> 
  group_by(subject, level, collection_no) |> 
  summarize(
    n_available = n()
  ) |> 
  ggplot() +
  geom_line(aes(x = collection_no, y=n_available, color=level)) +
  facet_wrap(~subject) +
  theme_bw()

# visualize to see if working correctly after cos transformation
newdata |>
  left_join(obj_location_data) |> 
  mutate(
    cos_turn_angle = cos(turning_angle)
  ) |> 
  filter(subject == 1, level == "_level_1", collection_no == 100) |> 
  ggplot() +
  geom_point(
    aes(x = x, y = y, color = distance, shape = as.factor(used)),
    size = 4
  ) +
  # geom_segment(
  #   data = tibble(x1 = 0, x2 = 15, y1 = 0, y2 = -10), aes(x = x1, y = y1, xend = x2, yend=y2),
  #   arrow = arrow()
  # ) +
  scale_shape_manual(values = c(16, 1)) +
  scale_color_viridis_c() +
  theme_void()
