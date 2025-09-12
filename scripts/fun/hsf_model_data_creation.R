library(tidyverse)

# read in data
cleaned_forage_data <- read_csv("data/piloting/march24_pilot_forage_data_w_ids.csv")

# source funs
source("scripts/fun/distance_matrix.R")

# for each consecutive step, need to create dataset laying out all available 
# options. using this code, will try to create that dataset out of simple tracked
# resource acquisition
practice_set <- head(cleaned_forage_data, n=10)

# work with one subject and level at a time
# loop through subjects
for(i in 1:length(unique(practice_set$subject))) {
  # loop through level
  for(j in 1:length(unique(practice_set$level))) {
    # create matrix of covariate values of distance, turning angles, points
    level_locations <- obj_location_data |> 
      dplyr::filter(level == paste("_level_", j))
    
    distances <- distance_matrix(level_locations)
    
    # loop through rows
    for(k in 1:length(practice_set$subject)) {
      used <- practice_set[k,]
      
    }
  }
}
