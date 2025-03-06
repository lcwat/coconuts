## MERGE FOOD LOCATION DATA WITH FORAGE DATA TO CREATE SEQUENCES FOR EACH 
## SUBJECT FOR EACH LEVEL
## these sequences will be used to calculate the conditional entropy and 
## compare traplining strategies of each subject 

create_sequences <- function(
    completed_plays_forage_df,
    level_df = obj_location_data, data_type = "subject"
  ) {
  # check whether simulation is set to true or false
  if(data_type == "simulation") {
    ## SIMULATED DATA
    # create and fill this df with sequence data with subject and level identifiers
    all_seq <-  tibble(
      forage_number = numeric(), nn_rule = numeric(), level = character(), 
      obj_ID = numeric(), order = numeric()
    )
    
    # turn simulated data into proper level string format
    completed_plays_forage_df <- completed_plays_forage_df |> 
      mutate(
        level = paste("_level_", level, sep = "")
      )
    
    # loop through all levels
    for(i in 1:length(unique(completed_plays_forage_df$level))) {
      # establish level character
      level_string <- paste("_level", i, sep = "_")
      
      # merge foraging data with level arrangement to assign ids and create sequence
      lvl_df <- level_df |> 
        dplyr::filter(level == level_string)
      
      forage_lvl_df <- completed_plays_forage_df |> 
        dplyr::filter(level == level_string) |> 
        left_join(lvl_df, join_by(x == x, y == y, level == level))
      
      # define subject numbers
      forage_number <- unique(forage_lvl_df$forage_number)
      
      # create empty tibble with same format as output, bind together all 
      # forage simulations on this level
      lvl_seq <- tibble(
        forage_number = numeric(), nn_rule = numeric(), level = character(), 
        obj_ID = numeric(), order = numeric()
      )
      
      # loop through subjects and return their respective sequences
      for (j in 1:length(forage_number)) {
        # use i to filter and pull out sequence from df and select subset of cols
        seq <- forage_lvl_df[
          which(forage_lvl_df[,3] == forage_number[j]), 
          c("forage_number", "nn_rule", "level", "obj_ID")
          ]
        
        # add a col to define order
        seq["order"] <- seq_along(seq$forage_number)
        
        # relocate to ensure proper binding to cols
        seq <- seq |> 
          relocate(
            forage_number, nn_rule, level, obj_ID, order
          )
        
        lvl_seq <- rbind(lvl_seq, seq)
      }
      
      # bind together each levels sequence
      all_seq <- rbind(all_seq, lvl_seq)
    }
  } else if(data_type == "subject") {
    ## SUBJECT DATA
    # create and fill this df with sequence data with subject and level identifiers
    all_seq <-  tibble(
      subject = numeric(), level = character(), 
      obj_ID = numeric(), order = numeric()
    )
    
    # loop through all levels
    for(i in 1:10) {
      # establish level character
      level_string <- paste("_level", i, sep = "_")
      
      # merge foraging data with level arrangement to assign ids and create sequence
      lvl_df <- level_df |> 
        dplyr::filter(level == level_string)
      
      forage_lvl_df <- completed_plays_forage_df |> 
        dplyr::filter(level == level_string) |> 
        left_join(lvl_df, join_by(x == x, y == y, level == level))
      
      # define subject numbers
      subj <- unique(forage_lvl_df$subject)
      
      lvl_seq <- tibble(
        subject = numeric(), level = character(), 
        obj_ID = numeric(), order = numeric()
      )
      
      # loop through subjects and return their respective sequences
      for (j in 1:length(subj)) {
        # use i to filter and pull out sequence from df and select subset of cols
        seq <- forage_lvl_df[which(forage_lvl_df[,1] == subj[j]), c("subject", "level", "obj_ID")]
        
        # put the seq together into 2 col df with seq and subj
        seq["order_collected"] <- seq.int(1, length(pull(seq[ ,1])), 1)
        
        lvl_seq <- rbind(lvl_seq, seq)
      }
      
      # bind together each levels sequence
      all_seq <- rbind(all_seq, lvl_seq)
    }
  } else {
    stop("Please specify a valid data type: either 'simulation' or 'subject'.")
  }
  
  return(all_seq)
}