## MERGE FOOD LOCATION DATA WITH FORAGE DATA TO CREATE SEQUENCES FOR EACH 
## SUBJECT FOR EACH LEVEL
## these sequences will be used to calculate the conditional entropy and 
## compare traplining strategies of each subject 

create_sequences <- function(
    completed_plays_forage_df, 
    level_df = obj_location_data
  ) {
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
      seq["order"] <- seq_along(seq[,1])
      
      lvl_seq <- rbind(lvl_seq, seq)
    }
    
    # bind together each levels sequence
    all_seq <- rbind(all_seq, lvl_seq)
  }
  
  return(all_seq)
}