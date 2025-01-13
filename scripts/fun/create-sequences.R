## MERGE FOOD LOCATION DATA WITH FORAGE DATA TO CREATE SEQUENCES FOR EACH 
## SUBJECT FOR EACH LEVEL
## these sequences will be used to calculate the conditional entropy and 
## compare traplining strategies of each subject 

create_sequences <- function(
    forage_df, 
    level_list = list(
      level_1, level_2, level_3, level_4, level_5, level_6, level_7, level_8, 
      level_9, level_10
      )
  ) {
  # check to see if level list is populated
  if(length(level_list) < 10) {
    stop("Please load in the level data frames (level_1, level_2, ...)")
  }
  
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
    lvl <- forage_df |> 
      filter(level == level_string) |> 
      left_join(level_list[[i]], join_by(x == x, y == y))
    
    # define subject numbers
    subj <- unique(lvl$subject)
    
    lvl_seq <- tibble(
      subject = numeric(), level = character(), 
      obj_ID = numeric(), order = numeric()
    )
    
    # loop through subjects and return their respective sequences
    for (j in 1:length(subj)) {
      # use i to filter and pull out sequence from df
      seq <- lvl[which(lvl[,1] == subj[j]), c("subject", "level", "obj_ID")]
      
      # put the seq together into 2 col df with seq and subj
      seq["order"] <- seq_along(seq[,1])
      
      lvl_seq <- rbind(lvl_seq, seq)
    }
    
    # bind together each levels sequence
    all_seq <- rbind(all_seq, lvl_seq)
  }
  
  return(all_seq)
}