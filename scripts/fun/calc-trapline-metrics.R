## CALCULATE TRAPLINING/ROUTINE MOVEMENT METRICS AUTOMATICALLY FROM SUBJECT 
## FORAGE DATA
## this script will calculate both entropy aka routine movement index (rmi) and 
## determinism (det) and store their values assigned to each subject in a neat 
## tibble. each range from 0 to 1 with 1 indicating greater routineness, but 
## they arrive to that judgment from different information

# speed up entropy calculations
library(compiler)

# source required functions
source("scripts/fun/determinism.R")
source("scripts/fun/entropy.R")
source("scripts/fun/AutoO.R")

trapline_metrics <- function(sequence_df) {
  # create table for output
  output <- tibble(
    subject = numeric(), 
    level = character(), 
    det = numeric(), 
    rmi = numeric(),
    ood = numeric()
  )
  
  # loop through df and find entropy value for each level, prob best to do this
  # on the desktop, takes quite a bit of memory to do, will be slow on laptop
  for(i in seq_along(unique(sequence_df$subject))) {
    # set subj number
    subj <- sequence_df$subject[[i]]
    
    # loop though the 10 levels
    for(j in 1:10) {
      # create level string to filter for the sequence
      level_string <- paste("_level_", j, sep = "")
      
      s <- sequence_df |> 
        dplyr::filter(subject == subj, level == level_string) |> 
        pull(obj_ID)
      
      # calculate determinism (d), entropy (e)/routine movement index (r), order 
      # of dependency (o) (additional info.)
      d <- determinism(s, 5)
      
      e <- entropy(s)
      
      r <- 1 - min(e)
      
      o <- AutoO(e)
      
      output <- output |> 
        add_row(
          subject = subj, level = level_string, det = d, rmi = r, ood = o
        )
    }
  }
  
  return(output)
}