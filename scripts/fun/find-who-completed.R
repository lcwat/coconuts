## FILTER THROUGH SUBJECT DATA TO SEE WHO COMPLETED THE GAME AND ALL LEVELS
## this will be an automatic process that will help determine which subjects
## to be dropped from analysis (either from piloting or incomplete/dropped
## participation), can also pass in a specific level if interested for 
## leaderboard stuff for those who just want to see how they do when playing
## only one or two levels

# level passed in with the standard format "_level_x"
who_completed <- function(forage_df, lvl = "all") {
  # need to know who didn't complete to drop from level and overall leaderboards
  completed_level <- forage_df |> 
    group_by(subject, level) |> 
    summarize(
      points_gathered = max(points),
      completed_level = if_else(
        between(points_gathered, 698, 710), # points usually doesn't max at exactly 700
        "completed", 
        if_else(
          between(points_gathered, 995, 1030), # same for levels w 1000 point goal
          "completed", 
          "DNF"
        )
      )
    )
  
  if(lvl == "all") {
    # now see if there are 10 completed game levels (excluding tutorial)
    completed_game <- completed_level |> 
      group_by(subject) |> 
      summarize(
        completed_game = if_else(
          sum(completed_level == "completed") == 10, 
          "completed", 
          "DNF"
        )
      )
    
    # find which subj or levels to include based on completed
    subj <- completed_game |> 
      dplyr::filter(completed_game != "DNF") |> 
      pull(subject) # return vector
    
    subj <- unique(subj)
    
  } else {
    # filter for who completed on the selected level
    subj <- completed_level |> 
      dplyr::filter(level == lvl & completed_level == "completed") |> 
      pull(subject)
    
    subj <- unique(subj)
  }

  return(subj)
}
