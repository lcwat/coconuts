## CONNECT TO THE DATABASE
## this script will establish a connection with the remote database hosted
## on my webserver



# load libraries ----------------------------------------------------------

library(DBI) # database access in R
library(RMariaDB) # con to MySQL db
library(dbplyr) # dplyr like functions that translate to SQL to manip database
library(tidyverse)


# load arrangements -------------------------------------------------------

level_1 <- read_csv("data/arrangements/level-1.csv")
level_2 <- read_csv("data/arrangements/level-2.csv")
level_3 <- read_csv("data/arrangements/level-3.csv")
level_4 <- read_csv("data/arrangements/level-4.csv")
level_5 <- read_csv("data/arrangements/level-5.csv")
level_6 <- read_csv("data/arrangements/level-6.csv")
level_7 <- read_csv("data/arrangements/level-7.csv")
level_8 <- read_csv("data/arrangements/level-8.csv")
level_9 <- read_csv("data/arrangements/level-9.csv")
level_10 <- read_csv("data/arrangements/level-10.csv")

# source functions --------------------------------------------------------

source("scripts/fun/calc-perform-metrics.R")
source("scripts/fun/find-who-completed.R")
source("scripts/fun/create-sequences.R")
source("scripts/fun/plot-path.R")
source("scripts/fun/determinism.R")
source("scripts/fun/entropy.R")
source("scripts/fun/AutoO.R")

# establish con -----------------------------------------------------------

# establish and store connection
con <- dbConnect(
  MariaDB(),
  dbname = "psych270_coconuts", 
  host = "s161.servername.online",
  port = 3306,
  username = "psych270_data_access", 
  password = "K=2vtcy1W3wq"
)

# have to add this user to db then specify ip address where it can be accessed
# by this user

# list tables in db
dbListTables(con)

# or read in a table as df, seems more sensible for working right now b/c
# the connection times out in 20s or so 
forage_data <- dbReadTable(con, "ForageData")
location_data <- dbReadTable(con, "LocationData")

# try out function
perform_summary <- calc_perform_metrics(location_data)
 
# will give the number of "steps" and time in each level for each subject
# get totals for overall leaderboard
totals <- perform_summary |> 
  group_by(subject) |> 
  summarize(
    total_distance = sum(total_dist_in_lvl), 
    total_time = sum(total_time_in_lvl)
  ) |> 
  arrange(total_distance)

# plot path, specify subject no., level no., and matching level dataframe
plot_path(1, 6, level_6)

# consider only subjects who completed game
keep <- who_completed(forage_data)

completed_only <- forage_data |> 
  filter(subject == keep)

# now get the sequences, resulting df is a little smaller b/c tutorial is 
# dropped
seq <- create_sequences(completed_only)

# save
write_csv(completed_only, "data/piloting/1-13-my-run-only.csv")
write_csv(seq, "data/piloting/1-13-sequences.csv")

seq <- read_csv("data/piloting/1-13-sequences.csv")

SEQ <- seq |> 
  filter(level == "_level_10") |> 
  pull(obj_ID)

# check out recurrance plot and determinism
recurrencePlot(SEQ, m = 1, d = 0, eps = 1, nt = 1, end.time = 800, pch = 16, cex = .1)

# could use piloting to decide appropriate minimum L value, 5 seems appropriate for now
determinism(SEQ, 5)

# loop through df and find determinism value for each level, save to new df
for(i in 1:10) {
  # create level string to filter for the sequence
  level_string <- paste("_level_", i, sep = "")
  
  SEQ <- seq |> 
    filter(level == level_string) |> 
    pull(obj_ID)
  
  # calculate
  deter <- determinism(SEQ, 5)
  
  det_table <- det_table |> 
    add_row(
      subject = 1, level = i, det = deter
    )
}

# create table for output
output <- tibble(
  subject = numeric(), 
  level = numeric(), 
  det = numeric(), 
  rmi = numeric(),
  ood = numeric()
)

# loop through df and find entropy value for each level, prob best to do this
# on the desktop, takes quite a bit of memory to do, will be slow on laptop
for(i in seq_along(seq$subject)) {
  # set subj number
  subj <- seq$subject[[i]]
  
  # loop though the 10 levels
  for(j in 1:10) {
    # create level string to filter for the sequence
    level_string <- paste("_level_", j, sep = "")
    
    s <- seq |> 
      filter(subject == subj, level == level_string) |> 
      pull(obj_ID)
    
    # calculate determinism (d), entropy (e)/routine movement index (r), order 
    # of dependency (o) (additional info.)
    d <- determinism(s, 5)
    
    e <- entropy(s)
    
    r <- 1 - min(e)
    
    o <- AutoO(e)
    
    output <- output |> 
      add_row(
        subject = subj, level = j, det = deter, rmi = r, ood = o
      )
  }
}


