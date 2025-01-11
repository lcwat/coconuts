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


