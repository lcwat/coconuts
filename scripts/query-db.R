## CONNECT TO THE DATABASE
## this script will establish a connection with the remote database hosted
## on my webserver



# load libraries ----------------------------------------------------------

library(DBI) # database access in R
library(RMariaDB) # con to MySQL db
library(tidyverse)

# load arrangements -------------------------------------------------------

obj_location_data <- read_csv("data/arrangements/object-location-data.csv")

# source functions --------------------------------------------------------

source("scripts/fun/calc-perform-metrics.R")
source("scripts/fun/find-who-completed.R")
source("scripts/fun/create-sequences.R")
source("scripts/fun/plot-path.R")
source("scripts/fun/calc-trapline-metrics.R")

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

dbDisconnect(con)



# plot path, specify subject no., level no., and matching level dataframe, 
# should work well with shiny app, although unsure of how showtext will look
plot_path(1, 10)

# consider only subjects who completed game
keep <- who_completed(forage_data)

completed_only <- forage_data |> 
  dplyr::filter(subject == keep)

# now get the sequences, resulting df is a little smaller b/c tutorial is 
# dropped
seq <- create_sequences(completed_only)

SEQ <- seq |> 
  dplyr::filter(level == "_level_10") |> 
  pull(obj_ID)

# check out recurrance plot and determinism
recurrencePlot(SEQ, m = 1, d = 0, eps = 1, nt = 1, end.time = 800, pch = 16, cex = .1)

# run this function to generate both routine movement index and determinism #s
trap <- trapline_metrics(seq)




