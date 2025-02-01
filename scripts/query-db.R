## CONNECT TO THE DATABASE
## this script will establish a connection with the remote database hosted
## on my webserver



# load libraries ----------------------------------------------------------

library(DBI) # database access in R
library(RMariaDB) # con to MySQL db
library(tidyverse)
library(keyring) # access password for db

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
  password = key_get("coconuts", "r_user")
)

# have to add this user to db then specify ip address where it can be accessed
# by this user


## -----------------------------------------------------------------------------
##      subject data
## -----------------------------------------------------------------------------

# list tables in db
dbListTables(con)

# or read in a table as df, seems more sensible for working right now b/c
# the connection times out in 20s or so 
forage_data <- dbReadTable(con, "ForageData")
location_data <- dbReadTable(con, "LocationData")

dbDisconnect(con)

# or grab what has already been saved
forage_data <- read_csv("data/piloting/1-13-my-run-only.csv")

# plot path, specify subject no., level no., and matching level dataframe, 
# should work well with shiny app, although unsure of how showtext will look
plot_path(1, 10)

# consider only subjects who completed game
keep <- who_completed(my_run)

completed_only <- my_run |> 
  dplyr::filter(subject == keep)

# now get the sequences, resulting df is a little smaller b/c tutorial is 
# dropped
seq <- create_sequences(completed_only)

SEQ <- seq |> 
  dplyr::filter(level == "_level_10") |> 
  pull(obj_ID)

recurrencePlot(SEQ, m = 1, d = 0, eps = 1, nt = 1, end.time = 800, pch = 16, cex = .1)

seq <- seq |> 
  dplyr::filter(level != "_tutorial")

# run this function to generate both routine movement index and determinism #s
trap <- trapline_metrics(seq)

## -----------------------------------------------------------------------------
##      simulated data
## -----------------------------------------------------------------------------

# could have some issues with no subject col
# grab simulated data
simulated_forage_data <- read_csv("data/simulation/lvl1-2-100-rep-forage.csv")

summary(simulated_forage_data)

seq <- create_sequences(simulated_forage_data, data_type = "simulation")

trap <- trapline_metrics(seq, data_type = "simulation")

# view
trap |> 
  ggplot(aes(x = rmi, color = as.factor(nn_rule), fill = as.factor(nn_rule))) +
  
  geom_density(
    alpha = .5
  ) +
  
  facet_wrap(~level, labels = c("Level 1", "Level 2")) +
  
  theme_bw()

# see correlation of metrics
trap |> 
  ggplot() +
  
  geom_point(aes(x = det, y = rmi, color = as.factor(nn_rule))) +
  
  theme_bw()

# can see a clear main effect on the agents of level design, whether there are
# mixed sizes or not
# nn0 tends to not trapline as much in either scenario  