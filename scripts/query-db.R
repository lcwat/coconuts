## CONNECT TO THE DATABASE
## this script will establish a connection with the remote database hosted
## on my webserver



# load libraries ----------------------------------------------------------

library(DBI) # database access in R
library(RMariaDB) # con to MySQL db
library(dbplyr) # dplyr like functions that translate to SQL to manip database
library(tidyverse)


# establish con -----------------------------------------------------------

# establish and store connection
con <- dbConnect(
  MariaDB(),
  dbname = "4496971_gamedatabase", 
  host = "https://pdb1056.awardspace.net",
  port = 3306,
  username = "lcwat", 
  password = "coconuts-from-R-02-15"
)
