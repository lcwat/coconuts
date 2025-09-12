## CONNECT TO THE DATABASE
## this script will establish a connection with the remote database hosted
## on my webserver



# load libraries ----------------------------------------------------------

# library(sys)
# library(ssh)
# library(glue)
library(DBI) # database access in R
library(RMariaDB) # con to MySQL db
library(tidyverse)
library(patchwork)
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
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
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

# or read in a table as df, seems more sensible for working right now b/c
# the connection times out in 20s or so 
forage_data <- dbReadTable(con, "ForageData")
location_data <- dbReadTable(con, "LocationData")

dbDisconnect(con)

# read in trapline pilot data
metrics <- read_csv("data/piloting/3-4-25-all-metrics.csv")
# read local data
forage_data <- read_csv("data/piloting/3-4-25-forage-piloting-data.csv")
location_data <- read_csv("data/piloting/3-4-25-path-piloting-data.csv")

# write to local drive
write_csv(forage_data, "data/piloting/3-4-25-forage-piloting-data.csv")
write_csv(location_data, "data/piloting/3-4-25-path-piloting-data.csv")


# read in data and look ---------------------------------------------------

# see the level order
forage_data |> 
  dplyr::filter(subject == 8) |> 
  pull(level) |> 
  unique()

# plot path, specify subject no., level no., and matching level dataframe, 
# should work well with shiny app, although unsure of how showtext will look
p1 <- plot_path(6, 3) + guides(color = "none")
p2 <- plot_path(8, 3)

p1 + p2

ggsave("fig_output/pilot_path_comparison.png", device = "png", width = 13, height = 6, units = "in")

plot_path(8, 6)

# consider only subjects who completed game
keep <- who_completed(forage_data)

completed_only <- forage_data |> 
  dplyr::filter(
    subject == keep[1] | subject == keep[2] | subject == keep[3] |
      subject == keep[4] | subject == keep[5] | subject == 4
    )

# now get the sequences, resulting df is a little smaller b/c tutorial is 
# dropped
seq <- create_sequences(completed_only)

# check to see completion of levels
seq |> 
  dplyr::filter(subject == 4) |> 
  pull(level) |> 
  unique()

recurrencePlot(SEQ, m = 1, d = 0, eps = 1, nt = 1, end.time = 800, pch = 16, cex = .1)

seq <- seq |> 
  dplyr::filter(level != "_tutorial")

# run this function to generate both routine movement index and determinism #s
trap <- trapline_metrics(seq)

# write
write_csv(trap, "data/piloting/3-4-25-trapline-metrics.csv")

perform <- calc_perform_metrics(location_data) |> 
  dplyr::filter(
    subject == keep[1] | subject == keep[2] | subject == keep[3] |
      subject == keep[4] | subject == keep[5] | subject == 4
  )

perform <- perform |> 
  dplyr::filter(level != "_tutorial")

metrics <- left_join(perform, trap)

# save 
write_csv(metrics, "data/piloting/3-4-25-all-metrics.csv")

# function for plotting route with trapline metrics
plot_w_metrics <- function(subj, lvl) {
  p <- plot_path(subj, lvl) +
    annotate("text", x = 0, y = -31, label = paste("Det = ", format(metrics$det[lvl], digits = 2), sep = "")) +
    annotate("text", x = 0, y = -35, label = paste("R = ", format(metrics$rmi[lvl], digits = 2), sep = ""))
  
  p
}

plot_w_metrics(1, 1)

## -----------------------------------------------------------------------------
##      simulated data
## -----------------------------------------------------------------------------


# trapline metric creation ------------------------------------------------

# # could have some issues with no subject col
# # grab simulated data
# simulated_forage_data <- read_csv("data/simulation/lvl1-2-100-rep-forage.csv")
# 
# summary(simulated_forage_data)
# 
# seq <- create_sequences(simulated_forage_data, data_type = "simulation")
# 
# # find how many unique coconuts were collected
# summary <- seq |> 
#   group_by(level, forage_number, nn_rule) |> 
#   summarize(
#     n_unique = length(unique(obj_ID))
#   )
# # # calculate trapline metrics 
# # trap <- trapline_metrics(seq, data_type = "simulation")
# 
# # read in trapline metrics
# traplines <- read_csv("data/simulation/lvl1-2-simulated-trapline-metrics.csv")
# 
# # read in performance
# sim_performance <- read_csv("data/simulation/lvl1-2-100-rep-performance.csv")
# 
# sim_performance <- sim_performance |> 
#   mutate(
#     level = paste("_level_", level, sep = "")
#   )
# 
# # join together
# perf_and_trapline <- left_join(
#   perf_and_trapline, summary, 
#   join_by(level == level, forage_number == forage_number, nn_rule == nn_rule)
#   )
# 
# write_csv(perf_and_trapline, "data/simulation/lvl1-2-all-metrics.csv")


# trapline analysis -------------------------------------------------------

perf_and_trapline <- read_csv("data/simulation/lvl1-2-all-metrics.csv")

# view
metrics |> 
  ggplot(aes(x = det)) +
  
  geom_density(
    alpha = .5, fill = "grey80"
  ) +
  
  facet_wrap(~level) +
  
  theme_bw()

metrics |> 
  ggplot(aes(x = det, y = total_dist_in_lvl, color = as.factor(subject))) +
  
  geom_point() +
  
  facet_wrap(~level) +
  
  theme_bw()

metrics |> 
  ggplot(aes(x = as.factor(subject), y = total_dist_in_lvl, fill = as.factor(subject))) +
  
  geom_bar(stat = "identity") +
  
  facet_wrap(~level) +
  
  theme_bw()

# see correlation of metrics
perf_and_trapline |> 
  ggplot() +
  
  geom_point(aes(x = det, y = steps, color = as.factor(nn_rule))) +
  
  scale_color_viridis_d("NN rule", option = "turbo") +
  
  theme_bw() + 
  
  facet_wrap(~level)

# try fitting a model
library(lme4)

# center
perf_and_trapline <- perf_and_trapline |> 
  mutate(
    c.rmi = rmi - mean(rmi), 
    c.det = det - mean(det), 
    c.n_unique = n_unique - mean(n_unique), 
    nn_rule = as.factor(nn_rule)
  )

contrasts(perf_and_trapline$nn_rule) <- contr.sum(6)

contrasts(perf_and_trapline$nn_rule)

perf_model <- lmer(
  steps ~ c.rmi * c.det * c.n_unique + (c.rmi * c.det * c.n_unique | level:nn_rule), # nested by level
  data = perf_and_trapline
)

perf_model <- lmer(
  steps ~ c.rmi * c.det * c.n_unique + (c.rmi * c.det * c.n_unique | nn_rule) + (1 | level), # nested by level
  data = perf_and_trapline
)

# see how the strategies and performance are changing across levels and subjects (nn)
model_step <- lmer(
  steps ~ level + (level | nn_rule), 
  data = perf_and_trapline
)

model_rmi <- lmer(
  rmi ~ level + (level | nn_rule), 
  data = perf_and_trapline
)

hist(perf_and_trapline$c.n_unique)

coef(perf_model)

summary(perf_model)

library(performance)
library(marginaleffects)
library(emmeans)

check_model(perf_model)

# create new data to visualize
toplot <- data.frame(
  emmeans(perf_model, ~c.rmi * c.det*c.n_unique, at=list(
    c.rmi = seq(.4,1,.1) - mean(perf_and_trapline$rmi), 
    c.det = c(.2, .4, .6) - mean(perf_and_trapline$det), 
    c.n_unique = c(24, 36, 52) - mean(perf_and_trapline$c.n_unique)
  ))
)

toplot <- data.frame(
  emmeans(perf_model, ~c.rmi, at=list(
    c.rmi = seq(.4,1,.1) - mean(perf_and_trapline$rmi)
  ))
)

toplot

ggplot(toplot, aes(x = c.rmi, y = emmean)) +
  
  geom_line() +
  
  geom_ribbon(aes(ymin = emmean - SE, ymax = emmean + SE), alpha = .3, color = NA) +
  geom_point(data = perf_and_trapline, aes(y = steps))

ggplot(toplot, aes(x = c.rmi, y = emmean, color = as.factor(c.det))) +
  
  geom_line() +
  
  geom_ribbon(
    aes(
      ymin = emmean - SE, ymax = emmean + SE, fill = as.factor(c.det)
    ), alpha = .3, color = NA
  ) +
  facet_grid(~c.n_unique)

perf_and_trapline$fitted = fitted(perf_model)

ggplot(perf_and_trapline, aes(x = rmi, y = fitted)) +
  
  geom_point() +
  
  facet_grid(~level~nn_rule)


# add col
newd <- newd |> 
  add_column(predicted = fit)

ggplot() +
  
  # raw data
  geom_point(data = perf_and_trapline, aes(x = c.rmi, y = steps, color = as.factor(nn_rule))) +
  
  # fits
  geom_line(data = newd, aes(x = c.rmi, y = predicted, color = as.factor(nn_rule))) +
  
  theme_bw() +
  
  facet_wrap(~as.factor(level))

# can see a clear main effect on the agents of level design, whether there are
# mixed sizes or not
# nn0 tends to not trapline as much in either scenario  

# NEXT STEPS 
# can work on integrating performance metrics of simulation with the traplining
# data along with any other predictors of interest like average size of coconuts
# or number of unique coconuts visited