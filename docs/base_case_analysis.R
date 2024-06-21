# BUGS Base-Case Model running script

# Library relevant packages
library(R2jags)
library(dplyr)
library(reshape2)
library(mcmc)
library(coda)
library(lattice)
library(R2OpenBUGS)
library(mcmcplots)
library(bayesplot)
library(ggplot2)
library(here)

# Model set-up
# filein <- "BUGS/Base-Case Model_BUGS_code.txt"
bc_model <- here("BUGS/Base_Case_Model_BUGS_code.txt")

# Data
events_data <- read.csv(here("raw_data/covid_vaccine_data_r.csv"))
total_vaccination_data <- read.csv(here("raw_data/covid_vaccine_data_n.csv"))

# Data for observed no. of events and total vaccinated people
# excluding the third column indicating the study id
r <- as.matrix(events_data[, -3])
n <- as.matrix(total_vaccination_data[, -3])
bc_data <- tibble::lst(r, n)


# Initial values for chains
bc_inits <- list(
  list(d = c(0, 0, 0, 0, 0, NA, NA, NA, NA)),
  list(d = c(-1, -3, -1, -3, -1, NA, NA, NA, NA)),
  list(d = c(1, -3, 1, -3, 1, NA, NA, NA, NA)),
  list(d = c(2, 4, 2, 4, 2, NA, NA, NA, NA))
)

# Parameters to monitor
bc_params <- c("d", "rr", "totresdev")

n.iter <- 20000
n.burnin <- 1000
n.thin <- floor((n.iter - n.burnin) / 500)


# Run WinBUGS using R2WinBUGS
base_case <- bugs(
  data = bc_data, 
  inits = bc_inits, 
  parameters.to.save = bc_params,
  model.file = bc_model,
  n.chains = 4, 
  n.iter = 20000,
  n.burnin = 1000,
  n.thin = 1,
  DIC = TRUE
  # debug = TRUE
  )


print(base_case)
base_case$summary


R2WinBUGS::attach.bugs(base_case)

mcmc_intervals
plot(base_case)
