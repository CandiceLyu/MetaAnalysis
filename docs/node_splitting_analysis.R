# BUGS node-splitting model running script

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


# Initial values for chains
ns_inits <- list(
  list(d = c(0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA)),
  list(d = c(-1, -3, -1, -3, -1, NA, NA, NA, NA, NA, NA, NA, NA)),
  list(d = c(1, -3, 1, -3, 1, NA, NA, NA, NA, NA, NA, NA, NA)),
  list(d = c(2, 4, 2, 4, 2, NA, NA, NA, NA, NA, NA, NA, NA))
)

# Data
events_data <- read.csv("covid_vaccine_data_r.csv")
total_vaccination_data <- read.csv("covid_vaccine_data_n.csv")
r <- as.matrix(events_data[, -3])
n <- as.matrix(total_vaccination_data[, -3])
ns_data <- list(r = r, n = n)

# Parameters to monitor
ns_params <- c("d", "rr", "totresdev")

# Jags set-up
filein <- "Node_Splitting_Model_BUGS_code.txt"

n.iter <- 20000
n.burnin <- 1000
n.thin <- floor((n.iter - n.burnin) / 500)

dataJags <-
  list(r =,
       n =,
       # r_pred
       
  )


# Run WinBUGS using R2WinBUGS
node_splitting <- bugs(
  data = ns_data, 
  inits = ns_inits, 
  parameters.to.save = ns_params, 
  model.file = "node_splitting_model_BUGS_code.txt",
  n.chains = 4, 
  n.iter = 20000,
  n.burnin = 1000,
  n.thin = 1,
  DIC = TRUE,
  debug = TRUE)

print(node_splitting)
node_splitting$summary


R2WinBUGS::attach.bugs(node_splitting)

plot(node_splitting)
