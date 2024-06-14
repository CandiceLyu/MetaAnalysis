# BUGS base case running script

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


# Initial values
inits <- 
  list(
    list(d = c(0, 0, 0, 0, 0, NA, NA, NA, NA)),
    list(d = c(-1, -3, -1, -3, -1, NA, NA, NA, NA)),
    list(d = c(1, -3, 1, -3, 1, NA, NA, NA, NA)),
    list(d = c(2, 4, 2, 4, 2, NA, NA, NA, NA)))

# Data
outcome_data <- read.csv(file = "raw_data/covid_vaccine_data.csv")

# jags set-up

filein <- "BUGS/Base-Case Model_BUGS_code.txt"
params <- c("rr")

n.iter <- 20000
n.burnin <- 1000
n.thin <- floor((n.iter - n.burnin)/500)

dataJags <-
  list(r =,
       n =,
       # r_pred
    
)

res_bugs <-
  R2OpenBUGS::bugs(
    data = dataJags,
    inits = NULL,
    parameters.to.save = params,
    model.file = filein,
    n.chains = 1,
    n.iter,
    n.burnin,
    n.thin,
    DIC = TRUE)

R2WinBUGS::attach.bugs(res_bugs$BUGSoutput)

mcmc_intervals