# Library relevant packages
library(brms)
library(metafor) 
library(rstan)
library(kableExtra) 
library(ggplot2) 
library(bayesplot)
library(gridExtra) 
library(ggplotify) 
library(ggridges) 
library(tidybayes) 
library(dplyr) 
library(glue) 
library(stringr) 
library(forcats)
library(posterior)

# Load outcome data
infec.data <- read.csv("infection_data.csv", header = TRUE)
sym.infec.data <- read.csv("symptomatic_infection_data.csv", header = TRUE)
sev.infec.data <- read.csv("severe_infection_data.csv", header = TRUE)
hos.data <- read.csv("hospitalisation_data.csv", header = TRUE)
death.data <- read.csv("death_data.csv", header = TRUE)

## Function to prepare data for Bayesian meta-analysis
#  Calculate log relative risks, relative risks, variances, and SEs
prepare_data <- function(data){
  es <- escalc(measure = "RR", 
               ai = n1, n1i = N1, 
               ci = n2, n2i = N2, 
               slab = author_id, data = data)
  es$RR <- exp(es$yi)
  es$sei <- sqrt(es$vi)
  # Remove the third and fourth columns 
  # corresponding to intervention indication variables
  es <- es[, -c(3, 4)]
  return(es)
}

# Prepare data for each outcome
infec.es <- prepare_data(infec.data)
sym.infec.es <- prepare_data(sym.infec.data)
sev.infec.es <- prepare_data(sev.infec.data)
hos.es <- prepare_data(hos.data)
death.es <- prepare_data(death.data)

## Function to run Bayesian meta-analysis and perform prior predictive checks
run_prior_predictive_checks <- function(data, model_name, priors, seed = 18006513){
  # Bayesian meta-analysis model using brms package
  model <- brm(
    data = data,
    family = "gaussian",
    yi | se(sei) ~ 1 + (1 | author_id),
    prior = priors,
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    control = list(adapt_delta = 0.99),
    sample_prior = "only",    # Prior predictive check (no data used)
    seed = seed
  )
  
  # Generate and store prior predictive check plot
  plot <- pp_check(model, ndraws = 100)
  assign(paste0(model_name, ".prior"), model, envir = .GlobalEnv)
  assign(paste0(model_name, ".priorpc.plot"), plot, envir = .GlobalEnv)
}

## Define different priors models
# Model 1: Non-informative Priors
priors_non_informative <- c(prior(normal(0, 10000), class = Intercept), 
                            prior(uniform(0, 10000), class = sd))

# Model 2: Informative Priors with Half-Normal for Heterogeneity
priors_informative <- c(prior(normal(-0.36, 0.2), class = Intercept), 
                        prior(normal(0, 0.5), class = sd, lb = 0))

# Model 3: Weakly Informative Priors with Half-Normal for Heterogeneity
priors_weakly_informative <- c(prior(normal(0, 1), class = Intercept),
                               prior(normal(0, 0.5), class = sd, lb = 0))

# Run prior predictive checks for each outcome and prior model
outcomes <- list("infec" = infec.es, 
                 "sym" = sym.infec.es, 
                 "sev" = sev.infec.es, 
                 "hos" = hos.es, 
                 "death" = death.es)
models <- list("1" = priors_non_informative,
               "2" = priors_informative, 
               "3" = priors_weakly_informative)

for (outcome in names(outcomes)) {
  for (model in names(models)) {
    run_prior_predictive_checks(outcomes[[outcome]], 
                                paste0(outcome, model), 
                                models[[model]])
  }
}

# Arrange and plot prior predictive checks for each outcome
plot_prior_checks <- function(outcome) {
  plots <- lapply(c("1", "2", "3"), 
                  function(m) get(paste0(outcome, m, ".priorpc.plot")))
  grid.arrange(grobs = plots)
}

# Plot results for all outcomes
plot_prior_checks("infec")
plot_prior_checks("sym")
plot_prior_checks("sev")
plot_prior_checks("hos")
plot_prior_checks("death")