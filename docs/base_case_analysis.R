# Base-Case BUGS Model running script

# Library relevant packages
library(R2jags)        # For running JAGS models
library(dplyr)         # For data manipulation
library(mcmc)          # For MCMC diagnostics
library(coda)          # For MCMC output analysis
library(lattice)       # For data visualisation
library(R2OpenBUGS)    # Interface for OpenBUGS
library(mcmcplots)     # For MCMC diagnostic plots
library(bayesplot)     # For Bayesian data visualisation
library(ggplot2)       # For creating plots

# Specify the model file path
bc_model <- "C:/Users/lvtianying/Desktop/base_case_code.txt"

# Load data: observed outcomes and total number of vaccinated individuals
outcomes <- readxl::read_xlsx("node_r.xlsx")
total_vaccination <- readxl::read_xlsx("node_n.xlsx")
# Prepare data for the model (excluding the study ID column)
bc_r <- as.matrix(outcomes[, -3])
bc_n <- as.matrix(total_vaccination[, -3])
bc_data <- list(r = bc_r, n = bc_n)

# Define initial values for four MCMC chains
bc_inits <- list(
  list(d = rep(0, 9)),
  list(d = rep(0.7, 9)),
  list(d = rep(-3, 9)),
  list(d = c(2, 4, 2, 4, 2, 2, 4, 2, 4))
)

# Parameters to monitor during the model run
bc_params <- c("d", "rr", "totresdev", "sd")

# Run the model using OpenBUGS
base_case <- R2OpenBUGS::bugs(
  data = bc_data, 
  inits = bc_inits, 
  parameters.to.save = bc_params, 
  model.file = bc_model,
  n.chains = 4, 
  n.iter = 20000,
  n.burnin = 1000,
  n.thin = 1, 
  DIC = TRUE                         # Calculate DIC for model comparison
)

# Output the model summary
base_case$summary
# Create MCMC diagnostic plots for the relative risks
mcmcplots::mcmcplot(base_case, regex = "rr")
# Convert MCMC samples to 'mcmc' object for diagnostics
mcmc_samples <- as.mcmc(base_case)
# Plot traceplots to assess chain convergence
coda::traceplot(mcmc_samples)
# Calculate and display the Gelman-Rubin diagnostic for convergence
gelman.diag(mcmc_samples)
# Generate a Gelman-Rubin plot for visual inspection of convergence
gelman.plot(mcmc_samples)
