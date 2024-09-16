# BUGS Node-Splitting Model running script

# Library relevant packages
library(R2jags)        # Interface to JAGS for Bayesian analysis
library(dplyr)         # Data manipulation
library(mcmc)          # MCMC diagnostics
library(coda)          # Output analysis and diagnostics for MCMC
library(lattice)       # Trellis graphics
library(R2OpenBUGS)    # Interface to OpenBUGS for Bayesian analysis
library(mcmcplots)     # MCMC diagnostic plots
library(bayesplot)     # Bayesian plotting tools
library(ggplot2)       # Data visualisation

# Specify the file path for the BUGS model
ns_model <- "C:/Users/lvtianying/Desktop/node_splitting_model.txt"

# Load data: observed outcomes and total number of vaccinated individuals
outcomes <- read.csv("covid_outcome_r.csv", header = TRUE)
total_vaccination <- read.csv("covid_total_n.csv", header = TRUE)
# Prepare data for the model (excluding the study ID column)
ns_r <- as.matrix(outcomes[, -3])
ns_n <- as.matrix(total_vaccination[, -3])
ns_data <- list(r = ns_r, n = ns_n)

# Define initial values for the chains
ns_inits <- list(
  list(d = rep(0, 13)),            
  list(d = rep(0.7, 13)),          
  list(d = rep(-3, 13)),           
  list(d = c(2, 4, 2, 4, 2, 2, 4, 2, 4, 2, 2, 4, 2))
)
# Specify parameters to monitor
ns_params <- c("d", "rr", "p.val", "totresdev", "sd")

# Run the Bayesian model using OpenBUGS
node_splitting <- R2OpenBUGS::bugs(
  data = ns_data, 
  inits = ns_inits, 
  parameters.to.save = ns_params, 
  model.file = ns_model,
  n.chains = 4, 
  n.iter = 20000,
  n.burnin = 1000,
  n.thin = 1, 
  DIC = TRUE)

# Display summary of the model
node_splitting$summary

# Extract and process MCMC samples
result <- node_splitting$summary
samples <- node_splitting$sims.list
# Extract specific transition data
direct_1_to_2 <- samples$d[, 6]
indirect_1_to_2 <- samples$d[, 10]
direct_2_to_3 <- samples$d[, 7]
indirect_2_to_3 <- samples$d[, 11]
direct_3_to_4 <- samples$d[, 8]
indirect_3_to_4 <- samples$d[, 12]
direct_4_to_5 <- samples$d[, 9]
indirect_4_to_5 <- samples$d[, 13]

# Extract and compute Bayesian p-values
p_values <- c(
  result["p.val[1]", 1], 
  result["p.val[2]", 1], 
  result["p.val[3]", 1], 
  result["p.val[4]", 1]
)
compute_bayesian_p <- function(p) {
  if (p > 0.5) {
    return(2 * (1 - p))     # Two-tailed p-value for p > 0.5
  } else {
    return(2 * p)           # Two-tailed p-value for p <= 0.5
  }
}
bayesian_p_values <- sapply(p_values, compute_bayesian_p)

# Create data frame for plotting
plot_data <- data.frame(
  Transition = factor(rep(c("(a) 1 to 2", "(b) 2 to 3", 
                            "(c) 3 to 4", "(d) 4 to 5"), 
                          each = 2 * length(direct_1_to_2))),
  Estimate = c(direct_1_to_2, indirect_1_to_2, direct_2_to_3, indirect_2_to_3, 
               direct_3_to_4, indirect_3_to_4, direct_4_to_5, indirect_4_to_5),
  Type = rep(c("Direct", "Indirect"), each = length(direct_1_to_2)),
  Pvalue = rep(bayesian_p_values, each = 2 * length(direct_1_to_2))
)

# Plot density of estimates with p-values
# for checking evidence consistency
ggplot(plot_data, aes(x = Estimate, fill = Type, linetype = Type)) +
  geom_density(alpha = 0.5)  +
  facet_wrap(~ Transition) +
  theme_minimal() +
  labs(x = "Log Risk Ratio",
       y = "Density") +
  geom_text(data = unique(plot_data[, c("Transition", "Pvalue")]), 
            aes(x = Inf, y = Inf, 
                label = paste("p =", format(Pvalue, digits = 2))), 
            hjust = 1.1, vjust = 1.1, 
            size = 5, color = "black",
            inherit.aes = FALSE) +
  theme(strip.text = element_text(size = 14)) +
  theme(legend.text = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))

# Plot MCMC diagnostics
mcmcplots::mcmcplot(node_splitting, regex = "rr")

# Convert to MCMC object and plot trace plots
mcmc_samples2 <- as.mcmc(node_splitting)
coda::traceplot(mcmc_samples2)

# Perform Gelman-Rubin diagnostic
gelman.diag(mcmc_samples2)
# Plot Gelman-Rubin diagnostics for convergence
gelman.plot(mcmc_samples2)