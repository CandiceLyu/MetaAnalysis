# Set seed for reproducibility
set.seed(18006513)

# Function to generate trace plots and calculate R-hat statistics
plot_and_rhat <- function(model, params){
  # Prepare posterior array
  posterior_array <- as.array(model)
  dimnames(posterior_array)$variable <- c("Intercept", "Tau", "sigma",
                                          "Intercept2", params, 
                                          "lprior", "lp__")
  # Trace plots
  trace_plot <- mcmc_trace(posterior_array, 
                           pars = c("Intercept", "Tau", params))
  print(trace_plot)
  # R-hat statistics
  print(rhat(model))
}

# Function to perform posterior predictive checks
posterior_predictive_check <- function(model, ndraws) {
  pp_check_plot <- pp_check(model, ndraws = ndraws)
  print(pp_check_plot)
}

# List of outcome data and associated parameter names
outcome_list <- list(
  infec = list(data = infec.es, params = paste("RE:", 1:15)),
  sym_infec = list(data = sym.infec.es, params = paste("RE:", 1:5)),
  sev_infec = list(data = sev.infec.es, params = paste("RE:", 1:6)),
  hos = list(data = hos.es, params = paste("RE:", 1:4)),
  death = list(data = death.es, params = paste("RE:", 1:3))
)

# Define prior sets
priors_list <- list(
  priors_non_informative,
  priors_informative,
  priors_weakly_informative
)

# Loop through each outcome and prior model
for (outcome_name in names(outcome_list)) {
  outcome_data <- outcome_list[[outcome_name]]$data
  outcome_params <- outcome_list[[outcome_name]]$params
  
  for (i in seq_along(priors_list)) {
    # Run Bayesian model with the current prior
    model <- brm(
      data = outcome_data,
      family = "gaussian",
      yi | se(sei) ~ 1 + (1 | author_id),
      prior = priors_list[[i]],
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      control = list(adapt_delta = 0.99),
      seed = 18006513
    )
    
    # Model summary, trace plot, R-hat, and posterior predictive checks
    print(summary(model))
    plot_and_rhat(model, outcome_params)
    posterior_predictive_check(model)
  }
}