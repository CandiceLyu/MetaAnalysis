# R script for generating forest plots
# for separate Bayesian meta-analyses

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

# Load data for the five outcomes
infec.data <- read.csv("infection_data.csv", header = TRUE)
sym.infec.data <- read.csv("symptomatic_infection_data.csv", header = TRUE)
sev.infec.data <- read.csv("severe_infection_data.csv", header = TRUE)
hos.data <- read.csv("hospitalisation_data.csv", header = TRUE)
death.data <- read.csv("death_data.csv", header = TRUE)

# Function to prepare data for Bayesian meta-analysis
# Calculate log relative risks, relative risks, variances, and SEs
prepare_data <- function(data){
  es <- escalc(measure = "RR", 
               ai = n1, n1i = N1, 
               ci = n2, n2i = N2, 
               slab = author_id, data = data)
  es$RR <- exp(es$yi)
  es$sei <- sqrt(es$vi)
  es <- es[, -c(3, 4)]   # Remove unnecessary columns
  return(es)
}

# Prepare data for each outcome
infec.es <- prepare_data(infec.data)
sym.infec.es <- prepare_data(sym.infec.data)
sev.infec.es <- prepare_data(sev.infec.data)
hos.es <- prepare_data(hos.data)
death.es <- prepare_data(death.data)

# Define weakly informative priors
priors_weakly <- c(prior(normal(0, 1), class = Intercept), 
                        prior(normal(0, 0.5), class = sd, lb = 0))

########################################################################
# 1.Infection
# Fit Bayesian meta-analysis model
infec.bayes <- brm(data = infec.es,
                   family = "gaussian",
                   yi | se(sei) ~ 1 + (1 | author_id),
                   prior = priors_weakly,
                   iter = 2000, warmup = 1000, cores = 4, chains = 4,
                   control = list(adapt_delta = 0.99),
                   seed = 18006513)

# Summary of the Bayesian model
summary(infec.bayes)
# Rhat value for both parameters is 1, signifying convergence

pp_check(infec.bayes, ndraws = 200)

# Prepare Data for Forest Plot
# Spread draws for each study and compute b_Intercept
infec.draws <- spread_draws(infec.bayes, r_author_id[author_id,], b_Intercept) %>%
  mutate(b_Intercept = r_author_id + b_Intercept) %>%
  mutate(author_id = as.character(author_id))

# Pooled effect
infec.pooled.draws <- spread_draws(infec.bayes, b_Intercept) %>%
  mutate(author_id = "pooled effect")

# Combine individual study and pooled effect data frames
author_levels1 <- c(as.character(1:15), "pooled effect")
infec.forest <- bind_rows(infec.draws, infec.pooled.draws) %>%
  ungroup() %>%
  mutate(author_id = factor(author_id, levels = author_levels1))

# Calculate summary statistics
infec.summ <- group_by(infec.forest, author_id) %>%
  mean_qi(b_Intercept)

# Exponentiate to get risk ratios
infec.forest <- infec.forest %>%
  mutate(b_Intercept = exp(b_Intercept))

infec.summ <- infec.summ %>%
  mutate(b_Intercept = exp(b_Intercept),
         .lower = exp(.lower),
         .upper = exp(.upper))

# Forest plot
ggplot(aes(x = b_Intercept, y = author_id), data = infec.forest) +
  
  # Add vertical lines for pooled effect and CI, and RR = 1
  geom_vline(xintercept = exp(fixef(infec.bayes)[1, 1]), 
             color = "red", size = 1) +
  geom_vline(xintercept = exp(fixef(infec.bayes)[1, 3:4]), 
             color = "red", linetype = 2) +
  geom_vline(xintercept = 1, color = "black", 
             size = 1) +
  
  # Add densities
  geom_density_ridges(fill = "dodgerblue", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointintervalh(data = infec.summ, 
                      size = 1) +
  
  # Add text and labels
  geom_text(data = mutate_if(infec.summ, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(x = "Risk Ratio", y = "Author ID") +
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(infec.summ$author_id))) 

########################################################################
# 2.Symptomatic Infection
sym.bayes <- brm(data = sym.infec.es,
                 family = "gaussian",
                 yi | se(sei) ~ 1 + (1 | author_id),
                 prior = priors_weakly,
                 iter = 2000, warmup = 1000, cores = 4, chains = 4,
                 control = list(adapt_delta = 0.99),
                 seed = 18006513)

summary(sym.bayes)
pp_check(sym.bayes, ndraws = 100)

# Prepare Data for Forest Plot
sym.draws <- spread_draws(sym.bayes, r_author_id[author_id,], b_Intercept) %>%
  mutate(b_Intercept = r_author_id + b_Intercept) %>%
  mutate(author_id = as.character(author_id))

# Pooled effect
sym.pooled.draws <- spread_draws(sym.bayes, b_Intercept) %>%
  mutate(author_id = "pooled effect")

# Combine individual study and pooled effect data frames
author_levels2<- c(as.character(1:5), "pooled effect")
sym.forest <- bind_rows(sym.draws, sym.pooled.draws) %>%
  ungroup() %>%
  mutate(author_id = factor(author_id, levels = author_levels2)) 

# Calculate summary statistics
sym.summ <- group_by(sym.forest, author_id) %>%
  mean_qi(b_Intercept)

# Exponentiate to get risk ratios
sym.forest <- sym.forest %>%
  mutate(b_Intercept = exp(b_Intercept))

sym.summ <- sym.summ %>%
  mutate(b_Intercept = exp(b_Intercept),
         .lower = exp(.lower),
         .upper = exp(.upper))

# Forest plot
ggplot(aes(x = b_Intercept, y = author_id), data = sym.forest) +
  
  geom_vline(xintercept = exp(fixef(sym.bayes)[1, 1]), 
             color = "red", size = 1) +
  geom_vline(xintercept = exp(fixef(sym.bayes)[1, 3:4]), 
             color = "red", linetype = 2) +
  geom_vline(xintercept = 1, color = "black", 
             size = 1) +
  
  geom_density_ridges(fill = "dodgerblue", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointintervalh(data = sym.summ, 
                      size = 1) +
  
  geom_text(data = mutate_if(sym.summ, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(x = "Risk Ratio", y = "Author ID") +
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(sym.summ$author_id)))

########################################################################
# 3.Severe Infection
sev.bayes <- brm(data = sev.infec.es,
                 family = "gaussian",
                 yi | se(sei) ~ 1 + (1 | author_id),
                 prior = priors_weakly,
                 iter = 2000, warmup = 1000, cores = 4, chains = 4,
                 control = list(adapt_delta = 0.99),
                 seed = 18006513)

summary(sev.bayes)
pp_check(sev.bayes, ndraws = 100)

# Prepare Data for Forest Plot
# Spread draws for each study and compute b_Intercept
sev.draws <- spread_draws(sev.bayes, r_author_id[author_id,], b_Intercept) %>%
  mutate(b_Intercept = r_author_id + b_Intercept) %>%
  mutate(author_id = as.character(author_id))

# Pooled effect
sev.pooled.draws <- spread_draws(sev.bayes, b_Intercept) %>%
  mutate(author_id = "pooled effect")

# Combine individual study and pooled effect data frames
author_levels3<- c(as.character(1:6), "pooled effect")
sev.forest <- bind_rows(sev.draws, sev.pooled.draws) %>%
  ungroup() %>%
  mutate(author_id = factor(author_id, levels = author_levels3)) 

# Calculate summary statistics
sev.summ <- group_by(sev.forest, author_id) %>%
  mean_qi(b_Intercept)

# Exponentiate to get risk ratios
sev.forest <- sev.forest %>%
  mutate(b_Intercept = exp(b_Intercept))

sev.summ <- sev.summ %>%
  mutate(b_Intercept = exp(b_Intercept),
         .lower = exp(.lower),
         .upper = exp(.upper))

# Forest plot
ggplot(aes(x = b_Intercept, y = author_id), data = sev.forest) +

  geom_vline(xintercept = exp(fixef(sev.bayes)[1, 1]), 
             color = "red", size = 1) +
  geom_vline(xintercept = exp(fixef(sev.bayes)[1, 3:4]), 
             color = "red", linetype = 2) +
  geom_vline(xintercept = 1, color = "black", 
             size = 1) +
  
  geom_density_ridges(fill = "dodgerblue", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointintervalh(data = sev.summ, 
                      size = 1) +
  
  geom_text(data = mutate_if(sev.summ, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(x = "Risk Ratio", y = "Author ID") +
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(sev.summ$author_id)))

########################################################################
# 4.Hospitalisation
hos.bayes <- brm(data = hos.es,
                 family = "gaussian",
                 yi | se(sei) ~ 1 + (1 | author_id),
                 prior = priors_weakly,
                 iter = 2000, warmup = 1000, cores = 4, chains = 4,
                 control = list(adapt_delta = 0.99),
                 seed = 18006513)

summary(hos.bayes)
pp_check(hos.bayes, ndraws = 100)

# Prepare Data for Forest Plot
# Spread draws for each study and compute b_Intercept
hos.draws <- spread_draws(hos.bayes, r_author_id[author_id,], b_Intercept) %>%
  mutate(b_Intercept = r_author_id + b_Intercept) %>%
  mutate(author_id = as.character(author_id))

# Pooled effect
hos.pooled.draws <- spread_draws(hos.bayes, b_Intercept) %>%
  mutate(author_id = "pooled effect")

# Combine individual study and pooled effect data frames
author_levels4<- c(as.character(1:4), "pooled effect")
hos.forest <- bind_rows(hos.draws, hos.pooled.draws) %>%
  ungroup() %>%
  mutate(author_id = factor(author_id, levels = author_levels4)) 

# Calculate summary statistics
hos.summ <- group_by(hos.forest, author_id) %>%
  mean_qi(b_Intercept)

# Exponentiate to get risk ratios
hos.forest <- hos.forest %>%
  mutate(b_Intercept = exp(b_Intercept))

hos.summ <- hos.summ %>%
  mutate(b_Intercept = exp(b_Intercept),
         .lower = exp(.lower),
         .upper = exp(.upper))

# Forest plot
ggplot(aes(x = b_Intercept, y = author_id), data = hos.forest) +

  geom_vline(xintercept = exp(fixef(hos.bayes)[1, 1]), 
             color = "red", size = 1) +
  geom_vline(xintercept = exp(fixef(hos.bayes)[1, 3:4]), 
             color = "red", linetype = 2) +
  geom_vline(xintercept = 1, color = "black", 
             size = 1) +

  geom_density_ridges(fill = "dodgerblue", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointintervalh(data = hos.summ, 
                      size = 1) +

  geom_text(data = mutate_if(hos.summ, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(x = "Risk Ratio", y = "Author ID") +
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(hos.summ$author_id)))

########################################################################
# 5.Death
death.bayes <- brm(data = death.es,
                   family = "gaussian",
                   yi | se(sei) ~ 1 + (1 | author_id),
                   prior = priors_weakly,
                   iter = 2000, warmup = 1000, cores = 4, chains = 4,
                   control = list(adapt_delta = 0.99),
                   seed = 18006513)

summary(death.bayes)
pp_check(death.bayes, ndraws = 100)

# Prepare Data for Forest Plot
# Spread draws for each study and compute b_Intercept
death.draws <- spread_draws(death.bayes, r_author_id[author_id,], b_Intercept) %>%
  mutate(b_Intercept = r_author_id + b_Intercept) %>%
  mutate(author_id = as.character(author_id))

# Pooled effect
death.pooled.draws <- spread_draws(death.bayes, b_Intercept) %>%
  mutate(author_id = "pooled effect")

# Combine individual study and pooled effect data frames
author_levels5 <- c(as.character(1:3), "pooled effect")
death.forest <- bind_rows(death.draws, death.pooled.draws) %>%
  ungroup() %>%
  mutate(author_id = factor(author_id, levels = author_levels5))

# Calculate summary statistics
death.summ <- group_by(death.forest, author_id) %>%
  mean_qi(b_Intercept)

# Exponentiate to get risk ratios
death.forest <- death.forest %>%
  mutate(b_Intercept = exp(b_Intercept))

death.summ <- death.summ %>%
  mutate(b_Intercept = exp(b_Intercept),
         .lower = exp(.lower),
         .upper = exp(.upper))

# Forest plot
ggplot(aes(x = b_Intercept, y = author_id), data = death.forest) +
  
  geom_vline(xintercept = exp(fixef(death.bayes)[1, 1]), 
             color = "red", size = 1) +
  geom_vline(xintercept = exp(fixef(death.bayes)[1, 3:4]), 
             color = "red", linetype = 2) +
  geom_vline(xintercept = 1, color = "black", 
             size = 1) +
  
  geom_density_ridges(fill = "dodgerblue", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointintervalh(data = death.summ, 
                      size = 1) +
  
  geom_text(data = mutate_if(death.summ, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(x = "Risk Ratio", y = "Author ID") +
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(death.summ$author_id))) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0.5, 2, by = 0.5))
