# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## MODEL OUTPUT ===============================================================

# Description:
#     Model outputs: extract effect sizes and Watanabe-Akaike information (WAIC)
#     to measure goodness of fit.

# Paper:
#     Detection of dengue virus in Aedes aegypti during an urban epidemic in Iquitos, Peru
#     (December 2010 to March 2011)

# Script author:
#     Anna B. Kawiecki        ORCID: 0000-0002-0499-2612

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# ---- 0. Load -----------------------------------------------------------------

# ---- 0.1 Read in R libraries ----

# Simplifies the use of relative file paths
library(here)

# Core packages for data manipulation and visualization
library(tidyverse)

# Functions for descriptive statistics and psychometrics
library(psych)

# Functions for statistical methods and distribution fitting
library(MASS)

# Integrated Nested Laplace Approximation for Bayesian inference
library(INLA)

# R interface to Stan for Bayesian modeling
library(rstan)

# Goodness-of-fit measures for Bayesian models
library(loo)

# Simple features for handling spatial vector data
library(sf)

# R interface to CmdStan, a backend for fitting Stan models
library(cmdstanr)

# Model check statistics and visualization for Bayesian models
library(bayesplot)

# ---- 0.2 Read in functions ----

# Load custom helper functions for model extraction and summarization

# Extract the point‑wise log‑likelihood matrix from a fitted model
fx.get.log.lik.matrix <- readRDS(here("analysis", "functions", "fx.get.log.lik.matrix.rds"))
# Compare WAIC and PSIS using loo package
fx.compare.loo <- readRDS(here("analysis", "functions", "fx.compare.loo.rds"))
# Extract fixed effects from model fitted with INLA
fx.fix.eff   <- readRDS(here("analysis", "functions", "fx.fix.eff.rds"))
# Extract fixed effects from model fitted with Stan
fx.stan.f.e  <- readRDS(here("analysis", "functions", "fx.stan.f.e.rds"))
# Inverse logit function 
fx_inverse_logit <- readRDS(here("analysis", "functions", "fx_inverse_logit.rds"))
# Compute fold-change and probability draws for one model
fx_fold_prob <- readRDS(here("analysis", "functions","fx_fold_prob.rds"))
# Generates PPD for a INLA Negative Binomial model.
fx.ppd.inla.nb <- readRDS(here("analysis",  "functions", "fx.ppd.inla.nb.rds"))
# Generates PPD for a INLA Binomial model.
fx.ppd.inla.binomial <- readRDS(here("analysis",  "functions", "fx.ppd.inla.binomial.rds"))
#  Generates PPD for a Stan Binomial model.
fx.ppd.stan.binomial <- readRDS(here("analysis",  "functions", "fx.ppd.stan.binomial.rds"))
# Bayesian Posterior Predictive Check (PPC) summary
fx.ppc.bayes.summary <- readRDS(here("analysis",  "functions", "fx.ppc.bayes.summary.rds"))


# ADD the ppc functions! 

# ---- 0.3 Read in data  ----

# Import household-level data
d.m <- readRDS(here("analysis", "data", "derived_data", "household_level_data",
                    "d.m.rds"))

# Import area-level data

m.area.week.lag <- readRDS(here("analysis", "data", "derived_data",
                                "area_level_data","m.area.week.lag.rds"))
m.area.week.lag.4 <- readRDS(here("analysis", "data", "derived_data",
                                  "area_level_data","m.area.week.lag.4.rds"))

h.area.week.lag <- readRDS(here("analysis", "data", "derived_data",
                                "area_level_data","h.area.week.lag.rds"))
h.area.week.lag.4 <- readRDS(here("analysis", "data", "derived_data",
                                  "area_level_data","h.area.week.lag.4.rds"))

# ---- 1. Ae. aegypti surveillance using a case-contact sampling strategy vs. broader surveillance -----

# ---- 1.1  Negative binomial models (Eq 1,2) ----
# Association between Ae. aegypti surveillance strategy and abundance ----

# Load fitted model objects
m.s.nb.base.0 <- readRDS(here("analysis", "outputs", "models", "m.s.nb.base.0.rds"))
m.s.nb.base.1 <- readRDS(here("analysis", "outputs", "models", "m.s.nb.base.1.rds"))
m.s.nb.1 <- readRDS(here("analysis", "outputs", "models", "m.s.nb.1.rds"))

# Combine the three models into a list for evaluation
m.s.nb.list <- list(m.s.nb.base.0, m.s.nb.base.1, m.s.nb.1)

# Load model stack for posterior predictive checks
stk.m.s.nb.1.e <- readRDS(here("analysis", "outputs", "models", "stk.m.s.nb.1.e.rds"))

# ---- 1.1.1 Compare goodness-of-fit metrics of negative binomial models ----

# ---- 1.1.1.1 Compare WAIC of negative binomial models ----

# Extract WAIC values from each model
m.s.nb.waic <- sapply(m.s.nb.list, function(f) f$waic$waic)

# Create simple model labels (e.g., m.1, m.2, m.3)
m <- sapply(seq(1, length(m.s.nb.list), by = 1),
            function(x) paste("m", x, sep = "."))

# Create a dataframe summarizing WAIC scores and model identifiers
m.s.nb.waic.df <- data.frame(
  waic  = m.s.nb.waic,
  model = c("b0", "b0+surv", "b0+surv+spde"),  # descriptive names
  m     = m
) %>%
  arrange(waic)  # sort models by best fit (lowest WAIC)

# Identify best model (lowest WAIC)
m.s.nb.waic.df$model[[1]]
# [1] "b0+surv+spde" — the model with surveillance covariates + spatial effect

# Save model comparison output
saveRDS(m.s.nb.waic.df, here("analysis", "outputs", "models",
                             "m.s.nb.waic.df.rds"))

# ---- 1.1.1.2 Compare CPO of negative binomial models ----
# Recompute CPO for any models with unreliable values flagged by INLA
# (only recomputes observations where cpo$failure > 0; leaves the rest as is)
m.s.nb.cpo <- lapply(m.s.nb.list, function(f) {
  if (sum(f$cpo$failure, na.rm = TRUE) > 0) {
    inla.cpo(f)
  } else {
    f
  }
})

# Extract LCPO (-sum(log(cpo))) values from each model
m.s.nb.lcpo <- sapply(m.s.nb.cpo, function(f) -sum(log(f$cpo$cpo), na.rm = TRUE))

# Create simple model labels (e.g., m.1, m.2, m.3)
m <- sapply(seq(1, length(m.s.nb.list), by = 1),
            function(x) paste("m", x, sep = "."))

# Create a dataframe summarizing LCPO scores and model identifiers
m.s.nb.lcpo.df <- data.frame(
  lcpo  = m.s.nb.lcpo,
  model = c("b0", "b0+surv", "b0+surv+spde"),  # descriptive names
  m     = m
) %>%
  arrange(lcpo)  # sort models by best fit (lowest LCPO)

# Identify best model (lowest LCPO)
m.s.nb.lcpo.df$model[[1]]
# [1] "b0+surv+spde" — the model with surveillance covariates + spatial effect

# Save model comparison output
saveRDS(m.s.nb.lcpo.df, here("analysis", "outputs", "models",
                             "m.s.nb.lcpo.df.rds"))

# ---- 1.1.1.3 Posterior predictive check of negative binomial models ----

# 1. Define the observed data for the Negative Binomial model 
# (e.g., count of female mosquitoes)
m.s.nb.1.yobs = d.m$aa_female

# 2. Generate Posterior Predictive Distributions (PPD)
# Uses fx.ppd.inla.nb to sample 1000 datasets from the posterior 
# distributions of the latent effects and the 'size' hyperparameter.
m.s.nb.1.ypred <- fx.ppd.inla.nb(
  inla_object = m.s.nb.1,           # The fitted INLA model object
  stack = stk.m.s.nb.1.e,           # The data stack used for the model
  tag = "est",                      # Only extract predictions for the estimation set
  nsamples = 1000                   # Number of simulated datasets to generate
)

# 3. Calculate Bayesian PPC Summary
# Compares the observed data (y_obs) against the 1000 simulated datasets (y_pred).
# This will return a table of statistics (mean, zero_pct, max, var) and 
# the Bayesian p-value for each.
ppc_nb <- fx.ppc.bayes.summary(
  y_obs = m.s.nb.1.yobs,
  y_pred = m.s.nb.1.ypred,
  model_label = "Ae.ae abundance ~ surveillance strategy (Eq. 1,2)"
)

saveRDS(ppc_nb, here("analysis", "outputs", "models","ppc_nb.rds"))

# ---- 1.1.2 Evaluate fixed effects of negative binomial models ----

# Extract fixed effects from each model using the custom function
m.s.nb.fe.df <- fx.fix.eff(m.s.nb.list) %>%
  left_join(m.s.nb.waic.df, by = "m") %>%     # join with model metadata
  mutate(variable = case_when(
    variable == "(Intercept)" ~ "b0",         # rename intercept for clarity
    T ~ variable
  ))

# Subset fixed effects for the best model
m.s.nb.fe.df %>%
  filter(model == "b0+surv+spde") %>%  # filter best model by name
  dplyr::select("variable", "mean", "q0.025",  "q0.975", "waic", "model")

#            variable mean  q0.025  q0.975    waic         model
#                  b0  0.39   0.31     0.49  11792.64  b0+surv+spde
# pos.case.contact.f1  1.76   1.21     2.56  11792.64  b0+surv+spde

# Save the full fixed effects summary for all models
saveRDS(m.s.nb.fe.df, here("analysis", "outputs", "models", "m.s.nb.fe.df.rds"))

# ---- 1.2  Logistic models (Eq 3,4 and 3,5): ----
# Association between the surveillance strategy and probability of DENV detection ----

# Load fitted model objects
m.s.b.base.0 <- readRDS(here("analysis", "outputs", "models", "m.s.b.base.0.rds"))
m.s.b.base.1 <- readRDS(here("analysis", "outputs", "models", "m.s.b.base.1.rds"))
m.s.b.1 <- readRDS(here("analysis", "outputs", "models", "m.s.b.1.rds"))
m.s.b.2 <- readRDS(here("analysis", "outputs", "models", "m.s.b.2.rds"))

# Combine the three models into a list for evaluation
m.s.b.list <- list(m.s.b.base.0, m.s.b.base.1, m.s.b.1)

# Load model stack for posterior predictive checks
stk.m.s.b.1.e <- readRDS(here("analysis", "outputs", "models", "stk.m.s.b.1.e.rds"))
stk.m.s.b.2.e <- readRDS(here("analysis", "outputs", "models", "stk.m.s.b.2.e.rds"))

# ---- 1.2.1 Compare goodness-of-fit metrics of logistic models ----

# ---- 1.2.1.1 Compare WAIC of logistic models ----

# Extract WAIC
m.s.b.waic <- sapply(m.s.b.list, function(f) f$waic$waic)

# Create a dataframe summarizing WAIC scores and model identifiers
m <- sapply(seq(1,length(m.s.b.list), by=1), function(x) paste("m",x,sep="."))

# Summarise WAIC and model identifiers
m.s.b.waic.df <- data.frame(waic= m.s.b.waic,
                            model= c("b0", "b0+surv","b0+surv+spde"),
                            m= m,
                            distribution= "bernouilli") %>%
  arrange(waic)

# Identify best model (lowest WAIC)
m.s.b.waic.df$model[[1]]
# [1] "b0+surv+spde" — the model with surveillance covariates + spatial effect

# Save model comparison output
saveRDS(m.s.b.waic.df, here("analysis", "outputs", "models", "m.s.b.waic.df.rds"))

# ---- 1.2.1.2 Compare CPO of logistic models ----
# Recompute CPO for any models with unreliable values flagged by INLA
# (only recomputes observations where cpo$failure > 0; leaves the rest as is)
m.s.b.cpo <- lapply(m.s.b.list, function(f) {
  if (sum(f$cpo$failure, na.rm = TRUE) > 0) {
    inla.cpo(f)
  } else {
    f
  }
})

# Extract LCPO (-sum(log(cpo))) values from each model
m.s.b.lcpo <- sapply(m.s.b.cpo, function(f) -sum(log(f$cpo$cpo), na.rm = TRUE))

# Create simple model labels (e.g., m.1, m.2, m.3)
m <- sapply(seq(1, length(m.s.b.list), by = 1),
            function(x) paste("m", x, sep = "."))

# Create a dataframe summarizing LCPO scores and model identifiers
m.s.b.lcpo.df <- data.frame(
  lcpo  = m.s.b.lcpo,
  model = c("b0", "b0+surv", "b0+surv+spde"),  # descriptive names
  m     = m
) %>%
  arrange(lcpo)  # sort models by best fit (lowest LCPO)

# Identify best model (lowest LCPO)
m.s.b.lcpo.df$model[[1]]
# [1] "b0+surv+spde" — the model with surveillance covariates + spatial effect

# Save model comparison output
saveRDS(m.s.b.lcpo.df, here("analysis", "outputs", "models",
                             "m.s.b.lcpo.df.rds"))

# ---- 1.2.1.3 Posterior predictive check of logistic models ----

# ---- 1.2.1.3.1 "b0+surv+spde" ----

# Define the observed binary data (e.g., presence/absence of DENV in household)
m.s.b.1.yobs = d.m$denv.house.num

# Index Verification
# Extract indices from the INLA stack to ensure the number of predictions 
# matches the number of observed data points.
est_idx_check <- INLA::inla.stack.index(stk.m.s.b.1.e, tag = "est")$data
length(est_idx_check)  # Sanity check: must match length(m.s.b.1.yobs)

# Generate Posterior Predictive Distributions (PPD)
m.s.b.1.ypred <- fx.ppd.inla.binomial(
  inla_object = m.s.b.1, 
  stack = stk.m.s.b.1.e,
  n_trials = rep(1, length(est_idx_check)), # Setting trials=1 for Bernoulli outcome
  tag = "est",
  nsamples = 1000
)

# Data Cleaning (NA Handling)
# remove rows where the observed data is NA before summarizing.
keep_idx <- which(!is.na(m.s.b.1.yobs)) # Identify indices of non-missing values
length(keep_idx)  # Confirm total valid observations (e.g., 1245)

# Subset both the observed vector and the prediction matrix to keep only valid cases
m.s.b.1.yobs_fit  <- m.s.b.1.yobs[keep_idx]
m.s.b.1.ypred_fit <- m.s.b.1.ypred[keep_idx, ]

# Calculate Bayesian PPC Summary
# Compare the cleaned observed data against the cleaned simulated data.
ppc_b1 <- fx.ppc.bayes.summary(
  y_obs = m.s.b.1.yobs_fit,
  y_pred = m.s.b.1.ypred_fit,
  model_label = "Any DENV+ Ae.ae ~ surveillance strategy (Eq. 3,4)"
)

saveRDS(ppc_b1, here("analysis", "outputs", "models","ppc_b1.rds"))

# ---- 1.2.1.3.2 "b0+surv+spde+aa_female" ----

# Define the observed binary data (e.g., presence/absence of DENV in household)
m.s.b.2.yobs = d.m$denv.house.num

# Extract indices from the INLA stack to ensure the number of predictions 
est_idx_check <- INLA::inla.stack.index(stk.m.s.b.2.e, tag = "est")$data
length(est_idx_check)  # confirm this matches length(m.s.b.1.yobs)

# Generate Posterior Predictive Distributions (PPD)
m.s.b.2.ypred <- fx.ppd.inla.binomial(
  m.s.b.2, 
  stack = stk.m.s.b.2.e,
  n_trials = rep(1, length(est_idx_check)),
  tag = "est", 
  nsamples = 1000
)

# Handle NAs
keep_idx <- which(!is.na(m.s.b.1.yobs))
length(keep_idx)  # 1245

# Subset both the observed vector and the prediction matrix to keep only valid cases
m.s.b.2.yobs_fit  <- m.s.b.2.yobs[keep_idx]
m.s.b.2.ypred_fit <- m.s.b.2.ypred[keep_idx, ]

# Calculate Bayesian PPC Summary
ppc_b2 <- fx.ppc.bayes.summary(
  y_obs = m.s.b.2.yobs_fit, 
  y_pred = m.s.b.2.ypred_fit, 
  model_label = "Any DENV+ Ae.ae ~ surveillance strategy + Ae.ae abundance (Eq. 3,5)"
)

saveRDS(ppc_b2, here("analysis", "outputs", "models","ppc_b2.rds"))

# ---- 1.2.2 Evaluate fixed effects of logistic models ----

# ---- 1.2.2.1 Evaluate fixed effects of logistic models no female counts----

# Subset fixed effects for the best model
m.s.b.fe.df <- fx.fix.eff(m.s.b.list) %>%
  left_join(m.s.b.waic.df, by="m") %>%
  mutate(variable= case_when(
    variable=="(Intercept)" ~ "b0",
    T~ variable
  ))

# Subset fixed effects for the best model
m.s.b.fe.df %>%
  filter(model == "b0+surv+spde") %>%  # filter best model by name
  dplyr::select("variable", "mean", "q0.025",  "q0.975", "waic", "model")

#            variable mean  q0.025  q0.975    waic         model
#                  b0 0.03   0.02   0.06 540.2786 b0+surv+spde
# pos.case.contact.f1 1.08   0.36   3.23 540.2786 b0+surv+spde

# Save the full fixed effects summary for all models
saveRDS(m.s.b.fe.df, here("analysis", "outputs", "models",  "m.s.b.fe.df.rds"))

# variable              mean  sd   q0.025 q0.5 q0.975 mode kld  fixed     
# 1                  b0 0.03 1.46   0.01 0.03   0.05 0.03   1 b0+pos.case.contact.f1+aa_female.i 
# 2 pos.case.contact.f1 0.86 1.85   0.26 0.86   2.87 0.86   1 b0+pos.case.contact.f1+aa_female.i 
# 3         aa_female.i 1.08 1.03   1.03 1.08   1.14 1.08   1 b0+pos.case.contact.f1+aa_female.i 

# ---- 1.2.2.1 Evaluate fixed effects of logistic models with female counts ----

m.s.b.2.fe.df <- fx.fix.eff(list(m.s.b.2)) 

saveRDS(m.s.b.2.fe.df, here("analysis", "outputs", "models",  "m.s.b.2.fe.df.rds"))

# ---- 2. Association between Ae. aegypti abundance and probability of DENV detection ----

# ---- 2.1 Household-level logistic model (Eq. 3,6)----

# Load fitted model objects
m.b.base.0 <- readRDS( here("analysis", "outputs", "models", "m.b.base.0.rds"))
m.b.base.1 <- readRDS( here("analysis", "outputs", "models", "m.b.base.1.rds"))
m.b.0 <- readRDS( here("analysis", "outputs", "models", "m.b.0.rds"))
m.b.1 <- readRDS( here("analysis", "outputs", "models", "m.b.1.rds"))
m.b.2 <- readRDS( here("analysis", "outputs", "models", "m.b.2.rds"))

# Combine the five models into a list for evaluation
m.b.list <- list(m.b.base.0, m.b.base.1, m.b.0, m.b.1, m.b.2 )


stk.m.b.2.e <- readRDS(here("analysis", "outputs", "models", "stk.m.b.2.e.rds"))


# ---- 2.1.1 Compare goodness of fit metrics of household-level logistic models ----

# ---- 2.1.1.1 Compare WAIC of household-level logistic models----

# Extract WAIC values from each model
m.b.waic <- sapply(m.b.list, function(f) f$waic$waic)

# Create simple model labels
m <- sapply(seq(1,length(m.b.list), by=1), function(x) paste("m",x,sep="."))

# Create a dataframe summarizing WAIC scores and model identifiers
m.b.waic.df <- data.frame(waic= m.b.waic,
                          model= c("b0", "b0+aa_female","b0+spde",
                                   "b0+aa_female+spde",
                                   "b0+aa_female+f(month.ar1)+spde"),
                          m= m) %>%
  arrange(waic)  # sort models by best fit (lowest WAIC)

# Identify best model (lowest WAIC)
m.b.waic.df$model[[1]]

#[1] "b0+aa_female+f(month.ar1)+spde"
# The best-fitting model was the logistic regression with spatial and AR1 monthly
# random effects: abundance + month as AR1 effect + spatial field

# Save model comparison output
saveRDS(m.b.waic.df, here("analysis", "outputs", "models", "m.b.waic.df.rds"))

# ---- 2.1.1.2 Compare CPO of household-level logistic models ----
# Recompute CPO/PIT for any models with unreliable values flagged by INLA
# (only recomputes observations where cpo$failure > 0; leaves the rest as is)
m.b.cpo <- lapply(m.b.list , function(f) {
  if (sum(f$cpo$failure, na.rm = TRUE) > 0) {
    inla.cpo(f)
  } else {
    f
  }
})

# Extract LCPO (-sum(log(cpo))) values from each model
m.b.lcpo <- sapply(m.b.cpo, function(f) -sum(log(f$cpo$cpo), na.rm = TRUE))

# Create simple model labels (e.g., m.1, m.2, m.3)
m <- sapply(seq(1, length(m.b.list), by = 1),
            function(x) paste("m", x, sep = "."))

# Create a dataframe summarizing LCPO scores and model identifiers
m.b.lcpo.df <- data.frame(
  lcpo  = m.b.lcpo,
  model= c("b0", "b0+aa_female","b0+spde",
           "b0+aa_female+spde",
           "b0+aa_female+f(month.ar1)+spde"),  # descriptive names
  m     = m
) %>%
  arrange(lcpo)  # sort models by best fit (lowest LCPO)

# Identify best model (lowest LCPO)
m.b.lcpo.df$model[[1]]
#[1] "b0+aa_female+f(month.ar1)+spde"
# The best-fitting model was the logistic regression with spatial and AR1 monthly
# random effects: abundance + month as AR1 effect + spatial field

# Save model comparison output
saveRDS(m.b.lcpo.df, here("analysis", "outputs", "models",
                             "m.b.lcpo.df.rds"))

# ---- 2.1.1.3 Posterior predictive checks  ----

# Define the observed binary data (e.g., presence/absence of DENV in household)
m.b.2.yobs = d.m$denv.house.num

# Extract indices from the INLA stack to ensure the number of predictions 
# matches the number of observed data points.
est_idx_check <- INLA::inla.stack.index(stk.m.b.2.e, tag = "est")$data
length(est_idx_check)  # confirm this matches length(m.s.b.1.yobs)

# Generate Posterior Predictive Distributions (PPD)
m.b.2.ypred <- fx.ppd.inla.binomial(
  m.b.2, 
  stack = stk.m.b.2.e,
  n_trials = rep(1, length(est_idx_check)),
  tag = "est", 
  nsamples = 1000
)

# Subset both y_obs and y_pred to only the non-NA (i.e., actually tested) households
keep_idx <- which(!is.na(m.s.b.1.yobs))
length(keep_idx)  # 1245

m.b.2.yobs_fit  <- m.b.2.yobs[keep_idx]
m.b.2.ypred_fit <- m.b.2.ypred[keep_idx, ]

# Calculate Bayesian PPC Summary
ppc_m.b.2 <- fx.ppc.bayes.summary(
  y_obs = m.b.2.yobs_fit, 
  y_pred = m.b.2.ypred_fit, 
  model_label = "Any DENV+ Ae.ae ~ Ae.ae abundance (Eq. 3,6)"
)

saveRDS(ppc_m.b.2, here("analysis", "outputs", "models","ppc_m.b.2.rds"))

# ---- 2.1.2 Evaluate fixed effects of household-level logistic models ----

# Extract fixed effects from each model using the custom function
m.b.fe.df <- fx.fix.eff(m.b.list) %>%
  left_join(m.b.waic.df, by="m") %>%
  mutate(variable= case_when(
    variable=="(Intercept)" ~ "b0",
    T~ variable
  ))

# Subset fixed effects for the best model
m.b.fe.df %>%
  filter(model == "b0+aa_female+f(month.ar1)+spde") %>%  # filter best model by name
  dplyr::select("variable", "mean", "q0.025",  "q0.975", "waic", "model")

#  variable mean q0.025 q0.975     waic                          model
#        b0 0.02   0.00   0.06 472.4464 b0+aa_female+f(month.ar1)+spde
# aa_female 1.10   1.04   1.16 472.4464 b0+aa_female+f(month.ar1)+spde

# Save the full fixed effects summary for all models
saveRDS(m.b.fe.df, here("analysis", "outputs", "models", "m.b.fe.df.rds"))

# ---- 2.1.3 Evaluate range of the household-level logistic model ----

# Create a list of fitted INLA models with spatial random effects
m.b.range.list <- list(m.b.0, m.b.1, m.b.2)

# Extract the posterior *mean* of the spatial range parameter ("Range for s")
summary(m.b.2)$hyperpar["Range for s", "mean"]
# 95.288


# Extract the posterior *mode* of the spatial range parameter ("Range for s")
summary(m.b.2)$hyperpar["Range for s", "mode"]
# 19.536

# ---- 2.2 Area-level logistic models (Eq. 7,8 and 7,9)----

# Load model lists
# - fit.m.01: model with Dirichlet-weighted lag structure
# - fit.m.2.x: models with individual lags from week 0 to week 6

# Load list of fitted models for lag effect comparison
m.lag.list <- readRDS(here("analysis", "outputs", "models",
                           "m.lag.list.rds"))

# Load model index data frame
m.lag.list.index <- readRDS(here("analysis", "outputs", "models",
                                 "m.lag.list.index.rds"))

# Load model with Dirichlet-weighted lag structure
# 1st element of the model list
fit.m.01 <- m.lag.list[[1]]

# ---- 2.2.1 Check traceplots of Dirichlet-weighted lag model ----

# Plot MCMC trace plots to visually inspect convergence for fit.m.01
# Note: Alternative diagnostic available from rethinking::trankplot()
rstan::traceplot(fit.m.01)


# ---- 2.2.2 Posterior predictive checks ----

# Generate Posterior Predictive Distributions (PPD)
m.01.ypred <- fx.ppd.stan.binomial(fit.m.01, n_trials = m.area.week.lag.4$n.tested, nsamples = 1000)

# Calculate Bayesian PPC Summary
ppc_m01 <- fx.ppc.bayes.summary(m.area.week.lag.4$n.denv, # Observed outcome
                                m.01.ypred,               # Predicted outcome from ppd
                                "Ae. ae DENV prevalence ~ Ae. ae abundance (Eq. 7,9)")

saveRDS(ppc_m01, here("analysis", "outputs", "models","ppc_m01.rds"))

# ---- 2.2.3 Evaluate fixed effects of the area-level logistic models ----

# Extract fixed effects (posterior summaries) from all models
m.lag.fe <- fx.stan.f.e(m.lag.list)

# Combine model estimates with metadata and prepare for visualization and analysis
m.lag.fe.df <- m.lag.fe %>%
  # Merge with model index
  left_join(m.lag.list.index) %>%
  # Exclude intercept and error scale
  filter(parameter != "sigma") %>%
  # Annotate each parameter with lag information
  mutate(lag = case_when(
    str_detect(parameter, "\\[1\\]$") == TRUE ~ "same week",
    str_detect(parameter, "\\[2\\]$") == TRUE ~ "1 week lag",
    str_detect(parameter, "\\[3\\]$") == TRUE ~ "2 week lag",
    str_detect(parameter, "\\[4\\]$") == TRUE ~ "3 week lag",
    str_detect(parameter, "\\[5\\]$") == TRUE ~ "4 week lag",
    str_detect(model.name, "m.0") == TRUE & parameter == "beta" ~ "total",
    str_detect(model.name, "m.0") == FALSE & parameter == "beta" &
      grepl("\\.0$", model.name) == TRUE ~ "same week",
    str_detect(model.name, "m.0") == FALSE & parameter == "beta" &
      grepl("\\.1$", model.name) == TRUE ~ "1 week lag",
    str_detect(model.name, "m.0") == FALSE & parameter == "beta" &
      grepl("\\.2$", model.name) == TRUE ~ "2 week lag",
    str_detect(model.name, "m.0") == FALSE & parameter == "beta" &
      grepl("\\.3$", model.name) == TRUE ~ "3 week lag",
    str_detect(model.name, "m.0") == FALSE & parameter == "beta" &
      grepl("\\.4$", model.name) == TRUE ~ "4 week lag",
    str_detect(model.name, "m.0") == FALSE & parameter == "beta" &
      grepl("\\.5$", model.name) == TRUE ~ "5 week lag",
    str_detect(model.name, "m.0") == FALSE & parameter == "beta" &
      grepl("\\.6$", model.name) == TRUE ~ "6 week lag",
    str_detect(parameter, "beta") == TRUE ~ "total",
    TRUE ~ NA
  )) %>%
  # Label dataset source
  mutate(dataset = case_when(
    grepl("^m\\.2", model.name) == TRUE ~ "complete for each lag",
    TRUE ~ "complete 4 week lags"
  )) %>%
  # Label model structure
  mutate(model.structure = case_when(
    grepl("^m\\.0", model.name) == TRUE ~ "combined effect of weighted week lags",
    TRUE ~ "single lagged week effect "
  )) %>%
  # Set categorical factor order for plotting
  mutate(lag = fct_relevel(
    factor(lag, levels = c("total", "same week", "1 week lag", "2 week lag",
                           "3 week lag", "4 week lag", "5 week lag", "6 week lag"))
  ))

# Save final lag effect summary data frame
saveRDS(m.lag.fe.df, here("analysis", "outputs", "models",
                          "m.lag.fe.df.rds"))


# ---- 3. Association between entomological surveillance metrics and human dengue incidence (Eq. 10, 9 and 10,8) ----

# ---- 3.1 Dirichlet weighted models with complete 4-week lag dataset (Eq. 10, 9) ----
h.area.week.lag.4 <- readRDS(here("analysis", "data", "derived_data",
                                  "area_level_data","h.area.week.lag.4.rds"))

# Read in model lists
h.0.lag.list <- readRDS(here("analysis", "outputs", "models",
                             "h.0.lag.list.rds"))
# Read in model names
h.0.lag.list.index <- readRDS(here("analysis", "outputs", "models",
                                   "h.0.lag.list.index.rds"))

#Extract individual models for comparison
fit.h.01 <- h.0.lag.list[[1]]
fit.h.02 <- h.0.lag.list[[2]]
fit.h.03 <- h.0.lag.list[[3]]

model_names <- c("Vector DENV prevalence model",
                 "Vector index model",
                 "Average vector abundance model")

# ---- 3.1.1 Compare by WAIC and PSIS-LOO ----

h.0.waic.loo <- fx.compare.loo(list(fit.h.01, fit.h.02, fit.h.03), model_names, method = "waic")
h.0.psis.loo <- fx.compare.loo(list(fit.h.01, fit.h.02, fit.h.03), model_names, method = "psis")


saveRDS(h.0.waic.loo, here("analysis", "outputs", "models","h.0.waic.loo.rds"))
saveRDS(h.0.psis.loo, here("analysis", "outputs", "models","h.0.psis.loo.rds"))


# ---- 3.1.2 Check PSIS-LOO diagnostics ----
fit_list <- list(fit.h.01, fit.h.02, fit.h.03)

loo_list <- lapply(fit_list, function(fit) {
  log_lik <- fx.get.log.lik.matrix(fit)
  loo::loo(log_lik)
})

names(loo_list) <- model_names

lapply(names(loo_list), function(nm) {
  k <- pareto_k_values(loo_list[[nm]])
  list(model = nm,
       n_flagged = sum(k > 0.7),
       flagged_obs = which(k > 0.7),
       max_k = max(k))
})

# [[1]]
# [[1]]$model
# [1] "Vector DENV prevalence model"
# 
# [[1]]$n_flagged
# [1] 2
# 
# [[1]]$flagged_obs
# [1]  2 18
# 
# [[1]]$max_k
# [1] 0.7641723
# 
# 
# [[2]]
# [[2]]$model
# [1] "Vector index model"
# 
# [[2]]$n_flagged
# [1] 0
# 
# [[2]]$flagged_obs
# integer(0)
# 
# [[2]]$max_k
# [1] 0.5833503
# 
# 
# [[3]]
# [[3]]$model
# [1] "Average vector abundance model"
# 
# [[3]]$n_flagged
# [1] 1
# 
# [[3]]$flagged_obs
# [1] 2
# 
# [[3]]$max_k
# [1] 0.7792133

# ---- 3.1.3 Posterior predictive checks ----

# Generate Posterior Predictive Distributions (PPD)
h.01.ypred <- fx.ppd.stan.binomial(fit.h.01, n_trials = h.area.week.lag.4$n.ind,
                                   nsamples = 1000)
h.02.ypred <- fx.ppd.stan.binomial(fit.h.02, n_trials = h.area.week.lag.4$n.ind,
                                   nsamples = 1000)
h.03.ypred <- fx.ppd.stan.binomial(fit.h.03, n_trials = h.area.week.lag.4$n.ind,
                                   nsamples = 1000)

# Calculate Bayesian PPC Summary
ppc_h01 <- fx.ppc.bayes.summary(h.area.week.lag.4$n.denv, # Observed outcome
                                h.01.ypred,               # Predicted outcome from ppd
                                "Human DENV inc ~ Ae. ae DENV prevalence (Eq. 10, 9)")
ppc_h02 <- fx.ppc.bayes.summary(h.area.week.lag.4$n.denv, # Observed outcome
                                h.02.ypred,               # Predicted outcome from ppd
                                "Human DENV inc ~ Ae. ae abundance (Eq. 10, 9)")
ppc_h03 <- fx.ppc.bayes.summary(h.area.week.lag.4$n.denv, # Observed outcome
                                h.03.ypred,               # Predicted outcome from ppd
                                "Human DENV inc ~ vector index (Eq. 10, 9)")

saveRDS(ppc_h01, here("analysis", "outputs", "models","ppc_h01.rds"))
saveRDS(ppc_h02, here("analysis", "outputs", "models","ppc_h02.rds"))
saveRDS(ppc_h03, here("analysis", "outputs", "models","ppc_h03.rds"))

# ---- 3.1.4 Evaluate OR of entomological metrics accross variable ranges ----

h.area <- readRDS(here("analysis", "data", "derived_data", "area_level_data",
                       "h.area.rds"))

range(h.area$prevalence.p.100, na.rm = TRUE) # 0 23.33

range(h.area$vi, na.rm = TRUE) # 0 44.02

range(h.area$avg.aa.f, na.rm = TRUE) # 0 5.11


# Observed ranges for each predictor (min, max)
ranges <- tibble(
  index      = 1:3,
  metric     = c("Ae. aegypti  DENV prevalence", "Ae. aegypti  female abundance",
                 "Vector index"),
  range_min  = c(0, 0, 0),
  range_max  = c(23.33, 5.11, 44.02)
)

h.lag.results <- lapply(ranges$index, function(i) {
  fx_fold_prob(
    model     = h.lag.list[[i]],
    range_min = ranges$range_min[ranges$index == i],
    range_max = ranges$range_max[ranges$index == i],
    label     = ranges$metric[ranges$index == i]
  )
}) %>%
  bind_rows() %>%
  dplyr::select(metric, range_min, range_max,
                fold_median, fold_low, fold_high,
                p_at_min_median, p_at_min_low, p_at_min_high,
                p_at_max_median, p_at_max_low, p_at_max_high)

print(h.lag.results)

saveRDS(h.lag.results, here("analysis", "outputs", "models", "h.lag.results.rds"))



# ---- 3.2 Effect of all entomological metric models (Eq. 10, 9 and 10,8) ----

# Models measuring the independent effect of each weekly-lagged measure
# and the Dirichlet-weighted lags of the entomological metrics

# Read in model lists for all models
h.lag.list <- readRDS(here("analysis", "outputs", "models", "h.lag.list.rds"))

# Read in model names
h.lag.list.index <- readRDS(here("analysis", "outputs", "models", "h.lag.list.index.rds"))


# ---- 3.2.1 Evaluate fixed effects of entomological metrics ----

# Extract fixed effects
h.lag.fe <- fx.stan.f.e(h.lag.list)

# Create dataframe with named fixed effects and estimates
h.lag.fe.df <- h.lag.fe %>%
  left_join(h.lag.list.index) %>%
  #filter(parameter != "alpha" & parameter != "sigma") %>%
  mutate(lag= case_when(
    str_detect(parameter, "\\[1\\]$") ==TRUE~ "same week",
    str_detect(parameter, "\\[2\\]$") ==TRUE~ "1 week lag",
    str_detect(parameter, "\\[3\\]$") ==TRUE ~ "2 week lag",
    str_detect(parameter, "\\[4\\]$") ==TRUE ~ "3 week lag",
    str_detect(parameter, "\\[5\\]$") ==TRUE ~ "4 week lag",
    str_detect(model.name, "h.0") ==TRUE & parameter == "beta" ~ "total",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.0$", model.name) ==TRUE~ "same week",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.1$", model.name) ==TRUE~ "1 week lag",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.2$", model.name) ==TRUE~ "2 week lag",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.3$", model.name) ==TRUE~ "3 week lag",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.4$", model.name) ==TRUE~ "4 week lag",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.5$", model.name) ==TRUE~ "5 week lag",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.6$", model.name) ==TRUE~ "6 week lag",
    str_detect(parameter, "beta") ==TRUE ~ "total",
    T~ NA
  )) %>%
  mutate(dataset= case_when(
    grepl("^h\\.2", model.name) ==TRUE ~ "complete for each lag",
    T ~ "complete 4 week lags"
  )) %>%
  mutate(variable= case_when(
    grepl("\\d+\\.1\\.\\d+", model.name) ==TRUE |
      grepl("^h.01", model.name) ==TRUE  ~ "Ae. aegypti \nDENV prevalence",
    grepl("\\d+\\.2\\.\\d+", model.name) ==TRUE |
      grepl("^h.02", model.name) ==TRUE  ~ "Ae. aegypti \nfemale abundance",
    grepl("\\d+\\.3\\.\\d+", model.name) ==TRUE |
      grepl("^h.03", model.name) ==TRUE  ~ "Vector index"
  )) %>%
  mutate(variable=factor(variable,
                         levels= c("Ae. aegypti \nDENV prevalence",
                                   "Vector index",
                                   "Ae. aegypti \nfemale abundance"))) %>%
  mutate(model.structure= case_when(
    grepl("^h\\.0", model.name) ==TRUE ~ "combined effect of weighted week lags",
    T ~ "single lagged week effect "
  )) %>%
  mutate(lag= fct_relevel(factor(lag, levels = c("total",
                                                 "same week",
                                                 "1 week lag",
                                                 "2 week lag",
                                                 "3 week lag",
                                                 "4 week lag",
                                                 "5 week lag",
                                                 "6 week lag"))) )

# Save fixed effect dataframe
saveRDS(h.lag.fe.df, here("analysis", "outputs", "models", "h.lag.fe.df.rds"))

# ---- 4. Sensitivity analysis ----

fit.h.01.sen <- readRDS(here("analysis", "outputs", "models", "fit.h.01.sen.rds"))
fit.h.02.sen <- readRDS(here("analysis", "outputs", "models", "fit.h.02.sen.rds"))
fit.h.03.sen <- readRDS(here("analysis", "outputs", "models", "fit.h.03.sen.rds"))

h.0.sen.lag.list <- list(fit.h.01.sen, fit.h.02.sen, fit.h.03.sen)

## Assign name to all models
h.sen.lag.list.index <- data.frame(model.name= c("h.01", "h.02","h.03"),
                                   index= seq(1,length(h.0.sen.lag.list ), by=1))

# ---- 4.1  Compare by WAIC and PSIS ----

h.0.sen.waic.loo <- fx.compare.loo(h.0.sen.lag.list, model_names, method = "waic")
h.0.sen.psis.loo <- fx.compare.loo(h.0.sen.lag.list, model_names, method = "psis")

saveRDS(h.0.sen.waic.loo, here("analysis", "outputs", "models","h.0.sen.waic.loo.rds"))
saveRDS(h.0.sen.psis.loo, here("analysis", "outputs", "models","h.0.sen.psis.loo.rds"))

# ---- 4.2  Fixed effects analysis comparison ----
h.sen.lag.fe <- fx.stan.f.e.cmdstanr(h.0.sen.lag.list)


# Create dataframe with named fixed effects and estimates
h.sen.lag.fe.df <- h.sen.lag.fe %>%
  left_join(h.sen.lag.list.index) %>%
  filter(parameter != "alpha" & parameter != "sigma") %>%
  mutate(lag= case_when(
    str_detect(parameter, "\\[1\\]$") ==TRUE~ "same week",
    str_detect(parameter, "\\[2\\]$") ==TRUE~ "1 week lag",
    str_detect(parameter, "\\[3\\]$") ==TRUE ~ "2 week lag",
    str_detect(parameter, "\\[4\\]$") ==TRUE ~ "3 week lag",
    str_detect(parameter, "\\[5\\]$") ==TRUE ~ "4 week lag",
    str_detect(model.name, "h.0") ==TRUE & parameter == "beta" ~ "total",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.0$", model.name) ==TRUE~ "same week",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.1$", model.name) ==TRUE~ "1 week lag",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.2$", model.name) ==TRUE~ "2 week lag",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.3$", model.name) ==TRUE~ "3 week lag",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.4$", model.name) ==TRUE~ "4 week lag",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.5$", model.name) ==TRUE~ "5 week lag",
    str_detect(model.name, "h.0") ==FALSE &
      parameter == "beta" & grepl("\\.6$", model.name) ==TRUE~ "6 week lag",
    str_detect(parameter, "beta") ==TRUE ~ "total",
    T~ NA
  )) %>%
  mutate(dataset= case_when(
    grepl("^h\\.2", model.name) ==TRUE ~ "complete for each lag",
    T ~ "complete 4 week lags"
  )) %>%
  mutate(variable= case_when(
    grepl("\\d+\\.1\\.\\d+", model.name) ==TRUE |
      grepl("^h.01", model.name) ==TRUE  ~ "Ae. aegypti \nDENV prevalence",
    grepl("\\d+\\.2\\.\\d+", model.name) ==TRUE |
      grepl("^h.02", model.name) ==TRUE  ~ "Ae. aegypti \nfemale abundance",
    grepl("\\d+\\.3\\.\\d+", model.name) ==TRUE |
      grepl("^h.03", model.name) ==TRUE  ~ "Vector index"
  )) %>%
  mutate(variable=factor(variable,
                         levels= c("Ae. aegypti \nDENV prevalence",
                                   "Vector index",
                                   "Ae. aegypti \nfemale abundance"))) %>%
  mutate(model.structure= case_when(
    grepl("^h\\.0", model.name) ==TRUE ~ "combined effect of weighted week lags",
    T ~ "single lagged week effect "
  )) %>%
  mutate(lag= fct_relevel(factor(lag, levels = c("total",
                                                 "same week",
                                                 "1 week lag",
                                                 "2 week lag",
                                                 "3 week lag",
                                                 "4 week lag",
                                                 "5 week lag",
                                                 "6 week lag"))) )

# Save fixed effect dataframe
saveRDS(h.sen.lag.fe.df, here("analysis", "outputs", "models", "h.sen.lag.fe.df.rds"))


# ---- 5. WAIC and PSIS comparison full models vs. sensitivity analysis models ----
h.0.waic.loo <- readRDS(here("analysis", "outputs", "models","h.0.waic.loo.rds"))
h.0.psis.loo <- readRDS(here("analysis", "outputs", "models","h.0.psis.loo.rds"))
h.0.sen.waic.loo <- readRDS(here("analysis", "outputs", "models","h.0.sen.waic.loo.rds"))
h.0.sen.psis.loo <- readRDS(here("analysis", "outputs", "models","h.0.sen.psis.loo.rds"))

fx.loo.diff.table <- function(tbl, dataset_label = "", criterion_label = "") {
  # tbl: output of fx.compare.loo() -- has elpd_diff, se_diff, p_worse,
  # diag_diff, diag_elpd, and waic/looic, se_waic/se_looic
  ic_col    <- if ("waic" %in% names(tbl)) "waic" else "looic"
  se_ic_col <- if ("waic" %in% names(tbl)) "se_waic" else "se_looic"
  
  ci_lo <- tbl$elpd_diff - 1.96 * tbl$se_diff
  ci_hi <- tbl$elpd_diff + 1.96 * tbl$se_diff
  
  data.frame(
    Dataset                = dataset_label,
    Criterion              = criterion_label,
    Model                  = rownames(tbl),
    Criterion_value        = round(tbl[[ic_col]], 2),
    Criterion_value_se     = round(tbl[[se_ic_col]], 2),
    ELPD_diff              = round(tbl$elpd_diff, 2),
    ELPD_diff_se           = round(tbl$se_diff, 2),
    CI_lo                  = round(ci_lo, 2),
    CI_hi                  = round(ci_hi, 2),
    Pareto_k_diagnostic    = ifelse(trimws(tbl$diag_elpd) == "", "-", trimws(tbl$diag_elpd)),
    Significant            = ifelse(tbl$elpd_diff == 0, NA, !(ci_lo < 0 & ci_hi > 0))
    # Prob_worse             = round(tbl$p_worse, 2),
    # Comparison_diagnostic  = ifelse(trimws(tbl$diag_diff) == "", "-", trimws(tbl$diag_diff)),

  )
}

h.0.waic.diff     <- fx.loo.diff.table(h.0.waic.loo,     "Full dataset",        "WAIC")
h.0.psis.diff     <- fx.loo.diff.table(h.0.psis.loo,     "Full dataset",        "PSIS-LOO")
h.0.sen.waic.diff <- fx.loo.diff.table(h.0.sen.waic.loo, "Sensitivity analysis", "WAIC")
h.0.sen.psis.diff <- fx.loo.diff.table(h.0.sen.psis.loo, "Sensitivity analysis", "PSIS-LOO")

gof_diff_summary <- rbind(h.0.waic.diff, h.0.psis.diff, h.0.sen.waic.diff, h.0.sen.psis.diff)

saveRDS(gof_diff_summary, here("analysis", "outputs", "models", "gof_diff_summary.rds"))

# ---- 6. Posterior predictive checks all models ----

ppc <- bind_rows(ppc_nb,
                 ppc_b1, ppc_b2,
                 ppc_m.b.2,
                 ppc_m01,
                 ppc_h01, ppc_h02, ppc_h03)

saveRDS(ppc , here("analysis", "outputs", "models", "ppc.rds"))
