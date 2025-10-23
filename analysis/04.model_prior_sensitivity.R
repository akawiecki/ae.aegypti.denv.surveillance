# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## MODEL PRIOR SENSITIVITY ANALYISIS ===========================================

# Description:
#     Perform prior sensitivity analysis for Dirichlet-weighted 4-week lags
#     area-level models

# Paper:
#     Detection of dengue virus in Aedes aegypti during an urban epidemic in
#     Iquitos, Peru (December 2010 to March 2011)

# Script author:
#     Anna B. Kawiecki        ORCID: 0000-0002-0499-2612

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# ---- 0. Load R libraries -----------------------------------------------------

# ---- 0.1 Install libraries ----

# Install cmdstanr
# Use a fresh R session or restart your current session
# install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))

# Install rethinking
# Install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
#devtools::install_github("rmcelreath/rethinking")

# Install INLA_24.12.11

# ---- 0.2 Read in libraries ----

<<<<<<< HEAD
# Load required libraries ----

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

# Efficient leave-one-out cross-validation for Bayesian models
library(loo)

# Simple features for handling spatial vector data
library(sf)

# R interface to CmdStan, a backend for fitting Stan models
library(cmdstanr)

# Tools and examples for Bayesian data analysis (from McElreath)
library(rethinking)
=======
# Load required libraries using pacman for efficient package management
library(pacman)

pacman::p_load(
  here,        # Simplifies the use of relative file paths
  tidyverse,   # Core packages for data manipulation and visualization
  psych,       # Functions for descriptive statistics and psychometrics
  MASS,        # Functions for statistical methods and distribution fitting
  INLA,        # Integrated Nested Laplace Approximation for Bayesian inference
  rstan,       # R interface to Stan for Bayesian modeling
  loo,         # Efficient leave-one-out cross-validation for Bayesian models
  sf,          # Simple features for handling spatial vector data
  cmdstanr,    # R interface to CmdStan, a backend for fitting Stan models
  rethinking   # Tools and examples for Bayesian data analysis (from McElreath)
)

>>>>>>> 039226c2534a9e37020f42822743fb30c9c8b648

# ---- 0.3 Read in custom functions ----

# Custom function to extract fixed effects from model fitted with Stan
fx.stan.f.e  <- readRDS(here("analysis", "functions", "fx.stan.f.e.rds"))

# ---- 1. Data for prior sensitivity analysis ----------------------------------

# ---- 1.1 Read in data ----

# Import area-level data

m.area.week.lag <- readRDS(here("analysis", "data", "derived_data",
                                "area_level_data","m.area.week.lag.rds"))
m.area.week.lag.4 <- readRDS(here("analysis", "data", "derived_data",
                                  "area_level_data","m.area.week.lag.4.rds"))

h.area.week.lag <- readRDS(here("analysis", "data", "derived_data",
                                "area_level_data","h.area.week.lag.rds"))
h.area.week.lag.4 <- readRDS(here("analysis", "data", "derived_data",
                                  "area_level_data","h.area.week.lag.4.rds"))


# ---- 2. Association between Ae. aegypti abundance and probability of DENV detection ----

# ---- 2.1 Run prior sensitivity analysis ----

# Load 4-week lag dataset for modeling
m.week <- m.area.week.lag.4

# Store the number of observations (rows) in the dataset
nobsv = nrow(m.week)

# Prepare input list for Stan model using Dirichlet-weighted lags
dat.m.01 <- list(
  N = nobsv,                         # Total number of observations
  n = m.week$n.tested,               # Vector: number of mosquitoes tested
  K = 5,                             # Number of lagged predictors (0 to 4 weeks)
  x = cbind(m.week$week_lag_0,       # Matrix of lagged abundance predictors
            m.week$week_lag_1,
            m.week$week_lag_2,
            m.week$week_lag_3,
            m.week$week_lag_4),
  y = m.week$n.denv,                 # Vector: number of DENV-positive mosquitoes
  a = rep(2, 5)                      # Dirichlet prior concentration vector
  # (with α=2 for each lag)
)


# Define prior sets to be tested
prior.sets.m.01 <- list(
  list(alpha_prior = "normal(0, 0.2)", beta_prior = "normal(0, 0.2)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 0.2)", beta_prior = "normal(0, 1)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 0.5)", beta_prior = "normal(0, 0.2)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 0.5)", beta_prior = "normal(0, 1.5)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 0.5)", beta_prior = "normal(0, 10)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 0.5)", beta_prior = "normal(0, 0.5)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 1.5)", beta_prior = "normal(0, 0.5)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 1.5)", beta_prior = "normal(0, 1.0)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 1.5)", beta_prior = "normal(0, 0.2)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 1.5)", beta_prior = "normal(0, 10)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 10)", beta_prior = "normal(0, 1.0)",
       sigma_prior = "exponential(1)")
)

# Function to run the model with different priors
fx.priors.m.01 <- function(alpha_prior, beta_prior, sigma_prior) {
  model_code <- paste0("
    data {
      int<lower=0> N;
      array[N] int n;
      int<lower=0> K;
      matrix[N, K] x;
      array[N] int y;
      vector[5] a;
    }
    parameters {
      real alpha;
      real beta;
      simplex[5] w;
      real<lower=0> sigma;
    }
    model{
      vector[N] p;
      alpha ~ ", alpha_prior, ";
      beta ~ ", beta_prior, ";
      w ~ dirichlet(a);
      sigma ~ ", sigma_prior, ";
      for (i in 1:N) {
        p[i] = alpha + (x[i,1] * w[1] + x[i,2] * w[2] + x[i,3] * w[3] + x[i,4] * w[4] + x[i,5] * w[5]) * beta;
        p[i] = inv_logit(p[i]);
      }
      y ~ binomial(n, p);
    }
    generated quantities {
      vector[N] log_lik;
      vector[N] p;
      for (i in 1:N) {
        p[i] = alpha + (x[i,1] * w[1] + x[i,2] * w[2] + x[i,3] * w[3] + x[i,4] * w[4] + x[i,5] * w[5]) * beta;
        p[i] = inv_logit(p[i]);
      }
      for (i in 1:N) log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
    }
  ")

  rstan::stan(model_code = model_code, data = dat.m.01, iter = 2000, chains = 4,
              control = list(adapt_delta = 0.99))
}

# Run models with different priors
priors.fit.m.01 <- lapply(prior.sets.m.01,
                          function(p) fx.priors.m.01(p$alpha_prior,
                                                     p$beta_prior,
                                                     p$sigma_prior))

saveRDS(priors.fit.m.01,here("analysis", "outputs", "models",
                             "priors.fit.m.01.rds"))

# ---- 2.2  Process prior sensitivity analysis results ----

# Create a dataframe defining model names, prior combinations, and an index
priors.fit.m.01.index <- data.frame(model.name= c("a= 0.2\nb=0.2",
                                                  "a= 0.2\nb=1",
                                                  "a= 0.5\nb=0.2",
                                                  "a= 0.5\nb=1.5",
                                                  "a= 0.5\nb=10",
                                                  "a= 0.5\nb=0.5",
                                                  "a= 1.5\nb=0.5",
                                                  "a=1.5\nb=1",
                                                  "a=1.5\nb=0.2",
                                                  "a=1.5\nb=10",
                                                  "a=10\nb=1"),
                                    prior= c("alpha ~ N(0,0.2), beta~ N(0,0.2)",
                                             "alpha ~ N(0,0.2), beta~ N(0,1)",
                                             "alpha ~ N(0,0.5), beta~ N(0,0.1)",
                                             "alpha ~ N(0,0.5), beta~ N(0,0.2)",
                                             "alpha ~ N(0,0.5), beta~ N(0,1.5)",
                                             "alpha ~ N(0,0.5), beta~ N(0,10)",
                                             "alpha ~ N(0,1.5), beta~ N(0,0.5)",
                                             "alpha ~ N(0,1.5), beta~ N(0,1)",
                                             "alpha ~ N(0,1.5), beta~ N(0,0.2)",
                                             "alpha ~ N(0,1.5), beta~ N(0,10)",
                                             "alpha ~ N(0,10), beta~ N(0,1)"
                                    ),
                                    index= seq(1,length(priors.fit.m.01), by=1))

# Extract fixed effects from models
priors.m.01.fe <- fx.stan.f.e(priors.fit.m.01)

# Combine model output with prior information and relabel parameters
priors.m.01.fe.df <- priors.m.01.fe %>%
  left_join(priors.fit.m.01.index) %>%
  mutate(parameter.name= case_when(
    parameter=="w[1]"~ "same week",
    parameter=="w[2]"~"1 week lag",
    parameter=="w[3]"~"2 week lag",
    parameter=="w[4]"~"3 week lag",
    parameter=="w[5]"~"4 week lag",
    T~ parameter
  )) %>%
  mutate(parameter.name= factor(parameter.name,
                                levels = c("alpha" ,
                                           "beta",
                                           "same week",
                                           "1 week lag",
                                           "2 week lag",
                                           "3 week lag",
                                           "4 week lag",
                                           "sigma"  ))) %>%
  filter(str_detect(pattern = "a= 0.2", model.name)==FALSE)

# Save the formatted model output as an RDS file
saveRDS(priors.m.01.fe.df , here("analysis", "outputs", "models",
                                 "priors.m.01.fe.df.rds"))


# ---- 3. Association between Ae. aegypti DENV prevalence and DENV incidence in humans ----

# ---- 3.1 Run prior sensitivity analysis ----

# Load the dataset with Ae. aegypti prevalence over 0–4 week lags
h.week <- h.area.week.lag.4

# Get the number of weekly observations
nobsv = nrow(h.week)

# Prepare input list for the Stan model with Dirichlet-weighted lag structure
dat.h.01 <- list(
  N = nobsv,                     # Number of observations
  n = h.week$n.ind,              # Number of individuals under surveillance per week
  K = 5,                         # Number of lagged predictors (weeks 0 to 4)
  x = cbind(                     # Predictor matrix with 5 weekly lags of Ae. aegypti prevalence
    h.week$prev.week_lag_0,
    h.week$prev.week_lag_1,
    h.week$prev.week_lag_2,
    h.week$prev.week_lag_3,
    h.week$prev.week_lag_4
  ),
  y = h.week$n.denv,            # Outcome: number of DENV-positive individuals per week
  a = rep(2, 5)                 # Dirichlet prior parameters (α = 2 for each lag)
)

# Define different prior sets
prior.sets.h.01 <- list(
  list(alpha_prior = "normal(0, 0.2)", beta_prior = "normal(0, 0.2)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 0.2)", beta_prior = "normal(0, 1)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 0.5)", beta_prior = "normal(0, 0.1)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 0.5)", beta_prior = "normal(0, 0.2)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 0.5)", beta_prior = "normal(0, 1.5)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 0.5)", beta_prior = "normal(0, 10)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 1.5)", beta_prior = "normal(0, 0.5)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 1.5)", beta_prior = "normal(0, 1.0)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 1.5)", beta_prior = "normal(0, 0.2)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 1.5)", beta_prior = "normal(0, 10)",
       sigma_prior = "exponential(1)"),
  list(alpha_prior = "normal(0, 10)", beta_prior = "normal(0, 1.0)",
       sigma_prior = "exponential(1)")
)

# Function to run the model with different priors
fx.priors.h.01 <- function(alpha_prior, beta_prior, sigma_prior) {
  model_code <- paste0("
    data {
      int<lower=0> N;
      array[N] int n;
      int<lower=0> K;
      matrix[N, K] x;
      array[N] int y;
      vector[5] a;
    }
    parameters {
      real alpha;
      real beta;
      simplex[5] w;
      real<lower=0> sigma;
    }
    model{
      vector[N] p;
      alpha ~ ", alpha_prior, ";
      beta ~ ", beta_prior, ";
      w ~ dirichlet(a);
      sigma ~ ", sigma_prior, ";
      for (i in 1:N) {
        p[i] = alpha + (x[i,1] * w[1] + x[i,2] * w[2] + x[i,3] * w[3] + x[i,4] * w[4] + x[i,5] * w[5]) * beta;
        p[i] = inv_logit(p[i]);
      }
      y ~ binomial(n, p);
    }
    generated quantities {
      vector[N] log_lik;
      vector[N] p;
      for (i in 1:N) {
        p[i] = alpha + (x[i,1] * w[1] + x[i,2] * w[2] + x[i,3] * w[3] + x[i,4] * w[4] + x[i,5] * w[5]) * beta;
        p[i] = inv_logit(p[i]);
      }
      for (i in 1:N) log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
    }
  ")

  rstan::stan(model_code = model_code, data = dat.h.01, iter = 2000, chains = 4,
              control = list(adapt_delta = 0.99))
}

# Run models with different priors
priors.fit.h.01 <- lapply(prior.sets.h.01,
                          function(p) fx.priors.h.01(p$alpha_prior,
                                                     p$beta_prior,
                                                     p$sigma_prior))

# Save the formatted model output as an RDS file
saveRDS(priors.fit.h.01,here("analysis", "outputs", "models",
                             "priors.fit.h.01.rds"))

# ---- 3.2 Process prior sensitivity analysis results ----

# Create a dataframe defining model names, prior combinations, and an index
priors.fit.h.01.index <- data.frame(model.name= c("a= 0.2\nb=0.2",
                                                  "a= 0.2\nb=1",
                                                  "a= 0.5\nb=0.1",
                                                  "a= 0.5\nb=0.2",
                                                  "a= 0.5\nb=1.5",
                                                  "a= 0.5\nb=10",
                                                  "a= 1.5\nb=0.5",
                                                  "a=1.5\nb=1",
                                                  "a=1.5\nb=0.2",
                                                  "a=1.5\nb=10",
                                                  "a=10\nb=1"),
                                    prior= c("alpha ~ N(0,0.2), beta~ N(0,0.2)",
                                             "alpha ~ N(0,0.2), beta~ N(0,1)",
                                             "alpha ~ N(0,0.5), beta~ N(0,0.1)",
                                             "alpha ~ N(0,0.5), beta~ N(0,0.2)",
                                             "alpha ~ N(0,0.5), beta~ N(0,1.5)",
                                             "alpha ~ N(0,0.5), beta~ N(0,10)",
                                             "alpha ~ N(0,1.5), beta~ N(0,0.5)",
                                             "alpha ~ N(0,1.5), beta~ N(0,1)",
                                             "alpha ~ N(0,1.5), beta~ N(0,0.2)",
                                             "alpha ~ N(0,1.5), beta~ N(0,10)",
                                             "alpha ~ N(0,10), beta~ N(0,1)"),
                                    index= seq(1,length(priors.fit.h.01), by=1))

# Extract fixed effects
priors.h.01.fe <- fx.stan.f.e(priors.fit.h.01)

# Create data frame from plotting with interpretable labels
priors.h.01.fe.df <- priors.h.01.fe %>%
  left_join(priors.fit.h.01.index) %>%
  mutate(parameter.name= case_when(
    parameter=="w[1]"~ "same week",
    parameter=="w[2]"~"1 week lag",
    parameter=="w[3]"~"2 week lag",
    parameter=="w[4]"~"3 week lag",
    parameter=="w[5]"~"4 week lag",
    T~ parameter
  )) %>%
  mutate(parameter.name= factor(parameter.name,
                                levels = c("alpha" ,
                                           "beta",
                                           "same week",
                                           "1 week lag",
                                           "2 week lag",
                                           "3 week lag",
                                           "4 week lag",
                                           "sigma"  ))) %>%
  filter(str_detect(pattern = "a= 0.2", model.name)==FALSE)


# Save the formatted model output as an RDS file
saveRDS(priors.h.01.fe.df,here("analysis", "outputs", "models",
                             "priors.h.01.fe.df.rds"))
