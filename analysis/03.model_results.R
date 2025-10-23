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

<<<<<<< HEAD
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
library(pacman)
pacman::p_load(
  here,         # Manage relative file paths
  tidyverse,    # Data wrangling and visualization
  psych,        # Descriptive stats and psychometrics
  MASS,         # Statistical functions and distributions
  INLA,         # Bayesian inference using INLA
  inlabru,      # Interface for INLA models
  rstan,        # R interface to Stan
  loo,          # Model comparison via LOO/WAIC
  sf,           # Spatial data handling
  cmdstanr,     # R interface to CmdStan
  rethinking    # Tools for Bayesian analysis (McElreath)
)
>>>>>>> 039226c2534a9e37020f42822743fb30c9c8b648

# ---- 0.2 Read in functions ----

# Load custom helper functions for model extraction and summarization
# Custom WAIC extraction
fx.waic      <- readRDS(here("analysis", "functions", "fx.waic.rds"))
# Custom function to extract fixed effects from model fitted with INLA
fx.fix.eff   <- readRDS(here("analysis", "functions", "fx.fix.eff.rds"))
# Custom Custom function to extract fixed effects from model fitted with Stan
fx.stan.f.e  <- readRDS(here("analysis", "functions", "fx.stan.f.e.rds"))

# ---- 1. Ae. aegypti surveillance using a case-contact sampling strategy vs. broader surveillance -----

# ---- 1.1  Negative binomial models: ----
# Association between Ae. aegypti surveillance strategy and abundance ----

# Load fitted model objects
m.s.nb.base.0 <- readRDS(here("analysis", "outputs", "models", "m.s.nb.base.0.rds"))
m.s.nb.base.1 <- readRDS(here("analysis", "outputs", "models", "m.s.nb.base.1.rds"))
m.s.nb.1 <- readRDS(here("analysis", "outputs", "models", "m.s.nb.1.rds"))

# Combine the three models into a list for evaluation
m.s.nb.list <- list(m.s.nb.base.0, m.s.nb.base.1, m.s.nb.1)

# ---- 1.1.1 Compare WAIC of negative binomial models ----

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

# ---- 1.2  Logistic models: ----
# Association between the surveillance strategy and probability of DENV detection ----

# Load fitted model objects
m.s.b.base.0 <- readRDS(here("analysis", "outputs", "models", "m.s.b.base.0.rds"))
m.s.b.base.1 <- readRDS(here("analysis", "outputs", "models", "m.s.b.base.1.rds"))
m.s.b.1 <- readRDS(here("analysis", "outputs", "models", "m.s.b.1.rds"))

# Combine the three models into a list for evaluation
m.s.b.list <- list(m.s.b.base.0, m.s.b.base.1, m.s.b.1 )

# ---- 1.2.1 Compare WAIC of logistic models ----

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

# ---- 1.2.2 Evaluate fixed effects of logistic models ----

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


# ---- 2. Association between Ae. aegypti abundance and probability of DENV detection ----

# ---- 2.1 Household-level logistic model ----

# Load fitted model objects
m.b.base.0 <- readRDS( here("analysis", "outputs", "models", "m.b.base.0.rds"))
m.b.base.1 <- readRDS( here("analysis", "outputs", "models", "m.b.base.1.rds"))
m.b.0 <- readRDS( here("analysis", "outputs", "models", "m.b.0.rds"))
m.b.1 <- readRDS( here("analysis", "outputs", "models", "m.b.1.rds"))
m.b.2 <- readRDS( here("analysis", "outputs", "models", "m.b.2.rds"))

# Combine the five models into a list for evaluation
m.b.list <- list(m.b.base.0, m.b.base.1, m.b.0, m.b.1, m.b.2 )

# ---- 2.1.1 Compare WAIC of household-level logistic models ----

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

# ---- 2.2 Area-level logistic models ----

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

# ---- 2.2.2 Evaluate fixed effects of the area-level logistic models ----

# Extract fixed effects (posterior summaries) from all models
m.lag.fe <- fx.stan.f.e(m.lag.list)

# Combine model estimates with metadata and prepare for visualization and analysis
m.lag.fe.df <- m.lag.fe %>%
  # Merge with model index
  left_join(m.lag.list.index) %>%
  # Exclude intercept and error scale
  filter(parameter != "alpha" & parameter != "sigma") %>%
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

# ---- 3. Association between entomological surveillance metrics and human dengue incidence ----

# ---- 3.1 Dirichlet weighted models with complete 4-week lag dataset ----

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

# ---- 3.1.1 Compare by WAIC ----

h.0.waic <- rethinking::compare( fit.h.01, fit.h.02,fit.h.03, func=WAIC)

# Label models by explanatory variable (each of the entomological metrics)
h.0.waic$model.name <- c("h.01","h.03","h.02")

rownames(h.0.waic) <- c("Vector DENV prevalence model",
                        "Vector index model",
                        "Average vector abundance model")

saveRDS(h.0.waic, here("analysis", "outputs", "models","h.0.waic.rds"))

# ---- 3.2 Effect of all entomological metric models ----

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
saveRDS(h.lag.fe.df, here("analysis", "outputs", "models", "h.lag.fe.df.rds"))



