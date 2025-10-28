# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## MODEL FITTING ===============================================================

# Description:
#     Functions to extract goodness of fit metrics and parameter estimates from
#     fitted models.

# Paper:
#     Detection of dengue virus in Aedes aegypti during an urban epidemic in
#     Iquitos, Peru (December 2010 to March 2011)

# Script author:
#     Anna B. Kawiecki        ORCID: 0000-0002-0499-2612

# ---- 0. Load -----------------------------------------------------------------

# ---- 0.1 Read in R libraries ----

# Simplifies the use of relative file paths
library(here)

# Core packages for data manipulation and visualization
library(tidyverse)

# Integrated Nested Laplace Approximation for Bayesian inference
library(INLA)

# R interface to Stan for Bayesian modeling
library(rstan)

# Simple features for handling spatial vector data
library(sf)

# R interface to CmdStan, a backend for fitting Stan models
library(cmdstanr)

# ---- 1. Difference in WAIC ---------------------------------------------------

# fx.waic
# This function computes the WAIC (Watanabe-Akaike Information Criterion)
# for a list of INLA models and returns a data frame containing:
# - Fixed and random effects used in each model
# - WAIC values with standard error and confidence intervals
# - Differences in WAIC compared to the best (lowest WAIC) model
# - Predictive penalties
#
# Output: Data frame with WAIC statistics for model comparison

fx.waic <- function(m) {
  # f.waic.min
  # Helper function to identify the model with the lowest WAIC
  # Input: List of INLA models
  # Output: Model with minimum WAIC value
  f.waic.min <- function(m) {

    # Extract the WAIC value from each model
    min.waic <- min(unlist(lapply(1:length(m), function(x) {
      m[[x]][["waic"]][["waic"]]
    })))

    # Identify which model(s) have the minimum WAIC
    min.waic.T <- sapply(1:length(m),
                         function(x) m[[x]][["waic"]][["waic"]] == min.waic)

    # Return the model(s) with the minimum WAIC
    m.min.waic <- m[min.waic.T == TRUE]
  }

  # Apply helper function to find the best model
  m.min.waic <- f.waic.min(m)

  # f.waic.compare
  # Helper function to extract and calculate WAIC metrics for one model
  # Input: An index from a list of INLA models.
  # Output: A data frame of metrics for each model compared to the lowest WAIC
  f.waic.compare <- function(x) {
    # Number of observations
    n <- length(m[[x]][["waic"]][["local.waic"]])

    waic.compare <- list(

      # Fixed effects
      fixed = c(paste(m[[x]][["names.fixed"]], collapse = "+")),
      # Fixed effects without the intercept
      fixed.s = m[[x]][["fixed.effect.s"]],
      # Random effects
      random = c(paste(m[[x]][["model.random"]], collapse = "+")),
      # WAIC value
      waic = m[[x]][["waic"]][["waic"]],
      # Standard error of the point-wise WAIC
      s.e = as.numeric(sqrt(n * var(m[[x]][["waic"]][["local.waic"]],
                                    na.rm = TRUE))),
      # Lower CI of the point-wise WAIC
      ci.l = m[[x]][["waic"]][["waic"]] - as.numeric(
        sqrt(n * var(m[[x]][["waic"]][["local.waic"]],na.rm = TRUE))) * 2.6,
      # Upper CI of the point-wise WAIC
      ci.u = m[[x]][["waic"]][["waic"]] + as.numeric(
        sqrt(n * var(m[[x]][["waic"]][["local.waic"]], na.rm = TRUE))) * 2.6,
      # Difference between each model's WAIC and the lowest WAIC
      d.mean.waic = m[[x]][["waic"]][["waic"]] -
        m.min.waic[[1]][["waic"]][["waic"]],
      # Standard error of the difference in WAIC
      d.s.e = as.numeric(
        sqrt(n * var(m[[x]][["waic"]][["local.waic"]] -
                       m.min.waic[[1]][["waic"]][["local.waic"]],
                                      na.rm = TRUE))),
      # Lower CI of the difference in WAIC
      ci.d.l = m[[x]][["waic"]][["waic"]] -
        m.min.waic[[1]][["waic"]][["waic"]] -
        as.numeric(sqrt(n * var(m[[x]][["waic"]][["local.waic"]] -
                                  m.min.waic[[1]][["waic"]][["local.waic"]],
                                na.rm = TRUE))) * 2.6,
      # Upper CI of the difference in WAIC
      ci.d.u = m[[x]][["waic"]][["waic"]] -
        m.min.waic[[1]][["waic"]][["waic"]] +
        as.numeric(sqrt(n * var(m[[x]][["waic"]][["local.waic"]] -
                                  m.min.waic[[1]][["waic"]][["local.waic"]],
                                na.rm = TRUE))) * 2.6,
      # Prediction penalty
      p.waic = sum(m[[x]][["waic"]][["local.p.eff"]], na.rm = TRUE),
      # Model ID: index of the model
      m = paste("m", x, sep = ".")
    )
  }

  df.model <- lapply(1:length(m), f.waic.compare) %>%
    bind_rows() %>%
    arrange(waic)
}

saveRDS(fx.waic, here("analysis", "data", "derived_data", "fx.waic.rds"))

# ---- 2. Extract fixed effects from models fitted with R-INLA -----------------

# fx.fix.eff
# This function extracts and formats the fixed effect estimates from a list
# of models fitted using R-INLA (Integrated Nested Laplace Approximation).

# Input:
#   - m: A list of INLA model objects.
# Output:
#   - A combined data frame containing the exponentiated fixed effects estimates,
#     model specifications (fixed and random effects), and model identifiers.

fx.fix.eff <- function(m) {
  # f.fix.eff.exp
  # Extracts and exponentiates fixed effect estimates for each fitted model
  f.fix.eff.exp <- function(x) {
    # Extract and exponentiate fixed effect estimates from the model at index x.
    round(exp(m[[x]][["summary.fixed"]]), digits = 2) %>%
      # Convert row names (variable names) to a column named "variable".
      rownames_to_column(var = "variable") %>%
      # Append model-specific metadata:
      mutate(
        # Concatenate all fixed effect variable names (including intercept).
        fixed = paste(m[[x]][["names.fixed"]], collapse = "+"),
        # Concatenate all random effect terms.
        random = paste(m[[x]][["model.random"]], collapse = "+"),
        # List of fixed effect terms excluding the intercept (custom field).
        fixed.s = m[[x]][["fixed.effect.s"]],
        # Create a unique model identifier using the index.
        m = paste("m", x, sep = ".")
      ) %>%
      # Rename quantile columns for clarity and consistency.
      rename(q0.025 = "0.025quant", q0.5 = "0.5quant", q0.975 = "0.975quant")
  }

  # Apply the extraction function to each model in the list and combine all outputs.
  lapply(1:length(m), f.fix.eff.exp) %>% bind_rows()
}

# Save the resulting function to an RDS file for later use in the analysis pipeline.
saveRDS(fx.fix.eff, here("analysis", "data", "derived_data",  "fx.fix.eff.rds"))

# ---- 3.Extract fixed effects from models fitted with STAN --------------------

# fx.stan.f.e
# This function extracts and exponentiates the fixed effect estimates from a
# list of models fitted using STAN

# Input:
#   - m: A list of model objects fitted in STAN.
# Output:
#   - A combined data frame containing the exponentiated fixed effects estimates,
#     model specifications (fixed and random effects), and model identifiers.

fx.stan.f.e <- function(m) {
  # Internal function to process a single STAN model object from the list
  f.fix.eff.exp <- function(x) {
    as.data.frame(summary(m[[x]])) %>%
      # Convert row names (parameter names) to a column for easier manipulation
      rownames_to_column(var = "parameter") %>%
      # Select relevant summary statistics for each parameter
      dplyr::select(c(
        "parameter",            # Name of the parameter
        "summary.mean",         # Posterior mean
        "summary.se_mean",      # Standard error of the mean
        "summary.sd",           # Posterior standard deviation
        "summary.2.5.",         # 2.5% posterior quantile
        "summary.25.",          # 25% posterior quantile
        "summary.50.",          # Median
        "summary.97.5."         # 97.5% posterior quantile
      )) %>%
      # Filter to include only parameters of interest:
      # - "alpha", "beta": fixed effect coefficients
      # - "w[1]" to "w[5]": possible group-level or varying effect terms
      # - "sigma": residual or observation-level standard deviation
      filter(parameter %in% c("alpha", "beta", "w[1]", "w[2]",
                              "w[3]", "w[4]", "w[5]", "sigma")) %>%
      # Exponentiate the mean and selected quantiles
      mutate(
        mean.exp = exp(summary.mean),
        sd.exp = exp(summary.sd),
        q2.5.exp = exp(summary.2.5.),
        q97.5.exp = exp(summary.97.5.)
      ) %>%
      # Add model identifier and index
      mutate(
        m = paste("m", x, sep = "."),
        index = x
      )
  }

  # Apply the above extraction function to each model in the list and
  # combine all results into a single data frame
  lapply(1:length(m), f.fix.eff.exp) %>% bind_rows()
}

# Save the extraction function as an RDS file for reuse in the analysis pipeline
saveRDS(fx.stan.f.e, here("analysis", "data", "derived_data", "fx.stan.f.e.rds"))

