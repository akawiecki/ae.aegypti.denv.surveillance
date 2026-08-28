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

# ---- 1. Extract fixed effects from models fitted with R-INLA -----------------

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
saveRDS(fx.fix.eff, here("analysis",  "functions",  "fx.fix.eff.rds"))

# ---- 2.Extract fixed effects from models fitted with STAN --------------------

# fx.stan.f.e
# This function extracts and exponentiates the fixed effect estimates from a
# list of models fitted using STAN

# Input:
#   - m: A list of model objects fitted in STAN.
# Output:
#   - A combined data frame containing the exponentiated fixed effects estimates,
#     model specifications (fixed and random effects), and model identifiers.

fx.stan.f.e <- function(m) {
  f.fix.eff.exp <- function(x) {
    m[[x]]$summary(
      variables = c("alpha", "beta", "w[1]", "w[2]", "w[3]", "w[4]", "w[5]", "sigma"),
      mean, sd,
      ~quantile(.x, probs = c(0.025, 0.25, 0.5, 0.975)),
      se_mean = ~sd(.x) / sqrt(length(.x))
    ) %>%
      rename(
        parameter       = variable,
        summary.mean    = mean,
        summary.sd      = sd,
        summary.2.5.    = `2.5%`,
        summary.25.     = `25%`,
        summary.50.     = `50%`,
        summary.97.5.   = `97.5%`,
        summary.se_mean = se_mean
      ) %>%
      mutate(
        mean.exp  = exp(summary.mean),
        sd.exp    = exp(summary.sd),
        q2.5.exp  = exp(summary.2.5.),
        q97.5.exp = exp(summary.97.5.),
        m         = paste("m", x, sep = "."),
        index     = x
      )
  }
  lapply(seq_along(m), f.fix.eff.exp) %>% bind_rows()
}

# Save the extraction function as an RDS file for reuse in the analysis pipeline
saveRDS(fx.stan.f.e, here("analysis",  "functions", "fx.stan.f.e.rds"))

# ---- 3.Extract fixed effects from models fitted with cmdstanr ----------------

fx.cmdstanr.f.e <- function(m) {
  f.fix.eff.exp <- function(x) {
    m[[x]]$summary(
      variables = c("alpha", "beta", "w[1]", "w[2]", "w[3]", "w[4]", "w[5]", "sigma"),
      mean, sd,
      ~quantile(.x, probs = c(0.025, 0.25, 0.5, 0.975)),
      se_mean = ~sd(.x) / sqrt(length(.x))
    ) %>%
      rename(
        parameter       = variable,
        summary.mean    = mean,
        summary.sd      = sd,
        summary.2.5.    = `2.5%`,
        summary.25.     = `25%`,
        summary.50.     = `50%`,
        summary.97.5.   = `97.5%`,
        summary.se_mean = se_mean
      ) %>%
      mutate(
        mean.exp  = exp(summary.mean),
        sd.exp    = exp(summary.sd),
        q2.5.exp  = exp(summary.2.5.),
        q97.5.exp = exp(summary.97.5.),
        m         = paste("m", x, sep = "."),
        index     = x
      )
  }
  lapply(seq_along(m), f.fix.eff.exp) %>% bind_rows()
}

# Save the extraction function as an RDS file for reuse in the analysis pipeline
saveRDS(fx.cmdstanr.f.e, here("analysis",  "functions", "fx.cmdstanr.f.e.rds"))


# ---- 4.Compare STAN models with WAIC and PSIS ----------------

# fx.get.log.lik.matrix 
# Purpose:  Extract the point‑wise log‑likelihood matrix from a fitted model,
#           whether the model came from rstan (stanfit) or cmdstanr (CmdStanMCMC).
# Returns:  A matrix (draws × observations) of log‑likelihood values.

fx.get.log.lik.matrix <- function(fit) {
  # rstan case
  if (inherits(fit, "stanfit")) {             
    # extract_log_lik pulls log_lik for each observation & each draw,
    # merge_chains = TRUE combines all chains into one matrix.
    loo::extract_log_lik(fit, merge_chains = TRUE)
    
    # cmdstanr case 
  } else if (inherits(fit, "CmdStanMCMC")) {  
    # draws() retrieves the specified variable; format = "matrix" gives
    # a draws‑by‑observations matrix, same shape as the rstan output.
    fit$draws("log_lik", format = "matrix")
    
    # unsupported type 
  } else {
    # Stop with a clear message so the user knows what went wrong.
    stop("Unsupported fit object class: ", class(fit))
  }
}

saveRDS(fx.get.log.lik.matrix, here("analysis",  "functions", "fx.get.log.lik.matrix.rds"))

# fx.compare.loo 
# Purpose:  Compute WAIC or PSIS‑LOO for a list of models and return
#           a tidy data.frame whose rows are labelled with the supplied
#           model names.
# Arguments:
#   fit_list   – list of fitted model objects (stanfit or CmdStanMCMC)
#   model_names– character vector of names, same length as fit_list
#   method     – "waic" or "psis" (default: waic)
# Returns:   data.frame with columns: elpd_diff, se_diff, etc.

fx.compare.loo <- function(fit_list, model_names,
                           method = c("waic", "psis")) {
  method <- match.arg(method)   # ensure method is either "waic" or "psis"
  
  # Compute criterion for each model 
  crit_list <- lapply(fit_list, function(fit) {
    # 1. Get the log‑likelihood matrix (draws × observations)
    log_lik <- fx.get.log.lik.matrix(fit)
    # 2. Feed it to the appropriate LOO function
    if (method == "waic") loo::waic(log_lik) else loo::loo(log_lik)
  })
  
  # Compare all criteria
  tbl <- loo::loo_compare(crit_list)   # matrix with model1, model2, … rows
  
  # Replace generic row names with the real model names 
  # loo_compare names rows "model1", "model2", … according to the
  # original order in fit_list/model_names.
  rownames(tbl) <- model_names[as.integer(gsub("model", "", rownames(tbl)))]
  
  # Return as a plain data.frame 
  as.data.frame(tbl)
}

saveRDS(fx.compare.loo, here("analysis",  "functions", "fx.compare.loo.rds"))

# ---- 4.Compute fold-change and probability  ----------------

# Unified function to compute fold-change and probability draws for one model
# model: a stanfit object (not an index/list - pass the fitted model directly)
# range_min, range_max: observed min/max of the explanatory variable
fx_fold_prob <- function(model, range_min, range_max, label = NA) {
  post <- rstan::extract(model, pars = c("alpha", "beta"))
  alpha_draws <- post$alpha
  beta_draws  <- post$beta
  
  fold_draws  <- exp(beta_draws * (range_max - range_min))
  p_min_draws <- inverse_logit(alpha_draws + beta_draws * range_min)
  p_max_draws <- inverse_logit(alpha_draws + beta_draws * range_max)
  
  tibble(
    metric         = label,
    range_min      = range_min,
    range_max      = range_max,
    fold_mean      = round(mean(fold_draws),2),
    fold_median    = round(quantile(fold_draws, 0.5),2),
    fold_low       = round(quantile(fold_draws, 0.025),2),
    fold_high      = round(quantile(fold_draws, 0.975),2),
    p_at_min_mean   =round( mean(p_min_draws),4),
    p_at_min_median = round(quantile(p_min_draws, 0.5),4),
    p_at_min_low    = round(quantile(p_min_draws, 0.025),4),
    p_at_min_high   = round(quantile(p_min_draws, 0.975),4),
    p_at_max_mean   = round(mean(p_max_draws),4),
    p_at_max_median = round(quantile(p_max_draws, 0.5),4),
    p_at_max_low    = round(quantile(p_max_draws, 0.025),4),
    p_at_max_high   = round(quantile(p_max_draws, 0.975),4)
  )
}

saveRDS(fx_fold_prob, here("analysis",  "functions", "fx_fold_prob.rds"))


# inverse logit function
fx_inverse_logit <- function(x) {
  p <- 1 / (1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p
}

saveRDS(fx_inverse_logit, here("analysis",  "functions", "fx_inverse_logit.rds"))

