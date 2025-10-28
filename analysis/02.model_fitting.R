# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## MODEL FITTING ===============================================================

# Description:
#     Fit models:
#     - Ae. aegypti surveillance using a case-contact sampling strategy vs.
#     broader surveillance (household-level models):
#       - Association between surveillance strategy and Ae. aegypti abundance.
#       - Association between surveillance strategy and probability
#       of DENV detection in Ae. aegypti.
#     - Association between Ae. aegypti abundance and probability of DENV detection
#     (household-level models and area-level models).
#     - Association between entomological metrics and DENV incidence in humans
#     (area-level models).

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

# ---- 1. Data for model fitting -----------------------------------------------

# ---- 1.1 Read in data ----

# Read in the MoH polygons
sf.moh <- st_as_sf(readRDS( here("analysis", "data", "raw_data", "sf.moh.rds")))

# Read in the Iquitos city polygon
sf.city.poly <- readRDS(here("analysis", "data", "derived_data","household_level_data",
                             "sf.city.poly.rds"))

# Import household-level data
d.m <- readRDS(here("analysis", "data", "derived_data", "household_level_data",
                    "d.m.rds"))

# Import area-level data

m.area.week.lag <- readRDS(here("analysis", "data", "derived_data",
                                "area_level_data","m.area.week.lag.rds"))
m.area.week.lag.4 <- readRDS(here("analysis", "data", "derived_data",
                                  "area_level_data","m.area.week.lag.4.rds"))

m.area.week.lag.4.old <- readRDS(here("analysis", "data", "derived_data",
                                  "area_level_data","m.area.week.lag.4.old.rds"))

h.area.week.lag <- readRDS(here("analysis", "data", "derived_data",
                                "area_level_data","h.area.week.lag.rds"))
h.area.week.lag.4 <- readRDS(here("analysis", "data", "derived_data",
                                  "area_level_data","h.area.week.lag.4.rds"))

# ---- 1.2 INLA spatial autocorrelation preparation ----

# Inverse logit function
# Used to transform linear predictors to probabilities.

inverse_logit <- function (x){
  p <- 1 / (1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p
}

# ---- 1.2.1 INLA mesh ----

# Combine coordinates from dataset
coo.d.m <- cbind(d.m$longitude, d.m$latitude)

# Define mesh parameters
# max.edge determines triangle resolution (smaller = finer mesh)
max.edge <- diff(range(d.m$longitude)) / 15

# Outer boundary extension size
bound.outer <- diff(range(d.m$longitude)) / 3

# Define boundary using non-convex hull around the study area
bnd <- inla.nonconvex.hull(
  st_coordinates(sf.city.poly)[, 1:2],
  concave = -0.05,
  convex = -0.05,
  resolution = c(100, 100)
)

# Build the mesh
mesh1 <- inla.mesh.2d(
  loc = coo.d.m,
  boundary = inla.nonconvex.hull(
    as.matrix(coo.d.m),
    concave = -0.05,
    convex = -0.05,
    resolution = c(100, 100)
  ),
  max.edge = c(1, 5) * max.edge,
  offset = c(max.edge, diff(range(d.m$longitude)) / 5),
  cutoff = max.edge / 2
)

# Plot mesh and data points
plot(mesh1, asp = 1, main = "")
points(coo.d.m, pch = 16, col = "red", cex = 0.3, lwd = 1)

# Save mesh
mesh.d.m <- mesh1

# ---- 1.2.2 SPDE and A matrix for estimation ----

# Define the SPDE model using PC priors
spde.d.m <- inla.spde2.pcmatern(
  mesh = mesh.d.m,
  alpha = 2,
  prior.range = c(10, 0.01),  # P(range < 10) = 0.01
  prior.sigma = c(3, 0.01)    # P(sigma > 3) = 0.01
)

# Create index for spatial random field
index.d.m.s <- inla.spde.make.index("s", spde.d.m$n.spde)

# Construct A matrix for estimation locations
A.d.m.est <- inla.spde.make.A(
  mesh = mesh.d.m,
  loc = coo.d.m
)

# ---- 1.2.3 A matrix for prediction ----

# Prediction coordinates (e.g., grid or household points)
coo.d.m.p <- cbind(coop$x, coop$y)

# Construct A matrix for prediction locations
A.d.m.p <- inla.spde.make.A(
  mesh = mesh.d.m,
  loc = coo.d.m.p
)

# ---- 2. Ae. aegypti surveillance using a case-contact sampling strategy vs. broader surveillance -----

# ---- 2.1  Negative binomial models: ----
# Association between Ae. aegypti surveillance strategy and abundance ----

# ---- 2.1.1  Negative binomial model formulas ----

# Negative binomial - Baseline intercept-only model
m.s.nb.0.base.formula <- aa_female ~ 1

# Negative binomial with sampling strategy type and no spatial autocorrelation
m.s.nb.1.base.formula <- aa_female ~ 1 + pos.case.contact.f

# Negative binomial with sampling strategy type and with spatial autocorrelation
m.s.nb.1.formula <- y ~ 0 + b0 + pos.case.contact.f1 + f(s, model = spde.d.m)

# ---- 2.1.2  Negative binomial model stack ----

# Create model matrix for the categorical variable `pos.case.contact.f`
# This encodes the factor levels without an intercept (i.e., each level as its own indicator variable)
# Used later as fixed effects in the INLA model
m.s.nb.1.matrix <- model.matrix(~ 0 + pos.case.contact.f, data = d.m)

# Create INLA stack for estimation

# Construct the INLA data stack (`stk.m.s.nb.1.e`) used for model estimation
# This stack includes:
# - The response variable: number of female Ae. aegypti adults (`aa_female`)
# - A design matrix for fixed effects (including intercept and factor levels
# from `pos.case.contact.f`)
# - The spatial random effect, linked via the projection matrix A

stk.m.s.nb.1.e <- inla.stack(
  tag = "est",                         # Label for this stack component
  data = list(y = d.m$aa_female),      # Response variable
  A = list(1, A.d.m.est),              # List of projection matrices:
  #  - 1 for fixed effects (identity matrix)
  #  - A.d.m.est for spatial random effects
  effects = list(
    data.frame(
      b0 = rep(1, nrow(d.m)),   # Intercept term (manually added)
      m.s.nb.1.matrix           # Fixed effect: levels of `pos.case.contact.f`
    ),
    s = index.d.m.s             # Spatial effect index created from SPDE model
  )
)

# ---- 2.1.3  Negative binomial model fitting ----

# Negative binomial - Baseline intercept-only model
m.s.nb.base.0 <- inla(m.s.nb.0.base.formula,
                      family= "nbinomial",
                      control.family=list(link='log'),
                      data = d.m,
                      control.predictor = list(
                        compute = TRUE, link = 1
                      ),
                      control.compute=list(config=T, waic= TRUE, cpo=TRUE)
)

# Negative binomial with sampling strategy type and no spatial autocorrelation
m.s.nb.base.1 <- inla(m.s.nb.1.base.formula,
                      family= "nbinomial",
                      control.family=list(link='log'),
                      data = d.m,
                      control.predictor = list(
                        compute = TRUE, link = 1
                      ),
                      control.compute=list(config=T, waic= TRUE, cpo=TRUE)
)

# Negative binomial with sampling strategy type and with spatial autocorrelation
m.s.nb.1 <- inla(m.s.nb.1.formula,
                 family= "nbinomial",
                 control.family=list(link='log'),
                 data = inla.stack.data(stk.m.s.nb.1.e),
                 control.predictor = list(
                   compute = TRUE, link = 1,
                   A = inla.stack.A(stk.m.s.nb.1.e)
                 ),
                 control.compute=list(config=T, waic= TRUE, cpo=TRUE)
)


# Save model outputs as .rds files
saveRDS(m.s.nb.base.0, here("analysis", "outputs", "models", "m.s.nb.base.0.rds"))
saveRDS(m.s.nb.base.1, here("analysis", "outputs", "models", "m.s.nb.base.1.rds"))
saveRDS(m.s.nb.1, here("analysis", "outputs", "models", "m.s.nb.1.rds"))


# ---- 2.2  Logistic models: ----
# Association between the surveillance strategy and probability of DENV detection ----

# ---- 2.2.1  Logistic model formulas ----

# Bernouilli Baseline intercept-only model
m.s.b.0.base.formula <- denv.house.num ~ 1

# Bernouilli with sampling strategy type and no spatial autocorrelation
m.s.b.1.base.formula <- denv.house.num ~ 1+ pos.case.contact.f

# Bernouilli with sampling strategy type and with spatial autocorrelation
m.s.b.1.formula <- y ~ 0 + b0 + pos.case.contact.f1 + f(s, model = spde.d.m)

# ---- 2.2.2  Logistic model stack ----

# Create model matrix for surveillance strategy
m.s.b.1.matrix <- model.matrix(~ 0 + pos.case.contact.f, data = d.m)

# Create INLA stack for model estimation
stk.m.s.b.1.e <- inla.stack(
  tag = "est",
  data = list(y = d.m$denv.house.num),  # outcome: DENV-infected mosquitoes (binary)
  A = list(1, A.d.m.est),             # List of projection matrices:
  #  - 1 for fixed effects (identity matrix)
  #  - A.d.m.est for spatial random effects
  effects = list(
    data.frame(b0 = rep(1, nrow(d.m)),  # intercept
               m.s.b.1.matrix),         # surveillance covariates
    s = index.d.m.s                     # spatial random effect index
  )
)

# ---- 2.2.3  Logistic model fitting ----

# Bernouilli Baseline intercept-only model
m.s.b.base.0 <- inla(m.s.b.0.base.formula,
                     family = "binomial", Ntrials = 1,
                     control.family = list(link = "logit"),
                     data = d.m,
                     control.predictor = list(
                       compute = TRUE, link = 1
                     ),
                     control.compute=list(config=T, waic= TRUE, cpo=TRUE)
)

# Bernouilli with sampling strategy type and no spatial autocorrelation
m.s.b.base.1 <- inla(m.s.b.1.base.formula,
                     family = "binomial", Ntrials = 1,
                     control.family = list(link = "logit"),
                     data = d.m,
                     control.predictor = list(
                       compute = TRUE, link = 1
                     ),
                     control.compute=list(config=T, waic= TRUE, cpo=TRUE)
)

# Bernouilli with sampling strategy type and with spatial autocorrelation
m.s.b.1 <- inla(m.s.b.1.formula,
                family = "binomial", Ntrials = 1,
                control.family = list(link = "logit"),
                data = inla.stack.data(stk.m.s.b.1.e),
                control.predictor = list(
                  compute = TRUE, link = 1,
                  A = inla.stack.A(stk.m.s.b.1.e)
                ),
                control.compute=list(config=T, waic= TRUE, cpo=TRUE)
)

# Save model outputs as .rds files
saveRDS(m.s.b.base.0, here("analysis", "outputs", "models", "m.s.b.base.0.rds"))
saveRDS(m.s.b.base.1, here("analysis", "outputs", "models", "m.s.b.base.1.rds"))
saveRDS(m.s.b.1, here("analysis", "outputs", "models", "m.s.b.1.rds"))


# ---- 3. Association between Ae. aegypti abundance and probability of DENV detection ----

# ---- 3.1 Household-level logistic model ----

# ---- 3.1.1 Household-level logistic model formulas ----

# Intercept-only logistic regression (baseline model)
m.b.0.base.formula <- denv.house.num ~ 1

# Logistic regression with Ae. aegypti female abundance (aa_female) as predictor
m.b.1.base.formula <- denv.house.num ~ 1 + aa_female

# Spatial logistic regression with intercept and spatial random field
m.b.0.formula <- y ~ 0 + b0 + f(s, model = spde.d.m)

# Spatial logistic regression with abundance + spatial random field
m.b.1.formula <- y ~ 0 + b0 + aa_female + f(s, model = spde.d.m)

# Spatial logistic regression with abundance + month as AR1 effect + spatial field
m.b.2.formula <- y ~ 0 + b0 + aa_female + f(month, model = "ar1") + f(s, model = spde.d.m)


# ---- 3.1.2 Household-level logistic model INLA stacks for estimation ----

# Stack for spatial-only model (intercept + spatial field)
stk.m.b.0.e <- inla.stack(
  tag = "est",
  data = list(y = d.m$denv.house.num),
  A = list(1, A.d.m.est),
  effects = list(data.frame(b0 = rep(1, nrow(d.m))), s = index.d.m.s)
)

# Stack for model with abundance and spatial effect
stk.m.b.1.e <- inla.stack(
  tag = "est",
  data = list(y = d.m$denv.house.num),
  A = list(1, A.d.m.est),
  effects = list(data.frame(
    b0 = rep(1, nrow(d.m)),
    aa_female = d.m$aa_female),
    s = index.d.m.s)
)

# Stack for full model with abundance, temporal effect (AR1), and spatial effect
stk.m.b.2.e <- inla.stack(
  tag = "est",
  data = list(y = d.m$denv.house.num),
  A = list(1, A.d.m.est),
  effects = list(data.frame(
    b0 = rep(1, nrow(d.m)),
    aa_female = d.m$aa_female,
    month = d.m$month.t),
    s = index.d.m.s)
)

# ---- 3.1.3 Household-level logistic model fitting ----

# Baseline model: intercept only
m.b.base.0 <- inla(
  m.b.0.base.formula,
  family = "binomial", Ntrials = 1,
  control.family = list(link = "logit"),
  data = d.m,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(config = TRUE, waic = TRUE, cpo = TRUE)
)

# Abundance and no spatial component
m.b.base.1 <- inla(
  m.b.1.base.formula,
  family = "binomial", Ntrials = 1,
  control.family = list(link = "logit"),
  data = d.m,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(config = TRUE, waic = TRUE, cpo = TRUE)
)

# Intercept + spatial random field
m.b.0 <- inla(
  m.b.0.formula,
  family = "binomial", Ntrials = 1,
  control.family = list(link = "logit"),
  data = inla.stack.data(stk.m.b.0.e),
  control.predictor = list(
    compute = TRUE, link = 1,
    A = inla.stack.A(stk.m.b.0.e)
  ),
  control.compute = list(config = TRUE, waic = TRUE, cpo = TRUE)
)

# Abundance + spatial field
m.b.1 <- inla(
  m.b.1.formula,
  family = "binomial", Ntrials = 1,
  control.family = list(link = "logit"),
  data = inla.stack.data(stk.m.b.1.e),
  control.predictor = list(
    compute = TRUE, link = 1,
    A = inla.stack.A(stk.m.b.1.e)
  ),
  control.compute = list(config = TRUE, waic = TRUE, cpo = TRUE)
)

# Abundance + temporal AR1 effect + spatial field
m.b.2 <- inla(
  m.b.2.formula,
  family = "binomial", Ntrials = 1,
  control.family = list(link = "logit"),
  data = inla.stack.data(stk.m.b.2.e),
  control.predictor = list(
    compute = TRUE, link = 1,
    A = inla.stack.A(stk.m.b.2.e)
  ),
  control.compute = list(config = TRUE, waic = TRUE, cpo = TRUE)
)


# Save model outputs as .rds files
saveRDS(m.b.base.0, here("analysis", "outputs", "models", "m.b.base.0.rds"))
saveRDS(m.b.base.1, here("analysis", "outputs", "models", "m.b.base.1.rds"))
saveRDS(m.b.0, here("analysis", "outputs", "models", "m.b.0.rds"))
saveRDS(m.b.1, here("analysis", "outputs", "models", "m.b.1.rds"))
saveRDS(m.b.2, here("analysis", "outputs", "models", "m.b.2.rds"))


# ---- 3.2 Area-level logistic models ----

# ---- 3.2.1 Dirichlet-weighted 4-week lags  ----

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

# Stan model specification with Dirichlet-weighted 4-week lags
m.01 <- '
data {
  int<lower=0> N;  // Number of observations
  array[N] int n;  // Number of Ae. aegypti tested in each observation
  int<lower=0> K;  // Number of predictors (lags)
  matrix[N, K] x;  // Predictor matrix of lagged abundance values
  array[N] int y;  // Vector of DENV-positive outcomes
  vector[5] a;     // Dirichlet prior concentration parameters for lag weights
}
parameters {
  real alpha;            // Intercept term
  real beta;             // Coefficient applied to the weighted sum of lagged predictors
  simplex[5] w;          // Simplex: weights for each of the 5 lag predictors
  real<lower=0> sigma;   // Standard deviation for error term
}
model {
  vector[N] p;           // Vector to hold linear predictor values per observation
  alpha ~ normal(0, 1.5);        // Weakly informative prior for intercept
  beta ~ normal(0, 0.5);         // Prior for slope coefficient
  w ~ dirichlet(a);              // Dirichlet prior on lag weights
  sigma ~ exponential(1);        // Exponential prior on sigma

  for (i in 1:N) {
    // Compute linear predictor as weighted sum of lagged predictors, scaled by beta
    p[i] = alpha + (x[i,1]*w[1] + x[i,2]*w[2] + x[i,3]*w[3] + x[i,4]*w[4] + x[i,5]*w[5]) * beta;
    p[i] = inv_logit(p[i]);  // Apply logistic link function to get predicted probability
  }

  y ~ binomial(n, p);  // Likelihood: binomial model for number of DENV-positive out of n tested
}
generated quantities {
  vector[N] log_lik;    // Log-likelihood values for model comparison
  vector[N] p;          // Posterior predicted probabilities

  for (i in 1:N) {
    // Recompute probabilities using posterior samples
    p[i] = alpha + (x[i,1]*w[1] + x[i,2]*w[2] + x[i,3]*w[3] + x[i,4]*w[4] + x[i,5]*w[5]) * beta;
    p[i] = inv_logit(p[i]);
  }

  for (i in 1:N) {
    // Compute log-likelihood for each observation
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
  }
}
'

# Fit m.01: Dirichlet-weighted lag model using complete 4-week lag dataset
fit.m.01 <- rstan::stan(
  model_code = m.01,           # Stan model code as a string
  data = dat.m.01,             # Input data list for Stan
  iter = 2000,                 # Total number of iterations per chain
  chains = 4,                  # Number of MCMC chains
  control = list(adapt_delta = 0.99)  # Tuning parameter
)


# ---- 3.2.2 Effect of each weekly- lagged measure independently  ----

# ---- 3.2.2.0 week lag 0 ----
# Effect of the current week (lag 0)

# Data frame with observations for week lag 0
m.week <- m.area.week.lag %>%
  filter(!is.na(week_lag_0))

# Number of observations
nobsv.2.0 = nrow(m.week)

# Prepare input list for Stan model
dat.2.0 <- list(N = nobsv.2.0,
                n = m.week$n.tested,
                x = m.week$week_lag_0,
                y = m.week$n.denv)

# Model for the week lag 0 effect
m.2.0 <- '
data {
  int<lower=0> N;               // Number of observations
  array[N] int n;               // Number of Ae. aegypti tested
  array[N] real x;              // Predictor (lagged Ae. ae. abundance)
  array[N] int y;               // Outcome: number of DENV detections
}
parameters {
  real alpha;                   // Intercept term
  real beta;                    // Coefficient
  real<lower=0> sigma;          // Error scale
}
model {
  vector[N] p;                  // Linear predictor for binomial probability

  // Priors
  alpha ~ normal(0, 1.5);       // Weakly informative prior for intercept
  beta ~ normal(0, 0.5);        // Prior for abundance effect
  sigma ~ exponential(1);       // Prior on error scale (not used directly)

  // Likelihood
  for (i in 1:N) {
    p[i] = alpha + beta * x[i]; // Linear effect of lag 0 abundance
    p[i] = inv_logit(p[i]);     // Convert to probability via logistic function
  }
  y ~ binomial(n, p);           // Binomial likelihood
}
generated quantities {
  vector[N] log_lik;            // Log-likelihood values for model comparison
  vector[N] p;                  // Posterior predicted probabilities

  // Recalculate predicted probabilities
  for (i in 1:N) {
    p[i] = alpha + beta * x[i];
    p[i] = inv_logit(p[i]);
  }

  // Compute log-likelihood for each observation
  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
}
'

# ---- 3.2.2.1 week lag 1 ----
# Effect of Ae. ae. abundance at a 1 week lag on probability of DENV detection

# Data frame with observations for week lag 1
m.week <- m.area.week.lag %>%
  filter(!is.na(week_lag_1))

# Number of observations
nobsv.2.1 = nrow(m.week)

# Prepare input list for Stan model
dat.2.1 <- list(N = nobsv.2.1,
                n = m.week$n.tested,
                x = m.week$week_lag_1,
                y = m.week$n.denv)

# Model for the week lag 1 effect
m.2.1 <- '
data {
  int<lower=0> N;               // Number of observations
  array[N] int n;               // Number of Ae. aegypti tested
  array[N] real x;              // Predictor (lagged Ae. ae. abundance)
  array[N] int y;               // Outcome: number of DENV detections
}
parameters {
  real alpha;                   // Intercept term
  real beta;                    // Coefficient
  real<lower=0> sigma;          // Error scale
}
model {
  vector[N] p;                  // Linear predictor for binomial probability

  // Priors
  alpha ~ normal(0, 1.5);       // Weakly informative prior for intercept
  beta ~ normal(0, 0.5);        // Prior for abundance effect
  sigma ~ exponential(1);       // Prior on error scale (not used directly)

  // Likelihood
  for (i in 1:N) {
    p[i] = alpha + beta * x[i]; // Linear effect of lag 0 abundance
    p[i] = inv_logit(p[i]);     // Convert to probability via logistic function
  }
  y ~ binomial(n, p);           // Binomial likelihood
}
generated quantities {
  vector[N] log_lik;            // Log-likelihood values for model comparison
  vector[N] p;                  // Posterior predicted probabilities

  // Recalculate predicted probabilities
  for (i in 1:N) {
    p[i] = alpha + beta * x[i];
    p[i] = inv_logit(p[i]);
  }

  // Compute log-likelihood for each observation
  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
}
'

# ---- 3.2.2.2 week lag 2 ----
# Effect of Ae. ae. abundance at a 2 week lag on probability of DENV detection

m.week <- m.area.week.lag %>%
  filter(!is.na(week_lag_2))

nobsv.2.2 = nrow(m.week)

dat.2.2 <- list(N = nobsv.2.2,
                n = m.week$n.tested,
                x = m.week$week_lag_2,
                y = m.week$n.denv)

# Model for the week lag 2 effect
m.2.2 <- '
data {
  int<lower=0> N;               // Number of observations
  array[N] int n;               // Number of Ae. aegypti tested
  array[N] real x;              // Predictor (lagged Ae. ae. abundance)
  array[N] int y;               // Outcome: number of DENV detections
}
parameters {
  real alpha;                   // Intercept term
  real beta;                    // Coefficient
  real<lower=0> sigma;          // Error scale
}
model {
  vector[N] p;                  // Linear predictor for binomial probability

  // Priors
  alpha ~ normal(0, 1.5);       // Weakly informative prior for intercept
  beta ~ normal(0, 0.5);        // Prior for abundance effect
  sigma ~ exponential(1);       // Prior on error scale (not used directly)

  // Likelihood
  for (i in 1:N) {
    p[i] = alpha + beta * x[i]; // Linear effect of lag 0 abundance
    p[i] = inv_logit(p[i]);     // Convert to probability via logistic function
  }
  y ~ binomial(n, p);           // Binomial likelihood
}
generated quantities {
  vector[N] log_lik;            // Log-likelihood values for model comparison
  vector[N] p;                  // Posterior predicted probabilities

  // Recalculate predicted probabilities
  for (i in 1:N) {
    p[i] = alpha + beta * x[i];
    p[i] = inv_logit(p[i]);
  }

  // Compute log-likelihood for each observation
  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
}
'

# ---- 3.2.2.3 week lag 3 ----
# Effect of Ae. ae. abundance at a 3 week lag on probability of DENV detection

m.week <- m.area.week.lag %>%
  filter(!is.na(week_lag_3))

nobsv.2.3 = nrow(m.week)

dat.2.3 <- list(N = nobsv.2.3,
                n = m.week$n.tested,
                x = m.week$week_lag_3,
                y = m.week$n.denv)

# Model for the week lag 3 effect
m.2.3 <- '
data {
  int<lower=0> N;               // Number of observations
  array[N] int n;               // Number of Ae. aegypti tested
  array[N] real x;              // Predictor (lagged Ae. ae. abundance)
  array[N] int y;               // Outcome: number of DENV detections
}
parameters {
  real alpha;                   // Intercept term
  real beta;                    // Coefficient
  real<lower=0> sigma;          // Error scale
}
model {
  vector[N] p;                  // Linear predictor for binomial probability

  // Priors
  alpha ~ normal(0, 1.5);       // Weakly informative prior for intercept
  beta ~ normal(0, 0.5);        // Prior for abundance effect
  sigma ~ exponential(1);       // Prior on error scale (not used directly)

  // Likelihood
  for (i in 1:N) {
    p[i] = alpha + beta * x[i]; // Linear effect of lag 0 abundance
    p[i] = inv_logit(p[i]);     // Convert to probability via logistic function
  }
  y ~ binomial(n, p);           // Binomial likelihood
}
generated quantities {
  vector[N] log_lik;            // Log-likelihood values for model comparison
  vector[N] p;                  // Posterior predicted probabilities

  // Recalculate predicted probabilities
  for (i in 1:N) {
    p[i] = alpha + beta * x[i];
    p[i] = inv_logit(p[i]);
  }

  // Compute log-likelihood for each observation
  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
}
'

# ---- 3.2.2.4 week lag 4 ----
# Effect of Ae. ae. abundance at a 4 week lag on probability of DENV detection

m.week <- m.area.week.lag %>%
  filter(!is.na(week_lag_4))

nobsv.2.4 = nrow(m.week)

dat.2.4 <- list(N = nobsv.2.4,
                n = m.week$n.tested,
                x = m.week$week_lag_4,
                y = m.week$n.denv)

# Model the week lag 4 effect
m.2.4 <- '
data {
  int<lower=0> N;               // Number of observations
  array[N] int n;               // Number of Ae. aegypti tested
  array[N] real x;              // Predictor (lagged Ae. ae. abundance)
  array[N] int y;               // Outcome: number of DENV detections
}
parameters {
  real alpha;                   // Intercept term
  real beta;                    // Coefficient
  real<lower=0> sigma;          // Error scale
}
model {
  vector[N] p;                  // Linear predictor for binomial probability

  // Priors
  alpha ~ normal(0, 1.5);       // Weakly informative prior for intercept
  beta ~ normal(0, 0.5);        // Prior for abundance effect
  sigma ~ exponential(1);       // Prior on error scale (not used directly)

  // Likelihood
  for (i in 1:N) {
    p[i] = alpha + beta * x[i]; // Linear effect of lag 0 abundance
    p[i] = inv_logit(p[i]);     // Convert to probability via logistic function
  }
  y ~ binomial(n, p);           // Binomial likelihood
}
generated quantities {
  vector[N] log_lik;            // Log-likelihood values for model comparison
  vector[N] p;                  // Posterior predicted probabilities

  // Recalculate predicted probabilities
  for (i in 1:N) {
    p[i] = alpha + beta * x[i];
    p[i] = inv_logit(p[i]);
  }

  // Compute log-likelihood for each observation
  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
}
'

# ---- 3.2.2.5 week lag 5 ----
# Effect of Ae. ae. abundance at a 5 week lag on probability of DENV detection

m.week <- m.area.week.lag %>%
  filter(!is.na(week_lag_5))

nobsv.2.5 = nrow(m.week)

dat.2.5 <- list(N = nobsv.2.5,
                n = m.week$n.tested,
                x = m.week$week_lag_5,
                y = m.week$n.denv)

# Model the week lag 5 effect
m.2.5 <- '
data {
  int<lower=0> N;               // Number of observations
  array[N] int n;               // Number of Ae. aegypti tested
  array[N] real x;              // Predictor (lagged Ae. ae. abundance)
  array[N] int y;               // Outcome: number of DENV detections
}
parameters {
  real alpha;                   // Intercept term
  real beta;                    // Coefficient
  real<lower=0> sigma;          // Error scale
}
model {
  vector[N] p;                  // Linear predictor for binomial probability

  // Priors
  alpha ~ normal(0, 1.5);       // Weakly informative prior for intercept
  beta ~ normal(0, 0.5);        // Prior for abundance effect
  sigma ~ exponential(1);       // Prior on error scale (not used directly)

  // Likelihood
  for (i in 1:N) {
    p[i] = alpha + beta * x[i]; // Linear effect of lag 0 abundance
    p[i] = inv_logit(p[i]);     // Convert to probability via logistic function
  }
  y ~ binomial(n, p);           // Binomial likelihood
}
generated quantities {
  vector[N] log_lik;            // Log-likelihood values for model comparison
  vector[N] p;                  // Posterior predicted probabilities

  // Recalculate predicted probabilities
  for (i in 1:N) {
    p[i] = alpha + beta * x[i];
    p[i] = inv_logit(p[i]);
  }

  // Compute log-likelihood for each observation
  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
}
'

# ---- 3.2.2.6 week lag 6 ----
# Effect of Ae. ae. abundance at a 6 week lag on probability of DENV detection

m.week <- m.area.week.lag %>%
  filter(!is.na(week_lag_6))

nobsv.2.6 = nrow(m.week)
dat.2.6 <- list(N = nobsv.2.6,
                n = m.week$n.tested,
                x = m.week$week_lag_6,
                y = m.week$n.denv)

# Model the week lag 6 effect
m.2.6 <- '
data {
  int<lower=0> N;               // Number of observations
  array[N] int n;               // Number of Ae. aegypti tested
  array[N] real x;              // Predictor (lagged Ae. ae. abundance)
  array[N] int y;               // Outcome: number of DENV detections
}
parameters {
  real alpha;                   // Intercept term
  real beta;                    // Coefficient
  real<lower=0> sigma;          // Error scale
}
model {
  vector[N] p;                  // Linear predictor for binomial probability

  // Priors
  alpha ~ normal(0, 1.5);       // Weakly informative prior for intercept
  beta ~ normal(0, 0.5);        // Prior for abundance effect
  sigma ~ exponential(1);       // Prior on error scale (not used directly)

  // Likelihood
  for (i in 1:N) {
    p[i] = alpha + beta * x[i]; // Linear effect of lag 0 abundance
    p[i] = inv_logit(p[i]);     // Convert to probability via logistic function
  }
  y ~ binomial(n, p);           // Binomial likelihood
}
generated quantities {
  vector[N] log_lik;            // Log-likelihood values for model comparison
  vector[N] p;                  // Posterior predicted probabilities

  // Recalculate predicted probabilities
  for (i in 1:N) {
    p[i] = alpha + beta * x[i];
    p[i] = inv_logit(p[i]);
  }

  // Compute log-likelihood for each observation
  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
}
'

# ---- 3.2.2.7 Fit models with each weekly- lagged measure ----

fit.m.2.0  <- rstan::stan(model_code = m.2.0, data = dat.2.0, iter = 2000,
                          chains = 4, control=list(adapt_delta=0.99))

fit.m.2.1  <- rstan::stan(model_code = m.2.1, data = dat.2.1, iter = 2000,
                          chains = 4, control=list(adapt_delta=0.99))

fit.m.2.2  <- rstan::stan(model_code = m.2.2, data = dat.2.2, iter = 2000,
                          chains = 4, control=list(adapt_delta=0.99))

fit.m.2.3  <- rstan::stan(model_code = m.2.3, data = dat.2.3, iter = 2000,
                          chains = 4, control=list(adapt_delta=0.99))

fit.m.2.4  <- rstan::stan(model_code = m.2.4, data = dat.2.4, iter = 2000,
                          chains = 4, control=list(adapt_delta=0.99))

fit.m.2.5  <- rstan::stan(model_code = m.2.5, data = dat.2.5, iter = 2000,
                          chains = 4, control=list(adapt_delta=0.99))

fit.m.2.6  <- rstan::stan(model_code = m.2.6, data = dat.2.6, iter = 2000,
                          chains = 4, control=list(adapt_delta=0.99))


# ----  3.3 Compile and save models with Ae.aegypti prevalence outcome ----

# Create a list of fitted models including:
# - fit.m.01: model with Dirichlet-weighted lag structure
# - fit.m.2.x: models with individual lags from week 0 to week 6
m.lag.list <- list(fit.m.01,
                   fit.m.2.0, fit.m.2.1, fit.m.2.2, fit.m.2.3,
                   fit.m.2.4, fit.m.2.5, fit.m.2.6)

# Create a data frame to index the models with corresponding labels
m.lag.list.index <- data.frame(
  model.name = c("m.01",
                 "m.2.0", "m.2.1", "m.2.2", "m.2.3", "m.2.4", "m.2.5", "m.2.6"),
  index = seq(1, length(m.lag.list), by = 1)
)

# Save list of models and index file to disk
saveRDS(m.lag.list, here("analysis", "outputs", "models",
                         "m.lag.list.rds"))
saveRDS(m.lag.list.index, here("analysis", "outputs", "models",
                               "m.lag.list.index.rds"))


# ---- 4. Association between entomological metrics and DENV incidence in humans ----

# ---- 4.1 Dirichlet-weighted 4-week lags  ----

# ---- 4.1.1 Ae. aegypti DENV prevalence  ----

# Load the dataset with Ae. aegypti prevalence over 0–4 week lags
h.week <- h.area.week.lag.4

# Get the number of weekly observations
nobsv = nrow(h.week)

# Prepare input list for the Stan model with Dirichlet-weighted lag structure
dat.01 <- list(
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

# Stan model to estimate lag-weighted effect of mosquito prevalence on DENV incidence
h.01 <- '
data {
  int<lower=0> N;             // Number of observations
  array[N] int n;             // Number of individuals under surveillance
  int<lower=0> K;             // Number of lagged predictors (K = 5)
  matrix[N, K] x;             // Predictor matrix: lagged Ae. aegypti prevalence
  array[N] int y;             // Outcome: DENV incidence in humans
  vector[5] a;                // Dirichlet prior parameters for lag weights
}
parameters {
  real alpha;                 // Intercept
  real beta;                  // Overall effect of weighted Ae. aegypti prevalence
  simplex[5] w;               // Weights for each lag (constrained to sum to 1)
  real<lower=0> sigma;        // Error scale
}
model {
  vector[N] p;                // Linear predictor

  // Priors
  alpha ~ normal(0, 0.5);     // Prior for intercept
  beta ~ normal(0, 0.2);      // Prior for overall abundance effect
  w ~ dirichlet(a);           // Dirichlet prior on lag weights
  sigma ~ exponential(1);     // Prior on error scale

  // Likelihood
  for (i in 1:N) {
    p[i] = alpha + (x[i,1]*w[1] + x[i,2]*w[2] + x[i,3]*w[3] + x[i,4]*w[4] + x[i,5]*w[5]) * beta;
    p[i] = inv_logit(p[i]);   // Logistic transformation to get binomial probability
  }
  y ~ binomial(n, p);         // Binomial likelihood
}
generated quantities {
  vector[N] log_lik;          // Log-likelihood for each observation
  vector[N] p;                // Posterior predicted probabilities

  // Recalculate predicted probabilities from posterior
  for (i in 1:N) {
    p[i] = alpha + (x[i,1]*w[1] + x[i,2]*w[2] + x[i,3]*w[3] + x[i,4]*w[4] + x[i,5]*w[5]) * beta;
    p[i] = inv_logit(p[i]);
  }

  // Compute log-likelihood values
  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);  // Log-probability of observed data
}
'

# ---- 4.1.2 Average Ae. aegypti female abundance ----

# Load the dataset containing 0–4 week lags of average Ae. aegypti female abundance
h.week <- h.area.week.lag.4

# Store the number of weekly observations
nobsv = nrow(h.week)

dat.02 <- list(
  N = nobsv,                    # Number of observations
  n = h.week$n.ind,             # Number of individuals under surveillance
  K = 5,                        # Number of lagged predictors (0–4 weeks)
  x = cbind(                    # Combine 5 lagged predictor variables into matrix
    h.week$avg.aa.f.week_lag_0,
    h.week$avg.aa.f.week_lag_1,
    h.week$avg.aa.f.week_lag_2,
    h.week$avg.aa.f.week_lag_3,
    h.week$avg.aa.f.week_lag_4
  ),
  y = h.week$n.denv,            # Weekly count of DENV-positive individuals
  a = rep(2, 5)                 # Dirichlet prior concentration parameters
)

# Stan model to estimate effect of average Ae. aegypti female abundance over 5 lagged weeks
h.02 <- '
data {
  int<lower=0> N;                // Number of observations
  array[N] int n;                // Number of individuals under surveillance
  int<lower=0> K;                // Number of lagged predictors (weeks 0–4)
  matrix[N, K] x;                // Predictor matrix of average Ae. aegypti abundance
  array[N] int y;                // Outcome vector: DENV incidence
  vector[5] a;                   // Dirichlet prior parameters for lag weights
}
parameters {
  real alpha;                    // Intercept term
  real beta;                     // Coefficient for weighted lag predictor
  simplex[5] w;                  // Lag weights (constrained to sum to 1)
  real<lower=0> sigma;           // Error scale (not directly used here but declared)
}
model {
  vector[N] p;                   // Linear predictor

  // Priors
  alpha ~ normal(0, 1.5);        // Weakly informative prior for intercept
  beta ~ normal(0, 0.5);         // Prior for total effect of average Ae. aegypti abundance
  w ~ dirichlet(a);              // Dirichlet prior on lag weights
  sigma ~ exponential(1);        // Prior on scale (unused)

  // Linear model and likelihood
  for (i in 1:N) {
    // Weighted sum of lagged predictors, scaled by beta
    p[i] = alpha + (x[i,1]*w[1] + x[i,2]*w[2] + x[i,3]*w[3] + x[i,4]*w[4] + x[i,5]*w[5]) * beta;
    p[i] = inv_logit(p[i]);      // Logistic transformation for probability
  }
  y ~ binomial(n, p);            // Binomial likelihood: DENV-positive out of n individuals
}
generated quantities {
  vector[N] log_lik;             // Log-likelihood for model comparison
  vector[N] p;                   // Posterior predicted probabilities

  // Recompute posterior predictions
  for (i in 1:N) {
    p[i] = alpha + (x[i,1]*w[1] + x[i,2]*w[2] + x[i,3]*w[3] + x[i,4]*w[4] + x[i,5]*w[5]) * beta;
    p[i] = inv_logit(p[i]);
  }

  // Calculate log-likelihood for each observation
  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
}
'


# ---- 4.1.3 Vector index ----

# Load the human surveillance dataset with 0–4 week lags
h.week <- h.area.week.lag.4

# Get the number of weekly observations
nobsv = nrow(h.week)

# Prepare input list for the Stan model with Dirichlet-weighted lag structure
dat.03 <- list(
  N = nobsv,                    # Number of observations
  n = h.week$n.ind,             # Number of individuals under surveillance
  K = 5,                        # Number of lagged predictors (0–4 weeks)
  x = cbind(                    # Combine 5 lagged predictor variables into matrix
    h.week$vi.week_lag_0,       # Same-week vector index (VI)
    h.week$vi.week_lag_1,       # 1-week lagged VI
    h.week$vi.week_lag_2,       # 2-week lagged VI
    h.week$vi.week_lag_3,       # 3-week lagged VI
    h.week$vi.week_lag_4        # 4-week lagged VI
  ),
  y = h.week$n.denv,            # Weekly count of DENV-positive individuals
  a = rep(2, 5)                 # Dirichlet prior concentration parameters
)

# Stan model code: relationship between lagged vector index and human DENV cases
h.03 <- '
data {
  int<lower=0> N;               // Number of observations
  array[N] int n;               // Number of individuals under surveillance in each week
  int<lower=0> K;               // Number of lagged predictors (weeks 0–4)
  matrix[N, K] x;               // Predictor matrix: vector index over 5 lags
  array[N] int y;               // Outcome: DENV incidence
  vector[5] a;                  // Dirichlet prior vector for lag weights
}
parameters {
  real alpha;                   // Intercept term
  real beta;                    // Coefficient for total effect
  simplex[5] w;                 // Lag weights (constrained to sum to 1 via Dirichlet prior)
  real<lower=0> sigma;          // Error scale
}
model {
  vector[N] p;                  // Linear predictor

  // Priors
  alpha ~ normal(0, 0.5);       // Prior for intercept
  beta ~ normal(0, 0.2);        // Prior for total effect of vector index
  w ~ dirichlet(a);             // Dirichlet prior for week lag weights
  sigma ~ exponential(1);       // Prior on error scale

  // Likelihood
  for (i in 1:N) {
    // Weighted sum of lagged predictors
    p[i] = alpha + (x[i,1] * w[1] + x[i,2] * w[2] + x[i,3] * w[3] + x[i,4] * w[4] + x[i,5] * w[5]) * beta;
    p[i] = inv_logit(p[i]);     // Logistic transformation for binomial probability
  }
  y ~ binomial(n, p);           // Binomial likelihood: DENV cases out of n individuals
}
generated quantities {
  vector[N] log_lik;            // Log-likelihood for each observation (for model comparison)
  vector[N] p;                  // Posterior predictive probabilities

  for (i in 1:N) {
    // Recalculate predicted probabilities using posterior samples
    p[i] = alpha + (x[i,1] * w[1] + x[i,2] * w[2] + x[i,3] * w[3] + x[i,4] * w[4] + x[i,5] * w[5]) * beta;
    p[i] = inv_logit(p[i]);
  }

  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);  // Compute log-likelihood per observation
}
'

# ---- 4.1.4 Fit Dirichlet-weighted models ----

# Fit models of weighted lagged vector metrics vs. human incidence

fit.h.01  <- rstan::stan(model_code = h.01,
                         data = dat.01,
                         iter = 2000,
                         chains = 4,
                         control=list(adapt_delta=0.99))

fit.h.02  <- rstan::stan(model_code = h.02,
                         data = dat.02 ,
                         iter = 2000,
                         chains = 4,
                         control=list(adapt_delta=0.99))

fit.h.03  <- rstan::stan(model_code = h.03,
                         data = dat.03,
                         iter = 2000,
                         chains = 4,
                         control=list(adapt_delta=0.99))


# ----  4.2 Effect of each weekly-lagged measure of the entomological metrics ----

# ----  4.2.1 Effect of each weekly-lagged Ae. aegypti prevalence value ----

# Effect of Ae. aegypti prevalence (%) at each independent week lag (0 to 6)
# on the probability of DENV detection

# Loop through each lag from 0 to 6
for (lag in 0:6) {

  # Load the human surveillance dataset with specified lag
  h.week <- h.area.week.lag %>%
    filter(!is.na(!!sym(paste0("prev.week_lag_", lag))))

  # Get the number of weekly observations for this lag
  assign(paste0("nobsv.2.1.", lag), nrow(h.week))

  # Prepare input list for the Stan model
  assign(paste0("dat.2.1.", lag), list(
    N = nrow(h.week),                            # Number of observations
    n = h.week$n.ind,                            # Number of individuals under surveillance
    x = h.week[[paste0("prev.week_lag_", lag)]], # Ae. aegypti prevalence at lag
    y = h.week$n.denv                            # Weekly count of DENV-positive individuals
  ))

  # Define Stan model as a string for this lag
  assign(paste0("h.2.1.", lag), '
data {
  int<lower=0> N;               // Number of observations
  array[N] int n;               // Number of individuals under surveillance
  array[N] real x;              // Predictor: Ae. aegypti prevalence at lag
  array[N] int y;               // Outcome: DENV incidence
}
parameters {
  real alpha;                   // Intercept
  real beta;                    // Coefficient for prevalence predictor
  real<lower=0> sigma;          // Error scale
}
model {
  vector[N] p;

  // Priors
  alpha ~ normal(0, 0.5);       // Prior on intercept
  beta ~ normal(0, 0.2);        // Prior on effect of prevalence
  sigma ~ exponential(1);       // Prior on error scale

  // Likelihood
  for (i in 1:N) {
    p[i] = alpha + beta * x[i]; // Linear model
    p[i] = inv_logit(p[i]);     // Logistic transformation
  }
  y ~ binomial(n, p);           // Binomial likelihood
}
generated quantities {
  vector[N] log_lik;            // Log-likelihood for model comparison
  vector[N] p;                  // Posterior predictive probabilities

  for (i in 1:N) {
    p[i] = alpha + beta * x[i];
    p[i] = inv_logit(p[i]);
  }
  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);
}
  ')
}


# Fit models with individual lagged effects from 0 to 6

fit.h.2.1.0  <- rstan::stan(model_code = h.2.1.0,
                            data = dat.2.1.0, iter = 2000,
                            chains = 4,
                            control=list(adapt_delta=0.99))

fit.h.2.1.1  <- rstan::stan(model_code = h.2.1.1,
                            data = dat.2.1.1,
                            iter = 2000, chains = 4,
                            control=list(adapt_delta=0.99))

fit.h.2.1.2  <- rstan::stan(model_code = h.2.1.2,
                            data = dat.2.1.2,
                            iter = 2000,
                            chains = 4,
                            control=list(adapt_delta=0.99))

fit.h.2.1.3  <- rstan::stan(model_code = h.2.1.3,
                            data = dat.2.1.3,
                            iter = 2000,
                            chains = 4,
                            control=list(adapt_delta=0.99))

fit.h.2.1.4  <- rstan::stan(model_code = h.2.1.4,
                            data = dat.2.1.4,
                            iter = 2000,
                            chains = 4,
                            control=list(adapt_delta=0.99))

fit.h.2.1.5  <- rstan::stan(model_code = h.2.1.5,
                            data = dat.2.1.5,
                            iter = 2000,
                            chains = 4,
                            control=list(adapt_delta=0.99))

fit.h.2.1.6  <- rstan::stan(model_code = h.2.1.6,
                            data = dat.2.1.6,
                            iter = 2000,
                            chains = 4,
                            control=list(adapt_delta=0.99))


# ----  4.2.2 Effect of each weekly-lagged average Ae. ae density value ----

# Effect of average Ae. ae density at 0–6 week lags on
# probability of DENV detection

for (lag in 0:6) {

  # Filter data for the current lag, removing NAs
  h.week <- h.area.week.lag %>%
    filter(!is.na(!!sym(paste0("avg.aa.f.week_lag_", lag))))

  # Get number of observations
  assign(paste0("nobsv.2.2.", lag), nrow(h.week))

  # Prepare data list for Stan
  assign(paste0("dat.2.2.", lag), list(
    N = nrow(h.week),                                # Number of observations
    n = h.week$n.ind,                                # Number of individuals under surveillance
    x = h.week[[paste0("avg.aa.f.week_lag_", lag)]], # Average Ae. ae density at current lag
    y = h.week$n.denv                                # DENV-positive individuals
  ))

  # Stan model code for the current lag
  assign(paste0("h.2.2.", lag), '
data {
  int<lower=0> N;               // Number of observations
  array[N] int n;               // Number of individuals under surveillance
  array[N] real x;              // Predictor: avg.aa.f at lag
  array[N] int y;               // Outcome: DENV incidence
}
parameters {
  real alpha;                   // Intercept
  real beta;                    // Coefficient for average Ae. ae density
  real<lower=0> sigma;          // Error scale
}
model {
  vector[N] p;                  // Linear predictor

  // Priors
  alpha ~ normal(0, 1.5);       // Prior for intercept
  beta ~ normal(0, 0.5);        // Prior on avg.aa.f effect
  sigma ~ exponential(1);       // Prior on error scale

  // Likelihood
  for (i in 1:N) {
    p[i] = alpha + beta * x[i]; // Linear effect
    p[i] = inv_logit(p[i]);     // Logistic transformation
  }
  y ~ binomial(n, p);           // Binomial likelihood
}
generated quantities {
  vector[N] log_lik;            // Log-likelihood for model comparison
  vector[N] p;                  // Posterior predicted probabilities

  for (i in 1:N) {
    p[i] = alpha + beta * x[i];
    p[i] = inv_logit(p[i]);
  }

  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);  // Observation-level log-likelihood
}
  ')
}


# Fit models with individual lagged effects from 0 to 6

fit.h.2.2.0  <- rstan::stan(model_code = h.2.2.0, data = dat.2.2.0, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.2.1  <- rstan::stan(model_code = h.2.2.1, data = dat.2.2.1, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.2.2  <- rstan::stan(model_code = h.2.2.2, data = dat.2.2.2, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.2.3  <- rstan::stan(model_code = h.2.2.3, data = dat.2.2.3, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.2.4  <- rstan::stan(model_code = h.2.2.4, data = dat.2.2.4, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.2.5  <- rstan::stan(model_code = h.2.2.5, data = dat.2.2.5, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.2.6  <- rstan::stan(model_code = h.2.2.6, data = dat.2.2.6, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))


# ----  4.2.3 Effect of each weekly-lagged vector index value ----

# Effect of vector index (vi) at 0–6 week lags on probability of DENV detection

# Loop through lags 0 to 6
for (lag in 0:6) {

  # Filter data for the current lag, removing NAs
  h.week <- h.area.week.lag %>%
    filter(!is.na(!!sym(paste0("vi.week_lag_", lag))))

  # Get number of observations
  assign(paste0("nobsv.2.3.", lag), nrow(h.week))

  # Prepare data list for Stan
  assign(paste0("dat.2.3.", lag), list(
    N = nrow(h.week),                            # Number of observations
    n = h.week$n.ind,                            # Number of individuals under surveillance
    x = h.week[[paste0("vi.week_lag_", lag)]],   # Vector index at current lag
    y = h.week$n.denv                            # DENV-positive individuals
  ))

  # Stan model code for the current lag
  assign(paste0("h.2.3.", lag), '
data {
  int<lower=0> N;               // Number of observations
  array[N] int n;               // Number of individuals under surveillance
  array[N] real x;              // Predictor: vector index at lag
  array[N] int y;               // Outcome: number of DENV cases
}
parameters {
  real alpha;                   // Intercept
  real beta;                    // Coefficient for vector index
  real<lower=0> sigma;          // Error scale
}
model {
  vector[N] p;                  // Linear predictor

  // Priors
  alpha ~ normal(0, 0.5);       // Weak prior for intercept
  beta ~ normal(0, 0.2);        // Prior on vegetation index effect
  sigma ~ exponential(1);       // Prior on error scale

  // Likelihood
  for (i in 1:N) {
    p[i] = alpha + beta * x[i]; // Linear effect of vector index
    p[i] = inv_logit(p[i]);     // Logistic transformation
  }
  y ~ binomial(n, p);           // Binomial likelihood
}
generated quantities {
  vector[N] log_lik;            // Log-likelihood for model comparison
  vector[N] p;                  // Posterior predicted probabilities

  for (i in 1:N) {
    p[i] = alpha + beta * x[i];
    p[i] = inv_logit(p[i]);
  }

  for (i in 1:N)
    log_lik[i] = binomial_lpmf(y[i] | n[i], p[i]);  // Observation-level log-likelihood
}
  ')
}


# Fit models with individual lagged effects from 0 to 6
fit.h.2.3.0  <- rstan::stan(model_code = h.2.3.0, data = dat.2.3.0, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.3.1  <- rstan::stan(model_code = h.2.3.1, data = dat.2.3.1, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.3.2  <- rstan::stan(model_code = h.2.3.2, data = dat.2.3.2, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.3.3  <- rstan::stan(model_code = h.2.3.3, data = dat.2.3.3, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.3.4  <- rstan::stan(model_code = h.2.3.4, data = dat.2.3.4, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.3.5  <- rstan::stan(model_code = h.2.3.5, data = dat.2.3.5, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))

fit.h.2.3.6  <- rstan::stan(model_code = h.2.3.6, data = dat.2.3.6, iter = 2000,
                            chains = 4, control=list(adapt_delta=0.99))


# ----  4.3 Compile and save models with human incidence outcomes ----

# Make list of models with Dirichlet-weighted lags and human incidence outcomes
h.0.lag.list <- list(fit.h.01, fit.h.02,fit.h.03)

# Assign name to models with Dirichlet-weighted lags and human incidence outcomes
h.0.lag.list.index <- data.frame(model.name= c("h.01", "h.02","h.03"),
                                 index= seq(1,length(h.0.lag.list), by=1))

# Save models with Dirichlet-weighted lags and human incidence outcomes
saveRDS(h.0.lag.list, here("analysis", "outputs", "models", "h.0.lag.list.rds"))
saveRDS(h.0.lag.list.index, here("analysis", "outputs", "models",
                                 "h.0.lag.list.index.rds"))

# Make list of all models with human incidence outcomes
h.lag.list <- list(fit.h.01, fit.h.02,fit.h.03,
                   fit.h.2.1.0,fit.h.2.1.1,fit.h.2.1.2,fit.h.2.1.3, fit.h.2.1.4,
                   fit.h.2.1.5,fit.h.2.1.6,
                   fit.h.2.2.0,fit.h.2.2.1,fit.h.2.2.2,fit.h.2.2.3, fit.h.2.2.4,
                   fit.h.2.2.5,fit.h.2.2.6,
                   fit.h.2.3.0,fit.h.2.3.1,fit.h.2.3.2,fit.h.2.3.3, fit.h.2.3.4,
                   fit.h.2.3.5,fit.h.2.3.6)

## Assign name to all models
h.lag.list.index <- data.frame(model.name= c("h.01", "h.02","h.03",
                                             "h.2.1.0","h.2.1.1","h.2.1.2","h.2.1.3",
                                             "h.2.1.4","h.2.1.5","h.2.1.6",
                                             "h.2.2.0","h.2.2.1","h.2.2.2","h.2.2.3",
                                             "h.2.2.4","h.2.2.5","h.2.2.6",
                                             "h.2.3.0","h.2.3.1","h.2.3.2","h.2.3.3",
                                             "h.2.3.4","h.2.3.5","h.2.3.6"),
                               index= seq(1,length(h.lag.list ), by=1))

# Save all models with human incidence outcomes
saveRDS(h.lag.list, here("analysis", "outputs", "models", "h.lag.list.rds"))
saveRDS(h.lag.list.index, here("analysis", "outputs", "models",
                               "h.lag.list.index.rds"))


