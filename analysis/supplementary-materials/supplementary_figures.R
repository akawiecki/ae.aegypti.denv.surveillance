# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## SUPPLEMENTARY MATERIAL ======================================================

# Description:
#     Supplementary material and figures.

# Paper:
#     Detection of dengue virus in Aedes aegypti during an urban epidemic
# in Iquitos, Peru (December 2010 to March 2011)

# Script author:
#     Anna B. Kawiecki        ORCID: 0000-0002-0499-2612

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# ---- 0. Load -----------------------------------------------------------------

# ---- 0.1 Read in R libraries ----

# Handles relative file paths in a project-agnostic way
library(here)

# Core packages for data manipulation and visualization (includes ggplot2)
library(tidyverse)

# Color palettes
library(RColorBrewer)

# Enhanced ggplot2 with control over axis breaks, labels, and formatting
library(scales)

# For combining plots and themes (used for figure layout)
library(cowplot)

# For combining multiple ggplot2 plots
library(patchwork)

# ---- 0.2 Color schemes ----

area.colors <- c("#56B4E9", "#009E73", "#F0E442","#CC79A7" )
agg.pallete <- brewer.pal(n = 3, name = "Set2")
lag.pallete <- c('#0c2c84',"#8DD3C7",  "#BEBADA", "#FB8072", "#80B1D3",
                 "#FDB462", "#B3DE69", "#FCCDE5")

# ---- 1. Supplementary Figure 1 -----------------------------------------------

# Visual representation of the aggregation areas of the city
# A) Locations where Ae. aegypti females were tested by RT-qPCR.
# B) Locations where human febrile cases were tested by nested RT-PCR.

# Metrics are represented across three neighborhoods in Iquitos:
# Punchana, Iquitos, and San Juan, which correspond to North Iquitos,
# Central Iquitos and South Iquitos areas, respectively, in the paper.

# ---- 1.1 Read in data ----

# Entomological observations assigned to aggregation areas
m.surv.area <- readRDS( here("analysis", "data", "derived_data","household_level_data",
                           "m.surv.area.rds"))

## Entomological and human observations assigned to aggregation areas
h.surv.area <- readRDS( here("analysis", "data", "derived_data","household_level_data",
                             "h.surv.area.rds"))

# Ministry of Health (MoH) polygons
sf.moh <- readRDS(here("analysis", "data", "raw_data", "sf.moh.rds") )


# ---- 1.2 Generate area polygons ----

# Assign areas to each MOH code based on predefined groupings
sf.moh.area <-sf.moh %>%
  mutate(area= case_when(
    moh %in% c("29","32","33","34",
               "22","23","24" ,"25","26","27","28","30","31") ~ "san.juan",
    moh %in% c("4","11","6","9","12","13","10","8","7","3","5","2","1") ~ "punchana",
    moh %in% c("17","14","19","18","20","21",
               "16","15") ~ "iquitos"
  )) %>%
  mutate(area = factor(area, levels = c("punchana", "iquitos", "san.juan")))


# Extract unique area names from the spatial data
area_vector <- na.omit(unique(sf.moh.area$area))

# Create a 100m buffer around each MOH polygon for union operations
moh.area <- sf.moh.area %>%
  mutate(moh_buffer = st_buffer(geom, 100))

# Define a function to merge MOH polygons within each area
fx.area.union <- function(x) {
  area.select = area_vector[x]

  # Subset MOHs within the selected area
  moh.area.select <- moh.area %>%
    filter(area == area.select)

  # Merge the buffered polygons into a single polygon
  union.polygon = st_union(moh.area.select$moh_buffer, by_feature = FALSE)

  # Create an sf object with the merged area polygon
  area_df <- data.frame(
    area = area_vector[x],
    polygon = union.polygon,
    stringsAsFactors = FALSE
  ) %>%
    st_as_sf()
}

# Apply the union function to each area and bind results into one sf object
sf.area <- lapply(1:length(area_vector), fx.area.union) %>%
  bind_rows()

# Remove the buffer (shrink polygons by 100m), calculate area in m2 and km2
sf.area <- st_buffer(sf.area, -100) %>%
  mutate(m2 = st_area(geometry)) %>%
  mutate(km2 = m2 / 1000000)

# ---- 1.3 Generate map of Ae. aegypti PCR tested females by area ----

# Prepare mosquito surveillance data for mapping
m.surv.area <- m.surv.area %>%
  # Set factor levels for area
  mutate(area = factor(area, levels = c("punchana", "iquitos", "san.juan"))) %>%
  # Filter for locations where mosquitoes were tested
  filter(n.tested > 0) %>%
  # Extract longitude and latitude from sf geometry
  mutate(
    longitude = sf::st_coordinates(.)[, 1],
    latitude = sf::st_coordinates(.)[, 2]
  )

# Generate faceted map of mosquito testing locations by epidemiological week
m.surv.area.epiweek.map <- ggplot()+
  # Plot background map of study areas with fill by area
  geom_sf(data = sf.area, aes(fill = area), alpha = 0.3) +
  # Overlay mosquito sampling points
  geom_sf(data = m.surv.area, color = "black", alpha = 0.7, size = 0.4) +
  theme_minimal() +
  # Facet by week of sample collection
  facet_wrap(. ~ date.surv, nrow = 3, ncol = 6) +
  # Define consistent color and fill scales for areas
  scale_color_manual(name = "Aggregation \narea",
                     values = brewer.pal(n = 3, name = "Set2"),
                     labels = c("North Iquitos", "Central Iquitos",
                                "South Iquitos")) +
  scale_fill_manual(name = "Aggregation \narea",
                    values = brewer.pal(n = 3, name = "Set2"),
                    labels = c("North Iquitos", "Central Iquitos",
                               "South Iquitos")) +
  # Legend and axis formatting
  labs(color = "Aggregation \narea") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  guides(color = "none")


# ---- 1.4 Generate map of human PCR tested individuals by area ----

# Prepare human surveillance data for mapping
h.surv.area <- h.surv.area %>%
  # Set area as a factor with ordered levels
  mutate(area = factor(area, levels = c("punchana", "iquitos", "san.juan"))) %>%
  # Keep only rows for human hosts
  filter(host == "human") %>%
  # Keep only locations with tested individuals
  filter(n.tested > 0)

# Generate faceted map of human PCR testing locations by epi week
h.surv.area.epiweek.map <- ggplot() +
  # Plot background map of areas
  geom_sf(data = sf.area, aes(fill = area), alpha = 0.3) +
  # Overlay points where human samples were tested
  geom_sf(data = h.surv.area, color = "black", alpha = 0.7, size = 0.4) +
  theme_minimal() +
  # Facet by date of collection
  facet_wrap(. ~ date.surv, nrow = 3, ncol = 6) +
  # Apply consistent colors and labels for areas
  scale_color_manual(name = "Aggregation \narea",
                     values = brewer.pal(n = 3, name = "Set2"),
                     labels = c("North Iquitos", "Central Iquitos",
                                "South Iquitos")) +
  scale_fill_manual(name = "Aggregation \narea",
                    values = brewer.pal(n = 3, name = "Set2"),
                    labels = c("North Iquitos", "Central Iquitos",
                               "South Iquitos")) +
  labs(color = "Aggregation \narea") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  guides(color = "none")


# ---- 1.5 Combine maps of human and entomological observations by area ----

# Combine entomological and human PCR maps vertically
area_map <- m.surv.area.epiweek.map / h.surv.area.epiweek.map +
  plot_layout(guides = "collect") +          # Collect legends in final layout
  plot_annotation(tag_levels = 'A')          # Auto-label panels as A, B, etc.

# Export combined figure to file
ggsave(here("analysis", "supplementary-materials", "SFig1.jpg"),
       area_map,
       width = 250, height = 250, dpi = 500, units = "mm")


# ---- 2. Supplementary Figure 2 -----------------------------------------------

# A) and B) represent results from prior sensitivity analysis for models with
# the explanatory variable average Ae. aegypti female abundance and outcome
# Ae. aegypti female DENV prevalence.
# C) and D) represent results from prior sensitivity analysis for models with
# the explanatory variable Ae. aegypti female DENV prevalence and the outcome
# dengue case incidence.

# ---- 2.1 Read in prior sensitivity analysis data ----

priors.m.01.fe.df <- readRDS(here("analysis", "outputs", "models",
                                  "priors.m.01.fe.df.rds"))
priors.h.01.fe.df <- readRDS(here("analysis", "outputs", "models",
                                  "priors.h.01.fe.df.rds"))

# ---- 2.2 Association between Ae. aegypti abundance and probability of DENV detection ----

# ---- 2.2.1 Panel A) ----

# Create panel A: Effect sizes (ORs) of Ae. aegypti female abundance
# on  Ae. aegypti vector DENV prevalence

priors.m.01.beta.fe.plot <- priors.m.01.fe.df %>%
  filter(parameter== "beta") %>%
  # Plot estimated fixed effects (mean ORs and 95% CI, colored by WAIC value)
  ggplot() +
  # points represent the mean of the fixed effect estimate
  geom_point(aes(y = parameter.name, x = mean.exp ),
             size = 2, alpha = 1, position = position_dodge(width = 0.85)
  ) +
  # error bar is the 95%CI
  geom_errorbarh(
    aes(y = parameter.name, xmin = q2.5.exp, xmax = q97.5.exp),
    alpha = 1,height = .2, position = position_dodge(width = 1)) +
  geom_vline(xintercept = 1) +

  # facet grid by model structure
  facet_grid( prior ~ ., labeller = label_wrap_gen(width = 20))  +
  labs(
    subtitle= "Effect of vector abundance \non vector DENV prevalence",
    y = "Total effect size", x = "Odds ratio",)+
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  theme(strip.text.y.right = element_text(angle = 0))


# ---- 2.2.2 Panel B) ----

# Create panel B: Estimated values with 95% credible intervals of
# the weight (w) parameters, representing the relative importance of
# each lagged measurement of the Ae. aegypti abundance on vector DENV prevalence.

priors.m.01.lag.fe.plot <- priors.m.01.fe.df %>%
  filter( parameter!= "beta" & parameter!= "alpha" & parameter!= "sigma" ) %>%
  # Plot estimated weights
  ggplot() +
  geom_col(aes(x = fct_rev(parameter.name),
               y = summary.mean,
               fill = parameter.name,
               color = parameter.name),
           size = 1, alpha = 0.6, position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(aes(x = fct_rev(parameter.name),
                    ymin = summary.2.5.,
                    ymax = summary.97.5.,
                    color = parameter.name),
                alpha = 1,
                width = .5,
                position = position_dodge(width = 0.5),
                size = 1) +
  scale_color_manual(
    values = c( "total" = lag.pallete[1],
                "same week"= lag.pallete[2],
                "1 week lag"=lag.pallete[3],
                "2 week lag" = lag.pallete[4],
                "3 week lag" = lag.pallete[5],
                "4 week lag" = lag.pallete[6],
                "5 week lag" = lag.pallete[7],
                "6 week lag" = lag.pallete[8])
  ) +
  scale_fill_manual(
    values = c( "total" = lag.pallete[1],
                "same week"= lag.pallete[2],
                "1 week lag"=lag.pallete[3],
                "2 week lag" = lag.pallete[4],
                "3 week lag" = lag.pallete[5],
                "4 week lag" = lag.pallete[6],
                "5 week lag" = lag.pallete[7],
                "6 week lag" = lag.pallete[8])
  ) +
  ylim(0,1)+
  # facet grid by model structure
  facet_grid( prior ~ ., labeller = label_wrap_gen(width = 3))  +
  labs(
    subtitle= "Relative importance of weekly-lagged vector abundance \non vector DENV prevalence",
    x = "Weight", y = "Relative importance",
    fill = "Weight",
    shape= "Model structure"
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial",
  ) +guides(color="none")+
  theme(
    strip.text = element_text(size = 6)  # Change font size here
  )


# ---- 2.3 Association between Ae. aegypti DENV prevalence and DENV incidence in humans ----

# ---- 2.3.1 Panel C) ----

# Create panel C: Effect sizes of Ae. aegypti DENV prevalence on human DENV incidence

priors.h.01.beta.fe.plot <- priors.h.01.fe.df %>%
  filter(parameter== "beta") %>%
  # Plot estimated fixed effects (mean ORs and 95% CI, colored by WAIC value)
  ggplot() +
  geom_point(aes(y = parameter.name, x = mean.exp ),
             size = 2, alpha = 1, position = position_dodge(width = 0.85)
  ) +
  geom_errorbarh(
    aes(y = parameter.name, xmin = q2.5.exp, xmax = q97.5.exp),
    alpha = 1,height = .2, position = position_dodge(width = 1)) +
  geom_vline(xintercept = 1) +
  # Facet by model prior
  facet_grid( prior ~ ., labeller = label_wrap_gen(width = 20))  +
  labs(
    subtitle= "Effect of vector DENV prevalence \non dengue case incidence",
    y = "Total effect size",
    x = "Odds ratio")+
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  theme(strip.text.y.right = element_text(angle = 0))


# ---- 2.3.2 Panel D) ----

# Create panel D: Estimated values with 95% credible intervals of
# the weight (w) parameters, representing the relative importance of
# each lagged measurement of the vector metric on human DENV incidence.

priors.h.01.lag.fe.plot <- priors.h.01.fe.df %>%
  filter( parameter!= "beta" & parameter!= "alpha" & parameter!= "sigma" ) %>%
  # Plot estimated weights
  ggplot() +
  # points represent the mean of the fixed effect estimate
  geom_col(aes(x = fct_rev(parameter.name),
               y = summary.mean,
               fill = parameter.name,
               color = parameter.name),
           size = 1, alpha = 0.6, position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(aes(x = fct_rev(parameter.name),
                    ymin = summary.2.5.,
                    ymax = summary.97.5.,
                    color = parameter.name),
                alpha = 1,
                width = .5,
                position = position_dodge(width = 0.5),
                size = 1) +

  scale_color_manual(
    values = c( "total" = lag.pallete[1],
                "same week"= lag.pallete[2],
                "1 week lag"=lag.pallete[3],
                "2 week lag" = lag.pallete[4],
                "3 week lag" = lag.pallete[5],
                "4 week lag" = lag.pallete[6],
                "5 week lag" = lag.pallete[7],
                "6 week lag" = lag.pallete[8])
  ) +
  scale_fill_manual(
    values = c( "total" = lag.pallete[1],
                "same week"= lag.pallete[2],
                "1 week lag"=lag.pallete[3],
                "2 week lag" = lag.pallete[4],
                "3 week lag" = lag.pallete[5],
                "4 week lag" = lag.pallete[6],
                "5 week lag" = lag.pallete[7],
                "6 week lag" = lag.pallete[8])
  ) +
  ylim(0,1)+
  # Facet by prior
  facet_grid( prior ~ ., labeller = label_wrap_gen(width = 1))  +
  labs(
    subtitle= "Relative importance of weekly-lagged vector DENV prevalence \non dengue case incidence",
    x = "Weight", y = "Relative importance",
    fill = "Weight",
    shape= "Model structure"
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +guides(color="none")+
  theme(
    strip.text = element_text(size = 6)  # Change font size
  )

priors.h.01.lag.fe.plot


# ---- 2.4 Combine prior sensitivity analysis panels ----

layout <- "
AABBBBB
AABBBBB
AABBBBB
AABBBBB
AABBBBB
AABBBBB
AABBBBB
CCDDDDD
CCDDDDD
CCDDDDD
CCDDDDD
CCDDDDD
CCDDDDD
CCDDDDD
"

priors.sensitivity.plot <-
  priors.m.01.beta.fe.plot + # A
  priors.m.01.lag.fe.plot +  # B
  priors.h.01.beta.fe.plot + # C
  priors.h.01.lag.fe.plot +  # D
  plot_annotation(
    tag_levels = 'A'
  ) +
  plot_layout(guides = "collect",design= layout) &
  theme(legend.position = "bottom")

ggsave(here("analysis", "supplementary-materials", "SFig2.jpg"),
       priors.sensitivity.plot,
       width = 250, height = 350,
       dpi = 500, units = "mm")

# ---- 3. Supplementary Figure 3 -----------------------------------------------

# Effect of each weekly-lagged measure of Ae. aegypti abundance on
# Ae. aegypti DENV prevalence

# ---- 3.1 Read in data ----

# Load fixed effects output from area-level logistic regression models
m.lag.fe.df <- readRDS(here("analysis", "outputs", "models",
                            "m.lag.fe.df.rds"))

# ---- 3.2 Plot Effect of each weekly-lagged measure ----

m.lag.fe.sup.plot <- m.lag.fe.df %>%
  # Exclude models with  Dirichlet-weighted lag structure
  filter(model.structure != "combined effect of weighted week lags") %>%
  # Create plot of fixed effect estimates by lag
  ggplot() +
  # Plot mean of fixed effect (odds ratio) by lag
  geom_point(aes(y = lag, x = mean.exp, color = lag),
             size = 2, alpha = 1,
             position = position_dodge(width = 1)) +
  # Add 95% confidence interval as horizontal error bars
  geom_errorbarh(aes(y = lag, xmin = q2.5.exp, xmax = q97.5.exp,
                     color = lag),
                 alpha = 1, height = 0.3,
                 position = position_dodge(width = 1)) +
  # Add vertical reference line at OR = 1
  geom_vline(xintercept = 1) +
  # Manually specify colors for lag variables
  scale_color_manual(
    values = c(
      "total" = lag.pallete[1],
      "same week" = lag.pallete[2],
      "1 week lag" = lag.pallete[3],
      "2 week lag" = lag.pallete[4],
      "3 week lag" = lag.pallete[5],
      "4 week lag" = lag.pallete[6],
      "5 week lag" = lag.pallete[7],
      "6 week lag" = lag.pallete[8]
    )
  ) +
  # Set plot labels and subtitle with italic text for species name
  labs(
    subtitle = ~atop(paste("Effect of weekly-lagged "),
                     paste(italic("Ae.aegypti"),
                           " abundance on ",
                           italic("Ae.aegypti"),
                           " DENV prevalence")),
    y = "Weekly-lagged explanatory variable",
    x = "Odds ratio",
    colour = "Explanatory variable",
    shape = "Model structure"
  ) +
  # Use a clean theme with Arial font
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  # Format legend and facet labels
  theme(
    legend.text.align = 0,
    strip.text = element_text(size = 9)
  )

# Save the plot to file for use in supplementary materials
ggsave(here("analysis", "supplementary-materials", "SFig3.jpg"),
       m.lag.fe.sup.plot,
       width = 150, height = 100, dpi = 500, units = "mm")

# ---- 4. Supplementary Figure 4 -----------------------------------------------

# Supplementary Figure 4: Model comparison of models measuring the
# relative effect of weekly- lagged measures of candidate entomological
# surveillance metrics using WAIC.

# ---- 4.1 Read in data ----

h.0.waic <- readRDS(here("analysis", "outputs", "models", "h.0.waic.rds"))


rownames(h.0.waic) <- c("Model including vector DENV prevalence",
                        "Model including vector index",
                        "Model including average vector abundance")
# ---- 4.2 Plot ----

plot(h.0.waic)

# ---- 5. Supplementary Figure 5 -----------------------------------------------

# Effect of each weekly- lagged measure of candidate entomological surveillance
# metrics on dengue case incidence in the human population

# ---- 5.1 Read in data ----

h.lag.fe.df <-  readRDS(here("analysis", "outputs", "models", "h.lag.fe.df.rds"))

# ---- 5.2 Plot Effect of each weekly-lagged measure ----

# Create the plot visualizing effect estimates for each weekly-lagged metric
h.lag.fe.sup.plot <- h.lag.fe.df %>%
  # Remove Dirichlet-weighted models from the plot
  filter(model.structure!="combined effect of weighted week lags") %>%

  # Begin ggplot: fixed effects per lag week
  ggplot() +

  # Add point estimates (odds ratios)
  geom_point(aes(y = lag, x = mean.exp, color = lag),
             size = 2, alpha = 1,
             position = position_dodge(width = 0.85)) +

  # Add horizontal 95% confidence intervals
  geom_errorbarh(
    aes(y = lag, xmin = q2.5.exp, xmax = q97.5.exp, color = lag),
    alpha = 1, height = 0.3,
    position = position_dodge(width = 0.85)) +

  # Reference line at odds ratio = 1 (no effect)
  geom_vline(xintercept = 1) +

  # Manual color scale for each lag (consistent with figure palette)
  scale_color_manual(
    values = c(
      "total" = lag.pallete[1],
      "same week" = lag.pallete[2],
      "1 week lag" = lag.pallete[3],
      "2 week lag" = lag.pallete[4],
      "3 week lag" = lag.pallete[5],
      "4 week lag" = lag.pallete[6],
      "5 week lag" = lag.pallete[7],
      "6 week lag" = lag.pallete[8]
    )
  ) +

  # Facet by explanatory variable (e.g., vector index, abundance, etc.)
  facet_grid(
    variable ~ ., scales = "free", space = "free",
    labeller = label_wrap_gen(width = 6)
  ) +

  # Axis and legend labels
  labs(
    subtitle = "Effect of weekly-lagged \nvector metrics on dengue case incidence",
    y = "Weekly-lagged explanatory variable",
    x = "Odds ratio",
    colour = "Explanatory variable",
    shape = "Model structure"
  ) +

  # Theme and formatting
  theme_bw(base_size = 10, base_family = "Arial") +
  theme(
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 10)
  )


# Save as a high-resolution JPEG file
ggsave(here("analysis", "supplementary-materials", "SFig5.jpg"),
       h.lag.fe.sup.plot,
       width = 150, height = 100, dpi = 500, units = "mm")


