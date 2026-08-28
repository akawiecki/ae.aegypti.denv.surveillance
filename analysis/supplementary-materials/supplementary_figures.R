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

# ---- 0. Load ---------- -------------------------------------------------------

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

area.colors <- c(
  "#009E73",  # green     -> Vector abundance
  "#0072B2",  # blue      -> Vector DENV prevalence
  "#E69F00",  # orange    -> Vector index
  "#CC79A7"   # pink      -> DENV infections/1000 people
)
agg.pallete <- brewer.pal(n = 3, name = "Set2")


# ---- 1. Supplementary Figure 1 -----------------------------------------------
# ---- 1.1 Read in data ----
h.area.sen <- readRDS(here("analysis", "data", "derived_data","area_level_data",
                             "h.area.sen.rds"))

h.area.sen.long <- h.area.sen %>%
  dplyr::select(area, epiweek, date.surv,
                incidence.p.1000, prevalence.p.100, avg.aa.f, vi) %>%
  pivot_longer(cols = c(incidence.p.1000, prevalence.p.100, avg.aa.f, vi),
               names_to = "variable", values_to = "value") %>%
  # There was no human surveillance in this area.
  mutate( value = case_when(
    variable == "incidence.p.1000" & area == "san.juan" ~ NA,
    T ~ value )) |>
  mutate(
    area = case_when(
      area == "punchana"  ~ "North Iquitos",
      area == "iquitos"   ~ "Central Iquitos",
      area == "san.juan"  ~ "South Iquitos"
    ),
    area = factor(area, levels = c("North Iquitos", "Central Iquitos", "South Iquitos")),
    variable_label = case_when(
      variable == "avg.aa.f"           ~ "Average Ae. aegypti\nabundance/survey",
      variable == "vi"                 ~ "Vector index",
      variable == "prevalence.p.100"   ~ "Ae. aegypti DENV\nprevalence (%)",
      variable == "incidence.p.1000"   ~ "DENV infections\nper 1000 people"
    ),
    variable_label = factor(variable_label, levels = c(
      "Average Ae. aegypti\nabundance/survey",
      "Vector index",
      "Ae. aegypti DENV\nprevalence (%)",
      "DENV infections\nper 1000 people"
    ))
  ) %>%
  filter(!is.na(value))


# ---- 1.2 Plot

Sfig1 <- h.area.sen.long %>%
  ggplot(aes(x = date.surv, y = value, color = variable_label)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 2, alpha = 0.7) +
  facet_grid(variable_label ~ area, scales = "free_y", switch = "y",
             labeller = label_wrap_gen(width = 18)) +
  scale_x_date(
    date_labels = "%V\n%b",   # epiweek number on top line, month abbreviation below
    date_breaks = "week",
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = area.colors, guide = "none") +
  labs(x = "Epidemiological week", y = NULL) +
  theme_bw(base_size = 10, base_family = "Arial") +
  theme(
    strip.background = element_rect(fill = "white", color = NA),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0),
    panel.spacing = unit(0.6, "lines"),
    panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.4),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 6.5)
  )

# Save output area plots with legend as jpg
ggsave(here("analysis", "supplementary-materials", "S1Fig.jpg"),
       Sfig1,
       width = 260, height = 150, dpi = 500, units = "mm")

# Save output area plots with legend as pdf
ggsave(here("analysis", "supplementary-materials", "S1Fig.pdf"),
       Sfig1,
       width = 260, height = 150, dpi = 500, units = "mm",
       device = cairo_pdf)


# ---- 2. Supplementary Figure 2 -----------------------------------------------
# Sensitvitiy analysis of the models measuring the association
# between weekly lagged explanatory metrics of the Ae. aegypti population on dengue
# case incidence in the human population using observations collected across
# the entire city for areas where adult female collections and febrile surveillance
# were concurrent (applying the same selection criteria that were applied to
# select households for mosquito DENV testing in February and March 2011 to
# both mosquito and human surveillance across the entire study period. )
# A) and B) show the results from models measuring the
# association between weekly lagged explanatory metrics of the Ae. aegypti
# population on dengue case incidence in the human population,
# where the candidate explanatory variables were:
# average Ae. aegypti female abundance, Ae. aegypti DENV prevalence (%)
# and vector index per 100 surveys.


# ---- 2.1 Read in data ----
h.sen.lag.fe.df <- readRDS(here("analysis", "outputs", "models",
                                "h.sen.lag.fe.df.rds"))
# Create panel A: Effect sizes (ORs) of vector metrics on human DENV incidence
# The points are colored corresponding to the WAIC value of the models including
# each candidate explanatory variable.

# Join WAIC results and classify models by entomological metric
h.sen.lag.fe.df <- h.sen.lag.fe.df%>%
  mutate(variable=factor(variable, levels= c("Ae. aegypti \nDENV prevalence",
                                             "Vector index",
                                             "Ae. aegypti \nfemale abundance")))
# ---- 2.2 Plot figure ----

# Plot panel A
h.sen.beta.fe.plot <- h.sen.lag.fe.df  %>%
  filter(model.structure =="combined effect of weighted week lags" &
           parameter== "beta") %>%
  # Plot estimated fixed effects (mean ORs and 95% CI, colored by WAIC value)
  ggplot() +
  geom_point(aes(y = variable, x = mean.exp),
             size = 2, alpha = 1, position = position_dodge(width = 1)
  ) +
  geom_errorbarh(
    aes(y =variable, xmin = q2.5.exp, xmax = q97.5.exp),
    alpha = 1,height = .2, position = position_dodge(width = 1)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  # Facet by variable
  facet_grid( variable ~ ., scales = "free", space = "free",
              labeller = label_wrap_gen(width = 6)) +
  labs(
    subtitle= "Total effect of vector metrics \non dengue case incidence",
    y = NULL, x = "Odds ratio (95% CI)"
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  # Set facet label background to white and remove the border
  theme(
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_blank()
  )


# Create panel B: Estimated values with 95% credible intervals of
# the weight (w) parameters, representing the relative importance of
# each lagged measurement of the vector metric on human DENV incidence.

h.sen.lag.fe.plot <- h.sen.lag.fe.df %>%
  filter(model.structure =="combined effect of weighted week lags" &
           parameter!= "beta") %>%
  ggplot() +
  geom_col(aes(x = fct_rev(lag), y = summary.mean),
           size = 1, alpha = 0.6, position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(aes(x = fct_rev(lag),
                    ymin = summary.2.5.,
                    ymax = summary.97.5.),
                alpha = 1,
                width = .5,
                position = position_dodge(width = 0.5),
                size = 1) +
  ylim(0,1)+
  # Facet by variable
  facet_grid( variable ~ .,scales = "free", space = "free",
              labeller =
                label_wrap_gen(width = 6)) +
  labs(
    subtitle= "Relative importance of weekly-lagged vector metrics \non dengue case incidence",
    x = "Weekly lag", y = "Relative weight (95% CI)"
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  guides(color="none") +
  # Set facet label background to white and remove the border
  theme(
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 10)
  )


layout <- "
AABBBBB
AABBBBB
AABBBBB
"

Sfig.2.plot <-
  h.sen.beta.fe.plot +
  h.sen.lag.fe.plot+
  plot_layout(guides = "collect", design= layout)+
  plot_annotation(  tag_levels = 'A')

# Save as a high-resolution JPEG file
ggsave(here("analysis", "supplementary-materials", "S2Fig.jpg"),
       Sfig.2.plot ,
       width = 260, height = 150, dpi = 500, units = "mm")

# .pdf
ggsave(here("analysis", "supplementary-materials", "S2Fig.pdf"),
       Sfig.2.plot ,
       width = 260, height = 150, dpi = 500, units = "mm",
device = cairo_pdf)
# ---- 3. Supplementary Figure 3 -----------------------------------------------
# Association between Ae. aegypti abundance and probability of DENV detection
# A) and B) represent results from prior sensitivity analysis for models with
# the explanatory variable average Ae. aegypti female abundance and outcome
# Ae. aegypti female DENV prevalence.

# ---- 3.1 Read in prior sensitivity analysis data ----

priors.m.01.fe.df <- readRDS(here("analysis", "outputs", "models",
                                  "priors.m.01.fe.df.rds"))

# ---- 3.2.1 Panel A) ----

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
  geom_vline(xintercept = 1, linetype = "dashed") +

  # facet grid by model structure
  facet_grid( prior ~ ., labeller = label_wrap_gen(width = 20))  +
  labs(
    subtitle= "Total effect of vector abundance \non vector DENV prevalence",
    y = NULL, x = "Odds ratio (95% CI)",)+
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  theme(strip.text.y.right = element_text(angle = 0))


# ---- 3.2.2 Panel B) ----

# Create panel B: Estimated values with 95% credible intervals of
# the weight (w) parameters, representing the relative importance of
# each lagged measurement of the Ae. aegypti abundance on vector DENV prevalence.

priors.m.01.lag.fe.plot <- priors.m.01.fe.df %>%
  filter( parameter!= "beta" & parameter!= "alpha" & parameter!= "sigma" ) %>%
  # Plot estimated weights
  ggplot() +
  geom_col(aes(x = fct_rev(parameter.name),
               y = summary.mean),
           size = 1, alpha = 0.6, position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(aes(x = fct_rev(parameter.name),
                    ymin = summary.2.5.,
                    ymax = summary.97.5.),
                alpha = 1,
                width = .5,
                position = position_dodge(width = 0.5),
                size = .5) +
  ylim(0,1)+
  # facet grid by model structure
  facet_grid( prior ~ ., labeller = label_wrap_gen(width = 3))  +
  labs(
    subtitle= "Relative importance of weekly-lagged vector abundance \non vector DENV prevalence",
    x = "Weekly lag", y = "Relative weight (95% CI)"
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial",
  ) +
  guides(color="none")+
  theme(
    strip.text = element_text(size = 6)  # Change font size here
  )

layout <- "
AABBBBB
AABBBBB
AABBBBB
AABBBBB
AABBBBB
AABBBBB
AABBBBB
"

SFig3 <-
  priors.m.01.beta.fe.plot + # A
  priors.m.01.lag.fe.plot +  # B
  plot_annotation(
    tag_levels = 'A'
  ) +
  plot_layout(guides = "collect",design= layout) &
  theme(legend.position = "bottom")


# Save as a high-resolution JPEG file
ggsave(here("analysis", "supplementary-materials", "S3Fig.jpg"),
       SFig3 ,
       width = 200, height = 200, dpi = 500, units = "mm")

# .pdf
ggsave(here("analysis", "supplementary-materials", "S3Fig.pdf"),
       SFig3 ,
       width = 200, height = 200, dpi = 500, units = "mm",
device = cairo_pdf)
# ---- 4. Supplementary Figure 4 -----------------------------------------------

# Association between Ae. aegypti DENV prevalence and DENV incidence in humans ----
# A) and B) represent results from prior sensitivity analysis for models with
# the explanatory variable Ae. aegypti female DENV prevalence and the outcome
# dengue case incidence.

# ---- 4.1 Read in prior sensitivity analysis data ----

priors.h.01.fe.df <- readRDS(here("analysis", "outputs", "models",
                                  "priors.h.01.fe.df.rds"))


# ---- 4.2 Panel A) ----

# Create panel A: Effect sizes of Ae. aegypti DENV prevalence on human DENV incidence

priors.h.01.beta.fe.plot <- priors.h.01.fe.df %>%
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
  geom_vline(xintercept = 1, linetype = "dashed") +

  # facet grid by model structure
  facet_grid( prior ~ ., labeller = label_wrap_gen(width = 20))  +
  labs(
    subtitle= "Total effect of vector abundance \non vector DENV prevalence",
    y = NULL, x = "Odds ratio (95% CI)",)+
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  theme(strip.text.y.right = element_text(angle = 0))


# ---- 4.3 Panel B) ----

# Create panel B: Estimated values with 95% credible intervals of
# the weight (w) parameters, representing the relative importance of
# each lagged measurement of the vector metric on human DENV incidence.

priors.h.01.lag.fe.plot <- priors.h.01.fe.df %>%
  filter( parameter!= "beta" & parameter!= "alpha" & parameter!= "sigma" ) %>%
  # Plot estimated weights
  ggplot() +
  # points represent the mean of the fixed effect estimate
  geom_col(aes(x = fct_rev(parameter.name),
               y = summary.mean),
           size = 1, alpha = 0.6, position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(aes(x = fct_rev(parameter.name),
                    ymin = summary.2.5.,
                    ymax = summary.97.5.),
                alpha = 1,
                width = .5,
                position = position_dodge(width = 0.5),
                size = .5) +
  ylim(0,1)+
  # Facet by prior
  facet_grid( prior ~ ., labeller = label_wrap_gen(width = 1))  +
  labs(
    subtitle= "Relative importance of weekly-lagged vector DENV prevalence \non dengue case incidence",
    x = "Weekly lag", y = "Relative weight (95% CI)"
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  guides(color="none")+
  theme(
    strip.text = element_text(size = 6)  # Change font size
  )

priors.h.01.lag.fe.plot


# ---- 4.4 Combine prior sensitivity analysis panels ----

layout <- "
AABBBBB
AABBBBB
AABBBBB
AABBBBB
AABBBBB
AABBBBB
AABBBBB
"

SFig4 <-
  priors.h.01.beta.fe.plot + # A
  priors.h.01.lag.fe.plot +  # B
  plot_annotation(
    tag_levels = 'A'
  ) +
  plot_layout(guides = "collect",design= layout) &
  theme(legend.position = "bottom")

ggsave(here("analysis", "supplementary-materials", "S4Fig.jpg"),
       SFig4 ,
       width = 200, height = 200,
       dpi = 500, units = "mm")

# .pdf
ggsave(here("analysis", "supplementary-materials", "S4Fig.jpg"),
       SFig4 ,
       width = 200, height = 200,
       dpi = 500, units = "mm",
device = cairo_pdf)

# ---- 5. Supplementary Figure 5 -----------------------------------------------

# Effect of each weekly-lagged measure of Ae. aegypti abundance on
# Ae. aegypti DENV prevalence

# ---- 5.1 Read in data ----

# Load fixed effects output from area-level logistic regression models
m.lag.fe.df <- readRDS(here("analysis", "outputs", "models",
                            "m.lag.fe.df.rds"))

# ---- 5.2 Plot Effect of each weekly-lagged measure ----

m.lag.fe.sup.plot <- m.lag.fe.df %>%
  # Exclude models with  Dirichlet-weighted lag structure
  filter(model.structure != "combined effect of weighted week lags") %>%
  # Create plot of fixed effect estimates by lag
  ggplot() +
  # Plot mean of fixed effect (odds ratio) by lag
  geom_point(aes(y = lag, x = mean.exp),
             size = 2, alpha = 1,
             position = position_dodge(width = 1)) +
  # Add 95% confidence interval as horizontal error bars
  geom_errorbarh(aes(y = lag, xmin = q2.5.exp, xmax = q97.5.exp),
                 alpha = 1, height = 0.3,
                 position = position_dodge(width = 1)) +
  # Add vertical reference line at OR = 1
  geom_vline(xintercept = 1) +
  # Set plot labels and subtitle with italic text for species name
  labs(
    subtitle = ~atop(paste("Effect of weekly-lagged "),
                     paste(italic("Ae.aegypti"),
                           " abundance on ",
                           italic("Ae.aegypti"),
                           " DENV prevalence")),
    y = "Weekly-lagged explanatory variable",
    x = "Odds ratio (95% CI)",
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
ggsave(here("analysis", "supplementary-materials", "S5Fig.jpg"),
       m.lag.fe.sup.plot,
       width = 150, height = 100, dpi = 500, units = "mm")

ggsave(here("analysis", "supplementary-materials", "S5Fig.pdf"),
       m.lag.fe.sup.plot,
       width = 150, height = 100, dpi = 500, units = "mm",
device = cairo_pdf)

# ---- 6. Supplementary Figure 6 -----------------------------------------------

# Effect of each weekly- lagged measure of candidate entomological surveillance
# metrics on dengue case incidence in the human population

# ---- 6.1 Read in data ----

h.lag.fe.df <-  readRDS(here("analysis", "outputs", "models", "h.lag.fe.df.rds"))

# ---- 6.2 Plot Effect of each weekly-lagged measure ----

# Create the plot visualizing effect estimates for each weekly-lagged metric
h.lag.fe.sup.plot <- h.lag.fe.df %>%
  # Remove Dirichlet-weighted models from the plot
  filter(model.structure!="combined effect of weighted week lags") %>%

  # Begin ggplot: fixed effects per lag week
  ggplot() +

  # Add point estimates (odds ratios)
  geom_point(aes(y = lag, x = mean.exp),
             size = 2, alpha = 1,
             position = position_dodge(width = 0.85)) +

  # Add horizontal 95% confidence intervals
  geom_errorbarh(
    aes(y = lag, xmin = q2.5.exp, xmax = q97.5.exp),
    alpha = 1, height = 0.3,
    position = position_dodge(width = 0.85)) +

  # Reference line at odds ratio = 1 (no effect)
  geom_vline(xintercept = 1) +

  # Facet by explanatory variable (e.g., vector index, abundance, etc.)
  facet_grid(
    variable ~ ., scales = "free", space = "free",
    labeller = label_wrap_gen(width = 6)
  ) +

  # Axis and legend labels
  labs(
    subtitle = "Effect of weekly-lagged \nvector metrics on dengue case incidence",
    y = "Weekly-lagged explanatory variable",
    x = "Odds ratio (95% CI)",
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
ggsave(here("analysis", "supplementary-materials", "S6Fig.jpg"),
       h.lag.fe.sup.plot,
       width = 150, height = 100, dpi = 500, units = "mm")

# .pdf
ggsave(here("analysis", "supplementary-materials", "S6Fig.pdf"),
       h.lag.fe.sup.plot,
       width = 150, height = 100, dpi = 500, units = "mm",
device = cairo_pdf)
# ---- 7. Supplementary Table 1 ------------------------------------------------

S1Table <- readRDS(here("analysis", "outputs", "models", "gof_diff_summary.rds"))

openxlsx::write.xlsx(S1Table, here("analysis", "supplementary-materials", "S1_Table.xlsx"))


# ---- 8. Supplementary Table 2 ------------------------------------------------

S2Table <- readRDS(here("analysis", "outputs", "models", "h.lag.results.rds"))

openxlsx::write.xlsx(S2Table, here("analysis", "supplementary-materials", "S2_Table.xlsx"))

