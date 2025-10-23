# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## FIGURES =====================================================================

# Description:
#     Figures for publication.

# Paper:
#     Detection of dengue virus in Aedes aegypti during an urban epidemic
# in Iquitos, Peru (December 2010 to March 2011)

# Script author:
#     Anna B. Kawiecki        ORCID: 0000-0002-0499-2612

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# ---- 0. Load -----------------------------------------------------------------

# ---- 0.1 Read in R libraries ----

<<<<<<< HEAD
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

=======
library(pacman)

pacman::p_load(
  here,         # Handles relative file paths in a project-agnostic way
  tidyverse,    # Core packages for data manipulation and visualization (ggplot2)
  RColorBrewer, # Color palettes
  scales,       # Enhanced ggplot2 with control over axis breaks, labels, and formatting
  cowplot,      # For combining plots and themes (used for figure layout)
  patchwork     # For combining multiple ggplot2 plots
)
>>>>>>> 039226c2534a9e37020f42822743fb30c9c8b648


# ---- 0.2 Read in data ----

m.surv <- readRDS(here("analysis", "data", "derived_data",
                       "household_level_data", "m.surv.rds"))
m.h.surv <- readRDS(here("analysis", "data", "derived_data",
                         "household_level_data", "m.h.surv.rds"))
h.area <-readRDS(here("analysis", "data", "derived_data", "area_level_data",
                      "h.area.rds"))
m.lag.fe.df <- readRDS(here("analysis", "outputs", "models", "m.lag.fe.df.rds"))

h.lag.fe.df <- readRDS(here("analysis", "outputs", "models", "h.lag.fe.df.rds"))

h.0.waic <- readRDS(here("analysis", "outputs", "models", "h.0.waic.rds"))

# ---- 0.3 Color schemes ----

area.colors <- c("#56B4E9", "#009E73", "#F0E442","#CC79A7" )
agg.pallete <- brewer.pal(n = 3, name = "Set2")
lag.pallete <- c('#0c2c84',"#8DD3C7",  "#BEBADA", "#FB8072", "#80B1D3",
                 "#FDB462", "#B3DE69", "#FCCDE5")

# ---- 0.4 Create breaks in the time series ----

# Weekly sequence for the time period where Ae. aegypti females were tested
week_breaks <- seq.Date(from = as.Date("2010-12-01"),
                        to = as.Date("2011-03-31"),
                        by = "week")

# ---- 1. Figure 1 -------------------------------------------------------------

# Figure 1: Entomological adult surveys per epidemiological week.

fig1 <- ggplot(data = m.surv %>% filter(status == "WORKED")) +

  # Histogram of surveys per week
  geom_histogram(
    mapping = aes(
      x = date,
      group = ae.f.survey,     # Group by Ae. aegypti female survey result
      fill  = ae.f.survey      # Fill bars based on survey result
    ),
    breaks = week_breaks,      # Weekly bins (custom defined elsewhere)
    closed = "left"            # Bin intervals closed on the left
  ) +

  # X-axis formatting for date
  scale_x_date(
    expand            = c(0, 0),         # Remove extra space on axis
    date_breaks       = "4 weeks",       # Major gridline every 4 weeks
    date_minor_breaks = "week",          # Minor gridline every week
    date_labels       = "%d %b\n%Y"      # Custom date label format
  ) +

  # Y-axis with no padding
  scale_y_continuous(
    expand = c(0, 0)                      # Align histogram to x-axis
  ) +

  # Manual fill colors and custom legend labels
  scale_fill_manual(
    values = c(
      "DENV positive (>=1 Ae.ae female)"   = "tomato2",
      "DENV negative (all Ae.ae females)"  = "seagreen3",
      "No DENV test on Ae.ae females"      = "#6BAED6",
      "No Ae.ae females collected"         = "grey"
    ),
    labels = c(
      "DENV positive (>=1 Ae.ae female)"   = expression(paste(">= 1 DENV positive ", italic("Ae.aegypti"), " female")),
      "DENV negative (all Ae.ae females)"  = expression(paste("No DENV positive ", italic("Ae.aegypti"), " females")),
      "No DENV test on Ae.ae females"      = expression(paste("No DENV PCR test on ", italic("Ae.aegypti"), " females")),
      "No Ae.ae females collected"         = expression(paste("No ", italic("Ae.aegypti"), " females collected"))
    )
  ) +

  # Axis labels and legend title
  labs(
    fill = expression(paste("Adult ", italic("Ae.aegypti"),
                            " female survey result")),
    x    = "Week of adult survey",
    y    = "Number of surveys"
  ) +

  # Theme formatting
  theme_bw() +
  theme(
    legend.position      = "bottom",
    legend.box           = "vertical",
    legend.text.align    = 0,
    legend.title         = element_text(hjust = 0.5),
    legend.justification = "center",
    base_size            = 8,
    base_family          = "Arial"
  ) +

  # Configure legend appearance
  guides(fill = guide_legend(nrow = 4, title.position = "top"))

# Save Figure 1 to file
ggsave(
  here("analysis", "outputs", "figures", "fig1.jpg"),
  fig1,
  width  = 100,
  height = 110,
  dpi    = 500,
  units  = "mm"
)


# ---- 2. Figure 2 -------------------------------------------------------------

# Figure 2: Point estimates with associated 95%CI aggregated by
# epidemiological week and averaged over all observations collected across
# the entire city

# This section generates a multi-panel figure (Figure 2) illustrating
# Aedes aegypti abundance, Aedes aegypti DENV prevalence, and human DENV incidence
# over time.

# ---- 2.1 Ae. aegypti female abundance ----

iq.density <- m.h.surv %>%
  st_drop_geometry() %>%
  filter(host == "mosquito") %>%
  group_by(epiweek, date.surv) %>%
  # Calculate weekly mean abundance, standard deviation, standard error, and 95% CI
  mutate(
    mean     = mean(n.ind, na.rm = TRUE),
    sd       = sd(n.ind),
    se       = sd / sqrt(n()),
    ci_lower = mean - (1.96 * se),
    ci_upper = mean + (1.96 * se)
  ) %>%
  ungroup() %>%
  ggplot() +
  # Plot mean line
  geom_line(aes(x = date.surv, y = mean), size = 1) +
  # Plot points at each week
  geom_point(aes(x = date.surv, y = mean), size = 2, alpha = 0.9) +
  # Add confidence ribbon
  geom_ribbon(aes(x = date.surv, ymin = ci_lower, ymax = ci_upper),
              fill = "grey", alpha = 0.5) +
  # Area fill below mean line
  geom_area(aes(x = date.surv, y = mean), fill = area.colors[2], alpha = 0.4) +
  # X-axis: epidemiological week labels
  scale_x_date(
    date_labels = "%V\n%b",
    date_breaks = "week",
    expand = c(0, 0)
  ) +
  # Y-axis: clean formatting
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(
    title = expression(paste("Average ", italic("Ae.aegypti"), " abundance/survey")),
    x     = "Epidemiological week",
    y     = expression(paste("Average ", italic("Ae.aegypti"), " abundance/survey"))
  ) +
  theme_bw(base_size = 10, base_family = "Arial")

# ---- 2.2. Ae. aegypti DENV Prevalence ----

iq.ae.prev <- m.h.surv %>%
  st_drop_geometry() %>%
  filter(host == "mosquito") %>%
  # Handle missing tested counts and DENV values
  mutate(
    n.tested = na_if(n.tested, 0),
    n.denv   = case_when(is.na(n.tested) ~ NA, TRUE ~ n.denv)
  ) %>%
  group_by(epiweek, date.surv) %>%
  # Calculate prevalence and CI
  mutate(
    prevalence = sum(n.denv, na.rm = TRUE) / sum(n.tested, na.rm = TRUE),
    se = sqrt((prevalence * (1 - prevalence)) / sum(n.tested, na.rm = TRUE))
  ) %>%
  reframe(
    sum.n.tested = sum(n.tested, na.rm = TRUE),
    l.ci         = prevalence - 1.96 * se,
    u.ci         = prevalence + 1.96 * se,
    prevalence.p.100 = prevalence * 100,
    perc.l.ci    = l.ci * 100,
    perc.u.ci    = u.ci * 100
  ) %>%
  distinct() %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = date.surv, y = prevalence.p.100), size = 1) +
  geom_point(aes(x = date.surv, y = prevalence.p.100), size = 2, alpha = 0.9) +
  geom_ribbon(aes(x = date.surv, ymin = perc.l.ci, ymax = perc.u.ci),
              fill = "grey", alpha = 0.5) +
  geom_area(aes(x = date.surv, y = prevalence.p.100), fill = area.colors[1], alpha = 0.4) +
  scale_x_date(
    date_labels = "%V\n%b",
    date_breaks = "week",
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(
    title = expression(paste(italic("Ae.aegypti"), " DENV prevalence (%)")),
    x     = "Epidemiological week",
    y     = expression(paste(italic("Ae.aegypti"), " DENV prevalence (%)"))
  ) +
  theme_bw(base_size = 10, base_family = "Arial")


# ---- 2.3 Human DENV incidence ----

iq.hum.inc <- m.h.surv %>%
  st_drop_geometry() %>%
  # Exclude specific municipalities
  filter(!(moh %in% c("29", "32", "33", "34", "22", "23", "24",
                      "25", "26", "27", "28", "30", "31"))) %>%
  filter(host == "human") %>%
  mutate(
    n.tested = na_if(n.tested, 0),
    n.denv   = case_when(is.na(n.tested) ~ NA, TRUE ~ n.denv)
  ) %>%
  group_by(epiweek, date.surv) %>%
  mutate(
    prevalence = sum(n.denv, na.rm = TRUE) / sum(n.ind, na.rm = TRUE),
    se = sqrt((prevalence * (1 - prevalence)) / sum(n.ind, na.rm = TRUE))
  ) %>%
  reframe(
    sum.n.ind        = sum(n.ind, na.rm = TRUE),
    l.ci             = prevalence - 1.96 * se,
    u.ci             = prevalence + 1.96 * se,
    incidence.p.1000 = prevalence * 1000,
    perc.l.ci        = l.ci * 1000,
    perc.u.ci        = u.ci * 1000
  ) %>%
  distinct() %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = date.surv, y = incidence.p.1000), size = 1) +
  geom_point(aes(x = date.surv, y = incidence.p.1000), size = 2, alpha = 0.9) +
  geom_ribbon(aes(x = date.surv, ymin = perc.l.ci, ymax = perc.u.ci),
              fill = "grey", alpha = 0.5) +
  geom_area(aes(x = date.surv, y = incidence.p.1000), fill = area.colors[4], alpha = 0.4) +
  scale_x_date(
    date_labels = "%V\n%b",
    date_breaks = "week",
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(
    title = "DENV infections/1000 individuals under surveillance",
    x     = "Epidemiological week",
    y     = "DENV infections \nper 1000 individuals"
  ) +
  theme_bw(base_size = 10, base_family = "Arial")

# ---- 2.4 Combine all three panels into one figure ----

fig2 <- iq.density / iq.ae.prev / iq.hum.inc + plot_annotation(
  tag_levels = 'A' # Automatically label panels as A, B, C
)

# Save combined figure to output
ggsave(
  here("analysis", "outputs", "figures", "fig2.jpg"),
  fig2,
  width = 150,
  height = 200,
  dpi = 500,
  units = "mm"
)

# ---- 3. Figure 3 -------------------------------------------------------------

# Figure 3: Visual representation of the temporal trends of the 3 entomological
# surveillance metrics

# These metrics are represented across three neighborhoods in Iquitos:
# Punchana, Iquitos, and San Juan, which correspond to North Iquitos,
# Central Iquitos and South Iquitos, respectively, in the paper.

# ---- 3.1 Reshape surveillance data to long format ----

h.area.long <- h.area %>%

  # Select relevant variables from the full dataset
  dplyr::select(area, epiweek, date.surv,
                incidence.p.1000,     # Human case incidence per 1000 individuals
                prevalence.p.100,     # Ae. aegypti DENV prevalence per 100 females
                avg.aa.f,             # Average female Ae. aegypti abundance
                vi                    # Vector index
  ) %>%

  # Convert from wide to long format
  pivot_longer(
    cols = c(incidence.p.1000, prevalence.p.100, avg.aa.f, vi),
    names_to = "variable",
    values_to = "value"
  ) %>%

  # Assign human-readable names for plot labels or legends
  mutate(variable.name = case_when(
    variable == "incidence.p.1000"   ~ "DENV case \nincidence/1000",
    variable == "prevalence.p.100"   ~ "DENV Ae.aegypti fem \nprevalence/100",
    variable == "avg.aa.f"           ~ "Average Ae.aegypti fem \ndensity",
    variable == "vi"                 ~ "Vector index"
  )) %>%

  # Reorder the factor levels to control plotting order
  mutate(variable = factor(variable,
                           levels = c("prevalence.p.100",
                                      "avg.aa.f",
                                      "vi",
                                      "incidence.p.1000"))) %>%

  # Optional styling variable for plotting transparency or color
  mutate(collection = case_when(
    !is.na(value) == TRUE ~ 1,     # Full visibility for actual values
    TRUE                 ~ 0.2     # Reduced opacity for missing values
  )) %>%

  # Remove duplicate entries per area-date-variable
  group_by(area, date.surv, variable) %>%
  distinct() %>%

  # Count entries per area-date-variable
  mutate(n = n()) %>%

  ungroup()

# NOTE:
# Because original observations are grouped by host (e.g., human/mosquito),
# some area-date-variable combinations appear multiple times (e.g., NA for both hosts).
# After reshaping to long format, these duplicated NA combinations remain,
# unless explicitly filtered out.

# Aedes DENV prevalence and vector index share the left y-axis
# DENV infection/1000 people & average vector abundance share the right y-axis.

# ---- 3.2 Panel A) North Iquitos  ----

# ---- 3.2.1 North Iquitos Aedes DENV prevalence and vector index ----

h.area.v.vbles.pu.plot.1 <- h.area.long %>%
  filter(area=="punchana" &
           (variable== "prevalence.p.100" | variable== "vi") ) %>%
  filter(!is.na(value)) %>%
  # Establish x and y for entire plot
  ggplot(aes(x = date.surv, y = value, color= variable) ) +
  geom_line(alpha= 0.7) +              # Plot line
  geom_point(size=2, alpha= 0.7) +     # Plot points at the weekly breaks
  scale_x_date(date_labels="%V\n%b",  # Date label format: month date_labels       = "%V\n%b"
               date_breaks="week",     # Date labels on 1st of each month
               expand=c(0,0)) +        # Remove excess space
  scale_y_continuous(
    limits = c(0, 45),
    expand  = c(0,0))+
  scale_color_manual(
    values = c( "prevalence.p.100" = area.colors[1],
                "avg.aa.f"= area.colors[2],
                "vi"=area.colors[3],
                "incidence.p.1000" = area.colors[4]),
    labels = c("incidence.p.1000" = "DENV infections/1000 people",
               "avg.aa.f" = expression(paste("Average ", italic("Ae.aegypti")," abundance" )),
               "prevalence.p.100" = expression(paste(italic("Ae.aegypti")," DENV prevalence (%)")),
               "vi" = "Vector index" )
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  labs(
    subtitle = "North Iquitos",
    x        = NULL,
    y        = "",
    color= "Variable") +
  theme_cowplot() +
  theme(legend.position = "none")

# ---- 3.2.2 North Iquitos human DENV incidence & average vector abundance ----

h.area.v.vbles.pu.plot.2 <- h.area.long %>%
  filter(area=="punchana" &
           (variable== "incidence.p.1000" | variable== "avg.aa.f")) %>%
  filter(!is.na(value)) %>%
  # Establish x and y for entire plot
  ggplot(aes(x = date.surv, y = value, color= variable) ) +
  geom_line(alpha= 0.7) +              # Plot line
  geom_point(size=2, alpha= 0.7) +     # Plot points at the weekly breaks
  scale_x_date(date_labels="%V\n%b",   # Date label format: month date_labels       = "%V\n%b"
               date_breaks="week",     # Date labels on 1st of each month
               expand=c(0,0)) +        # Remove excess space
  scale_y_continuous(
    limits = c(0, 6),
    expand  = c(0,0),
    position = "right")+
  scale_color_manual(
    values = c( "prevalence.p.100" = area.colors[1],
                "avg.aa.f"= area.colors[2],
                "vi"=area.colors[3],
                "incidence.p.1000" = area.colors[4]),
    labels = c("incidence.p.1000" = "DENV infections/1000 people",
               "avg.aa.f" = expression(paste("Average ", italic("Ae.aegypti")," abundance" )),
               "prevalence.p.100" = expression(paste(italic("Ae.aegypti")," DENV prevalence (%)")),
               "vi" = "Vector index" )
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  labs(
    subtitle    = "North Iquitos",
    x        = NULL,
    y        ="",  #"Vector DENV prevalence (%) \n Vector index",
    color= "Variable")+
  theme_cowplot() +
  theme(legend.position = "none")

# ---- 3.2.3 Align North Iquitos panels vertically ----

h.area.v.vbles.pu <- cowplot::align_plots(h.area.v.vbles.pu.plot.1,
                                          h.area.v.vbles.pu.plot.2,
                                          align="hv", axis="tblr")

h.area.v.vbles.pu.aligned <- ggdraw(h.area.v.vbles.pu[[1]]) +
  draw_plot(h.area.v.vbles.pu[[2]])


# ---- 3.3 Panel B) Central Iquitos  ----

# ---- 3.3.1 Central Iquitos Aedes DENV prevalence and vector index ----

h.area.v.vbles.iq.plot.1 <- h.area.long %>%
  filter(area=="iquitos" &
           (variable== "prevalence.p.100" | variable== "vi")) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = date.surv, y = value, color= variable) ) +
  geom_line(alpha= 0.7) +
  geom_point(size=2, alpha= 0.7) +
  scale_x_date(date_labels="%V\n%b",
               date_breaks="week",
               expand=c(0,0)) +
  scale_y_continuous(
    limits = c(0, 45),
    expand  = c(0,0))+
  scale_color_manual(
    values = c( "prevalence.p.100" = area.colors[1],
                "avg.aa.f"= area.colors[2],
                "vi"=area.colors[3],
                "incidence.p.1000" = area.colors[4]),
    labels = c("incidence.p.1000" = "DENV infections/1000 people",
               "avg.aa.f" = expression(paste("Average ", italic("Ae.aegypti")," abundance" )),
               "prevalence.p.100" = expression(paste(italic("Ae.aegypti")," DENV prevalence (%)")),
               "vi" = "Vector index" )
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  labs(
    subtitle    = "Central Iquitos",
    x        = NULL,
    y        = "Vector DENV prevalence (%) & Vector index",
    color= "Variable")+
  theme_cowplot()+
  theme(legend.position = "none")


# ---- 3.3.2 Central Iquitos human DENV incidence & average vector abundance ----

h.area.v.vbles.iq.plot.2 <- h.area.long %>%
  filter(area=="iquitos" & (variable== "incidence.p.1000" | variable== "avg.aa.f")) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = date.surv, y = value, color= variable) ) +
  geom_line(alpha= 0.7, na.rm = TRUE) +
  geom_point(size=2, alpha= 0.7) +
  scale_x_date(date_labels="%V\n%b",
               date_breaks="week",
               expand=c(0,0)) +
  scale_y_continuous(
    limits = c(0, 6),
    expand  = c(0,0),
    position = "right")+
  scale_color_manual(
    values = c( "prevalence.p.100" = area.colors[1],
                "avg.aa.f"= area.colors[2],
                "vi"=area.colors[3],
                "incidence.p.1000" = area.colors[4]),
    labels = c("incidence.p.1000" = "DENV infections/1000 people",
               "avg.aa.f" = expression(paste("Average ", italic("Ae.aegypti")," abundance" )),
               "prevalence.p.100" = expression(paste(italic("Ae.aegypti")," DENV prevalence (%)")),
               "vi" = "Vector index" )
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  labs(
    subtitle    = "Central Iquitos",
    x        = NULL,
    y        = "DENV infections/1000 people & Average vector abundance",
    color= "Variable")+
  theme_cowplot()+
  theme(legend.position = "none")

# ---- 3.3.3 Align Central Iquitos panels vertically ----

h.area.v.vbles.iq <- cowplot::align_plots(h.area.v.vbles.iq.plot.1,
                                          h.area.v.vbles.iq.plot.2,
                                          align="hv", axis="tblr")
h.area.v.vbles.iq.aligned <- ggdraw(h.area.v.vbles.iq[[1]]) +
  draw_plot(h.area.v.vbles.iq[[2]])

# ---- 3.4 Panel C) South Iquitos  ----

# ---- 3.4.1 South Iquitos Aedes DENV prevalence and vector index ----

  h.area.v.vbles.sj.plot.1 <- h.area.long %>%
  # Retain rows where data is collected or not NA
    filter(n == 1 | !is.na(value)) %>%
  # Replace missing values with 0
    mutate(value = ifelse(is.na(value), 0, value)) %>%
  # Select Aedes DENV prevalence and vector index
    filter(area == "san.juan" &
             (variable == "prevalence.p.100" | variable == "vi")) %>%
    # Create time series plot
    ggplot(aes(x = date.surv, y = value, color = variable)) +
  # Plot lines with some transparency
    geom_line(alpha = 0.7) +
  # Overlay data points with alpha based on data collection presence
    geom_point(size = 2, aes(alpha = collection)) +

    # Customize x-axis: weekly intervals and month labels
    scale_x_date(
      date_labels = "%V\n%b",     # Show week number and abbreviated month on x-axis
      date_breaks = "week",       # Breaks at each week
      expand = c(0, 0)            # Remove extra space at plot edges
    ) +

    # Customize y-axis for prevalence and vector index
    scale_y_continuous(
      limits = c(0, 45),          # Set y-axis range
      expand = c(0, 0)
    ) +

    # Manual color mapping for each variable
    scale_color_manual(
      values = c(
        "prevalence.p.100" = area.colors[1],
        "avg.aa.f" = area.colors[2],
        "vi" = area.colors[3],
        "incidence.p.1000" = area.colors[4]
      ),
      labels = c(
        "incidence.p.1000" = "DENV case incidence/1000",
        "avg.aa.f" = expression(paste("Average ", italic("Ae.aegypti"), " density/survey")),
        "prevalence.p.100" = expression(paste(italic("Ae.aegypti"), " DENV prevalence (%)")),
        "vi" = "Vector index"
      )
    ) +

    theme_bw(base_size = 10, base_family = "Arial") +  # Use clean white background theme

    labs(
      subtitle = "South Iquitos",   # Subplot label
      x = NULL,                     # No x-axis title
      y = "",                       # No y-axis title
      color = "Variable"            # Legend title
    ) +

    theme_cowplot() +              # Use cowplot styling
    theme(legend.position = "none")  # Hide legend


# ---- 3.4.2 South Iquitos average vector abundance ----

# There was no human surveillance in this area.

  h.area.v.vbles.sj.plot.2 <- h.area.long %>%
    # Retain rows where data is collected or not NA
    filter(n == 1 | !is.na(value)) %>%
    # Replace missing values with 0
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    # Select only female abundance data
    filter(area == "san.juan" & (variable == "avg.aa.f")) %>%

    # Create time series plot
    ggplot(aes(x = date.surv, y = value, color = variable)) +
    # Plot lines with some transparency
    geom_line(alpha = 0.7) +
    # Overlay data points with alpha based on data collection presence
    geom_point(size = 2, aes(alpha = collection)) +

    # Format x-axis
  scale_x_date(
    date_labels = "%V\n%b",     # Show week number and abbreviated month
    date_breaks = "week",
    expand = c(0, 0)
  ) +

  # Format y-axis for abundance, displayed on right side
  scale_y_continuous(
    limits = c(0, 5),
    expand = c(0, 0),
    position = "right"          # Position y-axis on right for layering
  ) +

  # Match color and labels for consistency
  scale_color_manual(
    values = c(
      "prevalence.p.100" = area.colors[1],
      "avg.aa.f" = area.colors[2],
      "vi" = area.colors[3],
      "incidence.p.1000" = area.colors[4]
    ),
    labels = c(
      "incidence.p.1000" = "DENV case incidence/1000",
      "avg.aa.f" = expression(paste("Average ", italic("Ae.aegypti"), " density/survey")),
      "prevalence.p.100" = expression(paste(italic("Ae.aegypti"), " DENV prevalence (%)")),
      "vi" = "Vector index"
    )
  ) +

  theme_bw(base_size = 10, base_family = "Arial") +

  labs(
    subtitle = "South Iquitos",  # Subplot label
    x = "Epidemiological week",  # x-axis title
    y = "",                      # No y-axis title
    color = "Variable"
  ) +

  theme_cowplot() +
  theme(legend.position = "none")  # Hide legend

# ---- 3.4.3 Align South Iquitos panels vertically ----

# Align the two plots by both horizontal and vertical axes
h.area.v.vbles.sj <- cowplot::align_plots(
  h.area.v.vbles.sj.plot.1,
  h.area.v.vbles.sj.plot.2,
  align = "hv",   # Align both horizontally and vertically
  axis = "tblr"   # Align top, bottom, left, and right axes
)

# Overlay the second plot (female abundance) onto the first (prevalence + VI)
h.area.v.vbles.sj.aligned <- ggdraw(h.area.v.vbles.sj[[1]]) +
  draw_plot(h.area.v.vbles.sj[[2]])

# Display final aligned composite figure
h.area.v.vbles.sj.aligned

# ---- 3.5 Generate plot for common legend  ----

h.area.v.vbles.legend.plot <- h.area.long %>%
  ggplot(aes(x = date.surv, y = value, color= variable) ) +
  geom_line(alpha= 0.7) +
  geom_point(size=2, alpha= 0.7) +
  scale_x_date(date_labels="%V\n%b",
               date_breaks="week",
               expand=c(0,0)) +
  scale_y_continuous(
    expand  = c(0,0))+
  scale_color_manual(
    values = c( "prevalence.p.100" = area.colors[1],
                "avg.aa.f"= area.colors[2],
                "vi"=area.colors[3],
                "incidence.p.1000" = area.colors[4]),
    labels = c("incidence.p.1000" = "DENV infections \nper 1000 people",
               "avg.aa.f" = "Average vector \n abundance",
               "prevalence.p.100" = "Vector DENV \nprevalence (%)",
               "vi" = "Vector index" )
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +
  labs(
    subtitle    = "Punchana",
    x        = "",
    y        = "Value",
    color= "Variable")+
  theme_cowplot()

# Extract legend from legend plot
h.area.v.vbles.legend <- get_legend(h.area.v.vbles.legend.plot)


# ---- 3.6 Align all area plots and add common legend  ----

# Align all area plots
h.area.v.vbles.plot <- plot_grid(
  h.area.v.vbles.pu.aligned,
  h.area.v.vbles.iq.aligned,
  h.area.v.vbles.sj.aligned,
  align = 'v', axis = 'l', ncol=1)

# Generate grid plot with legend to the right of the aligned area plots.
h.area.v.vbles.legend.plot <- plot_grid(h.area.v.vbles.plot,
                                        h.area.v.vbles.legend,
                                        align="h", ncol=2,rel_widths = c(1, .3))

# Seave output area plots with legend
ggsave(here("analysis","outputs", "figures", "fig3.jpg"),
       h.area.v.vbles.legend.plot,
       width = 200, height = 150, dpi = 500, units = "mm")


# ---- 4. Figure 4 -------------------------------------------------------------

# Figure 4: A) and B) show the results from a model measuring the temporally
# lagged association between Ae. aegypti female abundance and Ae. aegypti female
# DENV prevalence. C) and D) show the results from models measuring the
# association between weekly lagged explanatory metrics of the Ae. aegypti
# population on dengue case incidence in the human population,
# where the candidate explanatory variables were:
# average Ae. aegypti female abundance, Ae. aegypti DENV prevalence (%)
# and vector index per 100 surveys.

# ---- 4.1 Panel A) ----

# Create panel A: Effect sizes (ORs) of Ae. aegypti female abundance
# on  Ae. aegypti vector DENV prevalence

m.beta.fe.plot <- m.lag.fe.df %>%
  mutate(variable=  "Ae. aegypti \nfemale abundance") %>%
  filter(model.structure =="combined effect of weighted week lags" &
           parameter== "beta") %>%
  # Plot estimated fixed effects (mean ORs and 95% CI)
  ggplot() +
  geom_point(aes(y = variable, x = mean.exp, color = lag),
             size = 2, alpha = 1, position = position_dodge(width = 1)) +
  geom_errorbarh(aes(y = variable, xmin = q2.5.exp, xmax = q97.5.exp, color = lag),
                 alpha = 1, height = .2, position = position_dodge(width = 1)) +
  geom_vline(xintercept = 1) +  # Reference line at OR = 1

  # Define colors for each lag
  scale_color_manual(
    values = c("total" = lag.pallete[1], "same week"= lag.pallete[2],
               "1 week lag"=lag.pallete[3], "2 week lag" = lag.pallete[4],
               "3 week lag" = lag.pallete[5], "4 week lag" = lag.pallete[6],
               "5 week lag" = lag.pallete[7], "6 week lag" = lag.pallete[8])
  ) +

  labs(
    subtitle = "Effect of vector abundance \non vector DENV prevalence",
    y = "Total effect size", x = "Odds ratio",
    colour = "Explanatory variable",
    shape = "Model structure"
  ) +
  theme_bw(base_size = 10, base_family = "Arial") +
  guides(color = "none")

# ---- 4.2 Panel B) ----

# Create panel B: Estimated values with 95% credible intervals of
# the weight (w) parameters, representing the relative importance of
# each lagged measurement of the Ae. aegypti abundance on vector DENV prevalence.

m.lag.fe.plot <- m.lag.fe.df %>%
  filter(model.structure =="combined effect of weighted week lags" &
           parameter!= "beta") %>%
  # Plot estimated weight effects and 95% CI
  ggplot() +
  geom_col(aes(x = fct_rev(lag), y = summary.mean, fill = lag,color = lag),
           size = 1, alpha = 0.6, position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(aes(x = fct_rev(lag),
                    ymin = summary.2.5.,
                    ymax = summary.97.5.,
                    color = lag),
                alpha = 1,width = .5,
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
  labs(
    subtitlem= "Relative importance of weekly-lagged vector abundance \non vector DENV prevalence",
    x = "Weight",
    y = "Relative importance",
    fill = "Weight",
    shape= "Model structure"
  ) +
  theme_bw(
    base_size = 10,
    base_family = "Arial"
  ) +guides(color="none")+
  theme(
    axis.title.y.right = element_text(size = 10, color = "black"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right =element_blank()
  ) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Ae. aegypti \nfemale abundance"))



# ---- 4.3 Panel C) ----

# Create panel C: Effect sizes (ORs) of vector metrics on human DENV incidence
# The points are colored corresponding to the WAIC value of the models including
# each candidate explanatory variable.

# Join WAIC results and classify models by entomological metric
h.lag.fe.df <- h.lag.fe.df %>%
  left_join(h.0.waic, by = "model.name") %>%
  mutate(variable= case_when(
    grepl("\\d+\\.1\\.\\d+", model.name) ==TRUE |
      grepl("^h.01", model.name) ==TRUE  ~ "Ae. aegypti \nDENV prevalence",
    grepl("\\d+\\.2\\.\\d+", model.name) ==TRUE |
      grepl("^h.02", model.name) ==TRUE  ~ "Ae. aegypti \nfemale abundance",
    grepl("\\d+\\.3\\.\\d+", model.name) ==TRUE |
      grepl("^h.03", model.name) ==TRUE  ~ "Vector index"
  )) %>%
  mutate(variable=factor(variable, levels= c("Ae. aegypti \nDENV prevalence",
                                             "Vector index",
                                             "Ae. aegypti \nfemale abundance")))

# Plot panel C
h.beta.fe.plot <- h.lag.fe.df %>%
  filter(model.structure =="combined effect of weighted week lags" &
           parameter== "beta") %>%
  # Plot estimated fixed effects (mean ORs and 95% CI, colored by WAIC value)
  ggplot() +
  geom_point(aes(y = variable, x = mean.exp, color = WAIC),
             size = 2, alpha = 1, position = position_dodge(width = 1)
  ) +
  geom_errorbarh(
    aes(y =variable, xmin = q2.5.exp, xmax = q97.5.exp,
        color = WAIC),
    alpha = 1,height = .2, position = position_dodge(width = 1)) +
  geom_vline(xintercept = 1) +
  # Facet by variable
  facet_grid( variable ~ ., scales = "free", space = "free",
              labeller = label_wrap_gen(width = 6)) +
  labs(
    subtitle= "Effect of vector metrics \non dengue case incidence",
    y = "Total effect size", x = "Odds ratio",
    colour = "WAIC",
    shape= "Model structure"
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


# ---- 4.4 Panel D) ----

# Create panel D: Estimated values with 95% credible intervals of
# the weight (w) parameters, representing the relative importance of
# each lagged measurement of the vector metric on human DENV incidence.

h.lag.fe.plot <- h.lag.fe.df %>%
  filter(model.structure =="combined effect of weighted week lags" &
           parameter!= "beta") %>%
  ggplot() +
  geom_col(aes(x = fct_rev(lag), y = summary.mean, fill = lag,color = lag),
           size = 1, alpha = 0.6, position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(aes(x = fct_rev(lag),
                    ymin = summary.2.5.,
                    ymax = summary.97.5.,
                    color = lag),
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
  # Facet by variable
  facet_grid( variable ~ .,scales = "free", space = "free",
              labeller =
                label_wrap_gen(width = 6)) +
  labs(
    subtitle= "Relative importance of weekly-lagged vector metrics \non dengue case incidence",
    x = "Weight", y = "Relative importance",
    fill = "Weight",
    shape= "Model structure"
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

# ---- 4.5 Combine all panels ----

layout <- "
AABBBBB
CCDDDDD
CCDDDDD
CCDDDDD
"

model.plot <-
  m.beta.fe.plot +
  m.lag.fe.plot+
  h.beta.fe.plot +
  h.lag.fe.plot+
  plot_layout(guides = "collect", design= layout)+
  plot_annotation(  tag_levels = 'A')

# Save Figure 4 plot
ggsave(here("analysis", "outputs", "figures", "fig4.jpg"), model.plot,
       width = 260, height = 250, dpi = 500, units = "mm")


