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

# For combining working with spatial data
library(sf)

# ---- 0.2 Read in data ----

m.ae.pcr<- readRDS(here("analysis", "data", "raw_data", "m.ae.pcr.rds"))

h.denominator.pcr.gis <- readRDS(here("analysis", "data", "raw_data",
                                      "h.denominator.pcr.gis.rds"))

m.surv <- readRDS(here("analysis", "data", "derived_data",
                       "household_level_data", "m.surv.rds"))
h.surv <- readRDS(here("analysis", "data", "derived_data",
                       "household_level_data", "h.surv.rds"))

m.h.surv <- readRDS(here("analysis", "data", "derived_data",
                         "household_level_data", "m.h.surv.rds"))
h.area <-readRDS(here("analysis", "data", "derived_data", "area_level_data",
                      "h.area.rds"))
m.lag.fe.df <- readRDS(here("analysis", "outputs", "models", "m.lag.fe.df.rds"))

h.lag.fe.df <- readRDS(here("analysis", "outputs", "models", "h.lag.fe.df.rds"))

h.0.waic <- readRDS(here("analysis", "outputs", "models", "h.0.waic.rds"))

# Entomological observations assigned to aggregation areas
m.surv.area <- readRDS( here("analysis", "data", "derived_data","household_level_data",
                             "m.surv.area.rds"))

## Entomological and human observations assigned to aggregation areas
h.surv.area <- readRDS( here("analysis", "data", "derived_data","household_level_data",
                             "h.surv.area.rds"))

# Ministry of Health (MoH) polygons
sf.moh <- readRDS(here("analysis", "data", "raw_data", "sf.moh.rds") )


# ---- 0.3 Color schemes ----

area.colors <- c(
  "#009E73",  # green     -> Vector abundance
  "#0072B2",  # blue      -> Vector DENV prevalence 
  "#E69F00",  # orange    -> Vector index 
  "#CC79A7"   # pink      -> DENV infections/1000 people 
)
agg.pallete <- brewer.pal(n = 3, name = "Set2")

# ---- 0.4 Create breaks in the time series ----

# Weekly sequence for the time period where Ae. aegypti females were tested
week_breaks <- seq.Date(from = as.Date("2010-12-01"),
                        to = as.Date("2011-03-31"),
                        by = "week")

# ---- 1. Figure 1 -------------------------------------------------------------

# Visual representation of the aggregation areas of the city
# A) Locations where Ae. aegypti females were tested by RT-qPCR.
# B) Locations where human febrile cases were tested by nested RT-PCR.

# Metrics are represented across three neighborhoods in Iquitos:
# Punchana, Iquitos, and San Juan, which correspond to North Iquitos,
# Central Iquitos and South Iquitos areas, respectively, in the paper.

# ---- 1.1 Generate area polygons ----

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

# ---- 1.2 Generate map of Ae. aegypti PCR tested females by area ----

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



# ---- 1.3 Generate map of human PCR tested individuals by area ----

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


# ---- 1.4 Combine maps of human and entomological observations by area ----

# Combine entomological and human PCR maps vertically
area_map <- m.surv.area.epiweek.map / h.surv.area.epiweek.map +
  plot_layout(guides = "collect") +          # Collect legends in final layout
  plot_annotation(tag_levels = 'A')          # Auto-label panels as A, B, etc.

# Export combined figure to file
ggsave( here("analysis", "outputs", "figures", "fig1.jpg"),
       area_map,
       width = 250, height = 250, dpi = 500, units = "mm")


# ---- 2. Figure 2 -------------------------------------------------------------

# Figure 2: Entomological adult surveys per epidemiological week.

fig2 <- ggplot(data = m.surv %>% filter(status == "WORKED")) +

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
  here("analysis", "outputs", "figures", "fig2.jpg"),
  fig2,
  width  = 100,
  height = 110,
  dpi    = 500,
  units  = "mm"
)


# ---- 3. Figure 3 -------------------------------------------------------------

# Figure 3: Point estimates with associated 95%CI aggregated by
# epidemiological week and averaged over all observations collected across
# the entire city

# This section generates a multi-panel figure (Figure 2) illustrating
# Aedes aegypti abundance, Aedes aegypti DENV prevalence, and human DENV incidence
# over time.

# ---- 3.1 Ae. aegypti female abundance ----

iq.density <- m.h.surv %>%
  st_drop_geometry() %>%
  filter(host == "mosquito") %>%
  group_by(epiweek, date.surv) %>%
  # Calculate weekly mean abundance, standard deviation, standard error, and 95% CI
  summarise(
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
  geom_area(aes(x = date.surv, y = mean), fill = area.colors[1], alpha = 0.4) +
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
  geom_area(aes(x = date.surv, y = prevalence.p.100), fill = area.colors[2], alpha = 0.4) +
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

fig3 <- iq.density / iq.ae.prev / iq.hum.inc + plot_annotation(
  tag_levels = 'A' # Automatically label panels as A, B, C
)

# Save combined figure to output
ggsave(
  here("analysis", "outputs", "figures", "fig3.jpg"),
  fig3,
  width = 150,
  height = 200,
  dpi = 500,
  units = "mm"
)

# ---- 4. Figure 4 -------------------------------------------------------------

# Figure 4: Visual representation of the temporal trends of the 3 entomological
# surveillance metrics

# These metrics are represented across three neighborhoods in Iquitos:
# Punchana, Iquitos, and San Juan, which correspond to North Iquitos,
# Central Iquitos and South Iquitos, respectively, in the paper.

# There was no consistent human surveillance in this South Iquitos area, so the 
# incidence.p.1000 values are not shown. 

# ---- 4.1 Reshape surveillance data to long format ----

h.area.long <- h.area %>%
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
# NOTE:
# Because original observations are grouped by host (e.g., human/mosquito),
# some area-date-variable combinations appear multiple times (e.g., NA for both hosts).
# After reshaping to long format, these duplicated NA combinations remain,
# unless explicitly filtered out.

# Aedes DENV prevalence and vector index share the left y-axis
# DENV infection/1000 people & average vector abundance share the right y-axis.

# ---- 4.2 Plot

fig4 <- h.area.long %>%
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
    strip.text.y.left = element_text(angle = 90),
    panel.spacing = unit(0.6, "lines"),
    panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.4),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 6), 
    axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 10)
  )

# Seave output area plots with legend
ggsave(here("analysis","outputs", "figures", "fig4.jpg"),
       fig4,
       width = 280, height = 175, dpi = 500, units = "mm")


# ---- 5. Figure 5 -------------------------------------------------------------

# Figure 5: A) and B) show the results from a model measuring the temporally
# lagged association between Ae. aegypti female abundance and Ae. aegypti female
# DENV prevalence.

# ---- 5.1 Panel A) ----

# Create panel A: Effect sizes (ORs) of Ae. aegypti female abundance
# on  Ae. aegypti vector DENV prevalence

m.beta.fe.plot <- m.lag.fe.df %>%
  mutate(variable=  "Ae. aegypti \nfemale abundance") %>%
  filter(model.structure =="combined effect of weighted week lags" &
           parameter== "beta") %>%
  # Plot estimated fixed effects (mean ORs and 95% CI)
  ggplot() +
  geom_point(aes(y = variable, x = mean.exp),
             size = 2, alpha = 1, position = position_dodge(width = 1)) +
  geom_errorbarh(aes(y = variable, xmin = q2.5.exp, xmax = q97.5.exp),
                 alpha = 1, height = .2, position = position_dodge(width = 1)) +
  geom_vline(xintercept = 1, linetype = "dashed") +  # Reference line at OR = 1
  labs(
    subtitle = "Total effect of vector abundance \non vector DENV prevalence",
    y = NULL, x = "Odds ratio (95% CI)"
  ) +
  theme_bw(base_size = 10, base_family = "Arial") +
  guides(color = "none")

# ---- 5.2 Panel B) ----

# Create panel B: Estimated values with 95% credible intervals of
# the weight (w) parameters, representing the relative importance of
# each lagged measurement of the Ae. aegypti abundance on vector DENV prevalence.

m.lag.fe.plot <- m.lag.fe.df %>%
  filter(model.structure =="combined effect of weighted week lags" &
           parameter!= "beta") %>%
  # Plot estimated weight effects and 95% CI
  ggplot() +
  geom_col(aes(x = fct_rev(lag), y = summary.mean),
           size = 1, alpha = 0.6, position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(aes(x = fct_rev(lag),
                    ymin = summary.2.5.,
                    ymax = summary.97.5.),
                alpha = 1,width = .5,
                position = position_dodge(width = 0.5),
                size = 1) +
  ylim(0,1)+
  labs(
    subtitle = "Relative importance of weekly-lagged vector abundance \non vector DENV prevalence",
    x = "Weekly lag",
    y = "Relative weight (95% CI)"
  ) +
  ylim(0,1)+
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

# ---- 4.5 Combine all panels ----

layout <- "
AABBBBB
"

fig.5.plot <-
  m.beta.fe.plot +
  m.lag.fe.plot+
  plot_layout(guides = "collect", design= layout)+
  plot_annotation(  tag_levels = 'A')

# Save Figure 5 plot
ggsave(here("analysis", "outputs", "figures", "fig5.jpg"), fig.5.plot,
       width = 260, height = 75, dpi = 500, units = "mm")

# ---- 6. Figure 6 -------------------------------------------------------------
# A) and B) show the results from models measuring the
# association between weekly lagged explanatory metrics of the Ae. aegypti
# population on dengue case incidence in the human population,
# where the candidate explanatory variables were:
# average Ae. aegypti female abundance, Ae. aegypti DENV prevalence (%)
# and vector index per 100 surveys.

# ---- 6.1 Panel A) ----

# Create panel A: Effect sizes (ORs) of vector metrics on human DENV incidence
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

# Plot panel A 
h.beta.fe.plot <- h.lag.fe.df %>%
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


# ---- 6.2 Panel B) ----

# Create panel B: Estimated values with 95% credible intervals of
# the weight (w) parameters, representing the relative importance of
# each lagged measurement of the vector metric on human DENV incidence.

h.lag.fe.plot <- h.lag.fe.df %>%
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

# ---- 6.3 Combine all panels ----

layout <- "
AABBBBB
AABBBBB
AABBBBB
"

fig.6.plot <-
  h.beta.fe.plot +
  h.lag.fe.plot+
  plot_layout(guides = "collect", design= layout)+
  plot_annotation(  tag_levels = 'A')

# Save Figure 6 plot
ggsave(here("analysis", "outputs", "figures", "fig6.jpg"), fig.6.plot,
       width = 260, height = 150, dpi = 500, units = "mm")


