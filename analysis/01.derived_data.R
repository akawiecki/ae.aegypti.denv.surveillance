# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## DATA PROCESSING =============================================================

# Description:
#     Process entomological and human surveillance data for modelling.

# Paper:
#     Detection of dengue virus in Aedes aegypti during an urban epidemic in Iquitos, Peru
#     (December 2010 to March 2011)

# Script author:
#     Anna B. Kawiecki        ORCID: 0000-0002-0499-2612

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# ---- 0. Load R libraries -----------------------------------------------------

<<<<<<< HEAD
# Handles relative file paths
library(here)

# R Markdown report generation
library(knitr)

# Data manipulation and ggplot2 graphics
library(tidyverse)

# Simplifies working with dates and times
library(lubridate)

# Converts ISO weeks to calendar dates
library(ISOweek)

# Spatial data handling for mapping
library(sf)

=======
library(pacman)

pacman::p_load(
  here,       # Handles relative file paths
  knitr,      # R Markdown report generation
  tidyverse,  # Data manipulation and ggplot2 graphics
  lubridate,  # Simplifies working with dates and times
  ISOweek,    # Converts ISO weeks to calendar dates
  sf          # Spatial data handling for mapping
)
>>>>>>> 039226c2534a9e37020f42822743fb30c9c8b648

# ---- 1. DATA PROCESSING ------------------------------------------------------

# ---- 1.1 Read in data ----

# Individual mosquito PCR data: mosquito ID, household ID, date, PCR results
ae.mid <- readRDS(here("analysis", "data", "raw_data","ae.mid.rds") )

# Household entomological surveillance data:
# household ID, mosquitoes collected, PCR results
m.ae.pcr <- readRDS(here("analysis", "data", "raw_data", "m.ae.pcr.rds") )

# Human participant PCR data: participant ID, household ID, PCR results
h.pcr <- readRDS(here("analysis", "data", "raw_data", "h.pcr.rds") )

# Ministry of Health (MoH) polygons
sf.moh <- readRDS(here("analysis", "data", "raw_data", "sf.moh.rds") )

# ---- 1.1.1 Merge mosquito surveillance and individual mosquito PCR results ---

m.ae.mid.pcr <- m.ae.pcr %>%
  left_join(ae.mid[, c("date.naa.loc.num",
 "m.dissected", "pcr.positive.body", "pcr",
"n.body.parts.tested", "mosquito_id"
)], by = "date.naa.loc.num")

# Save as .rds file:
saveRDS(m.ae.mid.pcr,here("analysis", "data", "derived_data", "household_level_data",
                    "m.ae.mid.pcr.rds"))

# ---- 1.2 Process dates ----

# ---- 1.2.1  Create breaks in the time series ----

# Monthly breaks
monthly_breaks <- seq.Date(from = as.Date("2010-10-01"),
                           to = as.Date("2011-05-01"),
                           by = "months")

# Weekly sequence for the entire outbreak
week_breaks_outbreak <- seq.Date(from = as.Date("2010-09-01"),
                           to = as.Date("2011-05-01"),
                           by = "week")

# Weekly sequence for the time period where Ae. aegypti females were tested
week_breaks <- seq.Date(from = as.Date("2010-12-01"),
                           to = as.Date("2011-03-31"),
                           by = "week")

# ---- 1.2.2 Order epiweeks ----

# Order the epiweeks such that they are ordered from December 2010 to March 2011
# and not from March to December:

# Epiweek order for the entire outbreak
epiweek.order = c(seq(35,52, by = 1),seq(1,17, by = 1) )

# Epiweek order for the time period where Ae. aegypti females were tested
epiweek.m.test.order = c(seq(48,52, by=1),seq(1,13, by=1) )


# ---- 1.2.3 Data frame of dates, epiweeks and Mondays  ----

# Create a data frame of sequential days with the corresponding epiweeks and the
# Monday of the epiweek for the time period where Ae. aegypti females were tested
# (from 2010-11-29 to 2011-03-31):

# Create a daily sequence of dates from 2010-11-29 to 2011-03-31:
date_sequence <- seq(ymd("2010-11-29"), ymd("2011-03-31"), by = "1 day")

# Create a dataframe with each date, epiweek and corresponding Monday:
epiweeks.testing <- as.data.frame(date_sequence) %>%
  # ISO week number (weeks start on Monday)
  mutate(epiweek= isoweek(date_sequence),
         # date.surv = the Monday of the epiweek
         date.surv = case_when(
           # Monday = 2 in lubridate::wday with default settings
           wday(date_sequence) == 2 ~ date_sequence
         )
  ) %>%
  group_by(epiweek) %>%
  # Fill in missing Monday values within each epiweek group
  fill(date.surv)

# ---- 1.2.4 Process dates in entomological surveillance data  ----

# Add epiweeks and a representative date for each epiweek (the Monday of each week),
# as well as categories for surveillance type. Where epiweeks span multiple months,
# assign the month with the highest number of observations.

# Household entomological surveillance data:
# epiweeks, Mondays and surveillance categories
m.surv = m.ae.pcr %>%
  # Flag households where entomological surveys occurred (worked = "yes")
  mutate(worked= case_when(
    status == "WORKED" ~ "yes",
    T ~ "no"
  )) %>%
  # Replace NA in Ae. aegypti adult collection columns with 0
  # (where 0 mosquitoes were collected)
  mutate_at(vars(starts_with("aa_")), ~if_else(is.na(.), 0, .)) %>%
  # Flag whether female Ae. aegypti adults were tested by PCR (tested = "yes")
  mutate(tested = case_when(
    !is.na(denv.house) ~ "yes",
    TRUE ~ "no"
  )) %>%
  # Keep only households where entomological surveys occurred (successful surveys)
  filter(worked == "yes") %>%
  # Extract longitude and latitude from geometry object
  mutate(longitude = sf::st_coordinates(.)[, 1],
         latitude = sf::st_coordinates(.)[, 2]) %>%
  # Remove geometry column after extracting coordinates
  st_drop_geometry() %>%
  # Join with epiweek and Monday date information
  left_join(epiweeks.testing, by= c("date" = "date_sequence")) %>%
  # Categorize surveillance strategies
  mutate(pos.index= case_when(
    site == "index" & cluster_type == "pos" ~ "positive index",
    T ~ "other surveillance"
  )) %>%
  mutate(pos.case.contact= case_when(
    cluster_type == "pos" ~ "positive case-contact",
    T ~ "other surveillance"
  )) %>%
  mutate(pos.neg.case.contact= case_when(
    cluster_type == "pos" ~ "positive case-contact",
    cluster_type == "neg" ~ "negative case-contact",
    T ~ "other surveillance"
  )) %>%
  mutate(surv.strategy= case_when(
    !is.na(site) ~ "case-contact",
    is.na(site) ~ "general surveillance"
  )) %>%
  # Classify date into outbreak "moment" (early/peak/waning)
  mutate(outbreak.moment= case_when(
    date.surv < ymd("2011-01-01") ~ "early",
    date.surv  >= ymd("2011-01-01") & date.surv  < ymd("2011-02-01") ~ "peak",
    date.surv  >= ymd("2011-02-01") ~ "waning",
  )) %>%
  # Arrange data by epiweek from December 2010 to March 2011
  arrange(factor(epiweek, levels = epiweek.order))


# Identify cases where the same epiweek is assigned to dates in two different months
# Clean duplicate epiweeks
m.surv.dup <- m.surv %>%
  group_by(year,month,epiweek,date.surv) %>%
  summarise(n = n()) %>%
  ungroup() %>%

  # For each date.surv (i.e. each epiweek Monday), check if it appears in two months
  group_by(date.surv) %>%
  mutate(n.unique = n()) %>%

  # Select the month with more observations if epiweek spans two months
  mutate(month.larger= case_when(
    n.unique == 2 & n[2]>n[1] ~ month[2],
    n.unique == 2 & n[1]>n[2] ~ month[1],
    T~ NA
  )) %>%

  # Keep only dates where adjustment is needed
  filter(!is.na(month.larger)) %>%
  dplyr::select(c("date.surv", "month.larger")) %>%
  distinct()

# Apply corrections to the main dataset
m.surv <- m.surv %>%
  # Replace with corrected month
  left_join(m.surv.dup[, c("date.surv", "month.larger")], by = "date.surv") %>%
  mutate(
    month = case_when(
      !is.na(month.larger) ~ month.larger,
      T~ month
    )) %>%
  dplyr::select(-month.larger) %>%

  # Convert denv.house status to numeric (1 for positive, 0 for negative)
  mutate(denv.house.num = case_when(
    denv.house=="positive"~1,
    denv.house=="negative"~ 0
  ))

# Save as .rds file:
saveRDS(m.surv,here("analysis", "data", "derived_data", "household_level_data",
                    "m.surv.rds"))


# ---- 1.2.5 Process dates in human participant PCR data  ----

# Create data frame with epiweeks and a representative date for each epiweek
# (the Monday of each week), for the time period where Ae. aegypti females were tested
h.date.surv <- h.pcr %>%
  st_drop_geometry() %>%
  # Isolate unique year-epiweek combinations
  distinct(year, epiweek) %>%
  mutate(
    # Construct ISO week string (e.g., "2011-W05")
    iso_week = sprintf("%04d-W%02d", year, epiweek),
    # Convert ISO week to Monday date (e.g., "2011-W05-1")
    # in "YYYY-Www-d" format where 1 = Monday
    date.surv = ISOweek2date(paste0(iso_week, "-1"))
  ) %>%
  arrange(factor(epiweek, levels = epiweek.order))

# Human participant PCR data: participant ID, household ID, PCR results
h.surv <- h.pcr %>%

  # Remove geometry column from the spatial dataframe (e.g., for easier manipulation or plotting)
  st_drop_geometry() %>%

  # To assign a representative date to each epiweek
  # join to h.date.surv (with epiweek week and the corresponding Monday)
  left_join(h.date.surv[,c("year", "epiweek", "date.surv")],
            by = c("year", "epiweek")) %>%

  # Classify each observation into one of the outbreak periods
  mutate(outbreak.moment = case_when(
    # Before Jan 1, 2011
    date.surv < ymd("2011-01-01") ~ "early",
    # Jan 1 to Jan 31
    date.surv >= ymd("2011-01-01") & date.surv < ymd("2011-02-01") ~ "peak",
    # Feb 1 and later
    date.surv >= ymd("2011-02-01") ~ "waning"
  )) %>%

  # Order the data by epiweek (in custom-defined order)
  arrange(factor(epiweek, levels = epiweek.order))

# Save data set as .rds file
saveRDS(h.surv, here("analysis", "data", "derived_data","household_level_data",
                     "h.surv.rds"))


# ---- 1.3 Combine entomological and human participant surveillance  ----

# Generate data set with harmonized variables for human PCR testing and
# mosquito PCR testing.

# Generate harmonized PCR testing values for the human surveillance data:
# Human surveillance data aggregation per epiweek and house
 h.merge <- h.surv %>%
   # Keep only human data from epiweeks where mosquito PCR testing was also performed
   filter(epiweek %in% seq(48, 52, by = 1) | epiweek %in% seq(1, 13, by = 1)) %>%
   # Arrange data by epiweek using a pre-defined order for plotting or analysis
   arrange(factor(epiweek, levels = epiweek.m.test.order)) %>%
   # Group by epiweek and household
   group_by(year, month, epiweek, date.surv, moh, location_code, longitude, latitude) %>%
  # Summarize human-level data
reframe(
  # Number of individuals under surveillance per epiweek/household
  n.ind = n(),
  # Number of individuals tested for DENV by PCR per epiweek/house
  n.tested = sum(!is.na(result)),
  # Number of DENV PCR-positive individuals per epiweek/house
  n.denv = sum(result == "positive", na.rm = TRUE)
) %>%
   # Add a column to indicate the host type
   mutate(host = "human") %>%
   # Ensure year and month are character strings (useful for plotting or labeling)
   mutate(
     year = as.character(year),
     month = as.character(month)
   )

# Generate harmonized PCR testing values for the entomological surveillance data:
# Human surveillance data aggregation per epiweek and house
# Entomological surveillance data aggregation per epiweek and house
 m.merge <- m.surv %>%
   # Group by the same spatial-temporal identifiers as for human data
   group_by(year, month, epiweek, date.surv, moh, location_code, longitude, latitude) %>%
   # Summarize mosquito-level data
   reframe(
     # Total number of female Ae. aegypti collected per epiweek/household
     n.ind = sum(aa_female, na.rm = TRUE),
     # Number of female Ae. aegypti tested by PCR per epiweek/household
     n.tested = sum(n, na.rm = TRUE),
     # Number of DENV PCR-positive female Ae. aegypti per epiweek/household
     n.denv = sum(n.pos.mosq, na.rm = TRUE)
   ) %>%
   # Add a column to indicate the host type
   mutate(host = "mosquito")

# Combined human and mosquito PCR testing and surveillance data with harmonized variables
 m.h.surv <- bind_rows(h.merge, m.merge)

 # Convert to `sf` spatial object using longitude and latitude as coordinates
 # UTM zone 18S
 m.h.surv <- st_as_sf(m.h.surv, coords = c("longitude", "latitude"),
                      crs = 32718)

 # Add back lat-long columns separately (if needed for mapping with `ggplot2`, leaflet, etc.)
 m.h.surv.latlong <- m.h.surv %>%
   mutate(
     longitude = sf::st_coordinates(.)[, 1],
     latitude = sf::st_coordinates(.)[, 2]
   )

 # Overwrite the spatial object with updated version that includes lat/long columns
 m.h.surv <- m.h.surv.latlong

# Save data set as .rds file
 saveRDS(m.h.surv, here("analysis", "data", "derived_data","household_level_data",
                        "m.h.surv.rds"))


# ---- 2.DATA FOR HOUSEHOLD-LEVEL MODELS ---------------------------------------

# Read in processed data
 m.surv <- readRDS(here("analysis", "data", "derived_data","household_level_data",
                        "m.surv.rds"))
 h.surv <- readRDS(here("analysis", "data", "derived_data","household_level_data",
                        "h.surv.rds"))
 m.h.surv <- readRDS(here("analysis", "data", "derived_data","household_level_data",
                          "m.h.surv.rds"))

# ---- 2.1 Entomological surveillance household-level data for modelling   ----

# Create numeric and factor IDs for relevant variables.
# This is required for some for certain formula structures in R-INLA.
d.m <- m.surv %>%
  # Convert outbreak phase (early/peak/waning) into a numeric variable for modeling/plotting
  mutate(outbreak.t = case_when(
    outbreak.moment == "early" ~ 1,
    outbreak.moment == "peak" ~ 2,
    outbreak.moment == "waning" ~ 3
  )) %>%
  # Recode and convert variables to factors/numerics
  mutate(
    # Outbreak phase as factor
    outbreak.moment = as.factor(outbreak.moment),
    # Surveillance type as binary factor: 1 = positive case-contact; 0 = other surveillance
    pos.case.contact.f = as.factor(case_when(
      pos.case.contact == "other surveillance" ~ "0",
      TRUE ~ "1"
    )),
    # Surveillance type as character/numeric
    surv = case_when(
      pos.case.contact == "other surveillance" ~ "0",
      TRUE ~ "1"
    ),
    # Convert month into a numeric time variable (1 = Dec, ..., 4 = Mar)
    month.t = case_when(
      month == "12" ~ 1,
      month == "1" ~ 2,
      month == "2" ~ 3,
      month == "3" ~ 4
    )
  ) %>%
  # Create NA-marked binary flags for specific surveillance types
  mutate(
    # Only keep 1 for "other surveillance", then turn 0s into NA
    surv.general = na_if(if_else(pos.case.contact == "other surveillance", 1, 0), 0),

    # Only keep 1 for "positive case-contact", then turn 0s into NA
    surv.pos.cc = na_if(if_else(pos.case.contact == "positive case-contact", 1, 0), 0)
  )

# Save as .rds file
saveRDS(d.m, here("analysis", "data", "derived_data","household_level_data",
                  "d.m.rds"))


# ---- 2.2 Process spatial data for modelling household-level data   ----

# To measure spatial autocorrelation in the model using R-INLA,
# we first need to generate a spatial mesh over the study area.
# This requires a polygon geometry representing the full extent
# of the city of Iquitos, which serves as the boundary for mesh construction.

# Read in the MoH polygons
sf.moh <- st_as_sf(readRDS( here("analysis", "data", "raw_data", "sf.moh.rds")))

# Create a polygon of the entire city of Iquitos using the union of the MoH polygons.
sf.city.poly <- st_union(sf.moh) %>% st_sf()

# Visualize single polygon of the city of Iquitos
plot(sf.city.poly)

# Save as .rds file
saveRDS(sf.city.poly, here("analysis", "data", "derived_data","household_level_data",
                           "sf.city.poly.rds"))

# ---- 3. DATA FOR AREA-LEVEL MODELS ---------------------------------------

# To enable spatio-temporal modelling, test results were grouped into three large,
# contiguous geographic areas with consistent entomological and human DENV testing
# throughout the study period: North Iquitos, Central Iquitos, and South Iquitos.

# Metrics are represented across three neighborhoods in Iquitos:
# Punchana, Iquitos, and San Juan, which correspond to North Iquitos,
# Central Iquitos and South Iquitos, respectively, in the paper.

# Read in processed data
m.h.surv <- readRDS(here("analysis", "data", "derived_data","household_level_data",
                         "m.h.surv.rds"))

# ---- 3.1 Process entomological data for modelling at area-level   ----

# ---- 3.1.1 Assign entomological observations to aggregation areas   ----

m.surv.area <- m.h.surv %>%
   filter(host == "mosquito") %>%
  # Household had some DENV PCR-positive female Ae. aegypti in epiweek
  mutate(denv = case_when(
    n.denv > 0 ~ "positive",
    T~ "negative"
  )) %>%
  # Aggregate observations into areas with consistent surveillance
  mutate(area= case_when(
    moh %in% c("29","32","33","34",
               "22","23","24" ,"25","26","27","28","30","31") ~ "san.juan",
    moh %in% c("4","11","6","9","12","13","10","8","7","3","5","2","1") ~ "punchana",
    moh %in% c("17","14","19","18","20","21",
               "16","15") ~ "iquitos"
  ))

# Save as .rds file
saveRDS(m.surv.area, here("analysis", "data", "derived_data","household_level_data",
                          "m.surv.area.rds"))

# ---- 3.1.2 Summarise entomological observations by aggregation area   ----

# Entomological surveillance observations summarized by area
m.area <- m.surv.area %>%
  # Remove the geometry column
  st_drop_geometry() %>%
  # Group by area and date
  group_by(area, year, epiweek, date.surv) %>%
  # Summarize mosquito data per area
  summarize(
    n.surveys = n(),                        # Count number of mosquito collection surveys
    n.ind = sum(n.ind, na.rm = TRUE),       # Total number of female Ae. aegypti collected per epiweek/area
    n.tested = sum(n.tested, na.rm = TRUE), # Total number of female Ae. aegypti tested by PCR per epiweek/area
    n.denv = sum(n.denv, na.rm = TRUE)      # Total number of DENV PCR-positive female Ae. aegypti per epiweek/area
  ) %>%
  ungroup() %>%
  # Wrangle NAs and 0s
  mutate(
    # If no mosquitoes were tested in a area, convert 0s to NA (so 0 tested is not confused with 0 positives)
    n.tested = na_if(n.tested, 0),
    n.denv = case_when(
      # If no mosquitoes were tested, the number of DENV PCR-positive female Ae. aegypti per epiweek/area should be NA
      is.na(n.tested) ~ NA,
      # Otherwise, keep the value
      TRUE ~ n.denv
    )
  ) %>%
  # Calculate derived metrics:
  mutate(
    prevalence = n.denv / n.tested,     # Proportion of tested female Ae. aegypti that were DENV positive
    avg.aa.f = n.ind / n.surveys        # Average number of individuals per mosquito collection survey
  ) %>%
  # Convert prevalence to percentage scale
  mutate(
    prevalence.by.pop = prevalence * 100
  ) %>%
  # Group again by area to prepare for further aggregation
  group_by(area) %>%
  # Count how many epiweeks of data are present per area
  mutate(n.epiweeks = n())

# Save as .rds file
saveRDS(m.area, here("analysis", "data", "derived_data","area_level_data",
                     "m.area.rds"))

# ---- 3.1.3 Generate weekly lagged mosquito metrics by area   ----

# Here we don't use functions like `dplyr::lag()` given that the time series
# is not contiguous.

# Create lagged covariates of average adult female mosquito abundance (avg.aa.f)
# to assess associations with dengue prevalence at various prior time points.
m.area.week.lag <- m.area %>%
  # Join with itself to align each surveillance record (date.surv.x)
  # with previous avg.aa.f values (from date.surv.y) in the same area.
  left_join(m.area[, c("area", "date.surv", "avg.aa.f")], by = "area", relationship = "many-to-many") %>%
  # Compute time lag in weeks between the current record and the lagged observation
  mutate(week_lag = as.numeric((date.surv.x - date.surv.y) / 7)) %>%
  # Sort by area and lag time for consistent structure
  arrange(area, week_lag) %>%
  # Retain only lags from the past or same week (i.e., exclude future values)
  filter(week_lag >= 0) %>%
  # Drop the lagged date column (no longer needed after calculating lag)
  dplyr::select(-c("date.surv.y")) %>%
  # Reshape to wide format: one column per lagged week of avg.aa.f (e.g., week_lag_0, week_lag_1, ...)
  pivot_wider(
    names_from = week_lag,
    values_from = avg.aa.f.y,
    names_prefix = "week_lag_"
  ) %>%
  # Rename original columns for clarity
  rename(
    date.surv = date.surv.x,
    avg.aa.f  = avg.aa.f.x
  ) %>%
  # Keep only records with non-missing dengue prevalence
  filter(!is.na(prevalence))

# Save as an RDS file
saveRDS(m.area.week.lag, here("analysis", "data", "derived_data", "area_level_data",
                              "m.area.week.lag.rds"))

# Select only weekly lags 0-4.
m.area.week.lag.4 <- m.area.week.lag %>%
  filter(!is.na(week_lag_1) & !is.na(week_lag_2) & !is.na(week_lag_3)& !is.na(week_lag_4))

# Save as an RDS file
saveRDS(m.area.week.lag.4, here("analysis", "data", "derived_data","area_level_data",
                                "m.area.week.lag.4.rds"))


# ---- 3.2 Process human surveillance data for modelling at area-level   ----

# ---- 3.2.1 Assign human surveillance observations to aggregation areas   ----

# Here we assign entomological and human surveillance observations to aggregation areas:
h.surv.area <- m.h.surv %>%
  mutate(area= case_when(
    moh %in% c("29","32","33","34",
               "22","23","24" ,"25","26","27","28","30","31") ~ "san.juan",
    moh %in% c("4","11","6","9","12","13","10","8","7","3","5","2","1") ~ "punchana",
    moh %in% c("17","14","19","18","20","21",
               "16","15") ~ "iquitos"
  ))

# Save as an RDS file
saveRDS(h.surv.area,  here("analysis", "data", "derived_data","household_level_data",
                           "h.surv.area.rds"))

# ---- 3.2.2 Summarise human surveillance observations by aggregation area ----

# Human surveillance observations summarized by area
h.area <- h.surv.area %>%
  # Remove the geometry column
  st_drop_geometry() %>%
  # Group by area and date
  group_by(area, epiweek, date.surv, host) %>%
  # Summarize human and entomological data per area
  summarize(
    n.surveys= n(),  # Count number of surveys
    # Total number of individuals per epiweek/area
    n.ind= sum(n.ind, na.rm= TRUE),
    # Total number of individuals tested by PCR per epiweek/area
    n.tested= sum(n.tested, na.rm= TRUE),
    # Total number of DENV PCR-positive individuals per epiweek/area
    n.denv= sum(n.denv, na.rm= TRUE)
    ) %>%
  ungroup() %>%
  # Wrangle NAs and 0s
  mutate(
    # If no individuals were tested in a area, convert 0s to NA
    n.tested= na_if(n.tested,0),
    # If no individuals were tested, the number of DENV PCR-positive individuals
    # per epiweek/area should be NA
    n.denv= case_when(
      is.na(n.tested) ~ NA,
      T~ n.denv)) %>%
    # Calculate derived metrics:
  mutate(prevalence= case_when(
    # Proportion of tested female Ae. aegypti that were DENV positive
    host=="mosquito"~ n.denv/n.tested,
    # Proportion of tested human participants that were DENV positive
    host=="human"~ n.denv/n.ind)
    ) %>%
 # Convert human incidence to infected individuals per 1000 individuals under surveillance
  mutate(incidence.p.1000= case_when(
    host=="human"~ prevalence*1000,
    T ~ NA
  )) %>%
  # Convert mosquito DENV prevalence to percentage scale
  mutate(prevalence.p.100= case_when(
    host=="mosquito"~ prevalence*100,
    T ~ NA
  )) %>%
  # Average number of mosquitoes per mosquito collection survey
  mutate(avg.aa.f= case_when(
    host=="mosquito"~  n.ind/n.surveys,
    host=="human"~ NA
  )) %>%
  # Calculate vector index
  mutate(vi= case_when(
    host=="mosquito"~  avg.aa.f*prevalence*100,
    host=="human"~ NA))

# Save as an RDS file:
saveRDS(h.area,  here("analysis", "data", "derived_data", "area_level_data",
                      "h.area.rds"))

# ---- 3.2.2 Generate weekly lagged mosquito metrics and human DENV incidence ----

# Subset data to include only rows where the host is human
h.area.h <- h.area %>%
  filter(host == "human")

# Subset data to include only rows where the host is mosquito
h.area.m <- h.area %>%
  filter(host == "mosquito")

# Join datasets to calculate weekly lagged mosquito metrics vs. human DENV incidence
h.area.week.lag <- h.area.h %>%
  # Join with mosquito prevalence data by area
  left_join(h.area.m[, c("area", "date.surv", "prevalence.p.100")], by = "area",
            relationship = "many-to-many") %>%
  # Calculate the week lag between human and mosquito data
  mutate(week_lag = as.numeric((date.surv.x - date.surv.y) / 7)) %>%
  # Arrange data by area and week lag
  arrange(area, week_lag) %>%
  # Retain only lags from the past or same week (i.e., exclude future values)
  filter(week_lag >= 0) %>%
  # Remove redundant date column
  dplyr::select(-c("date.surv.y")) %>%
  # Reshape data: create separate columns for each week's lagged prevalence
  pivot_wider(names_from = week_lag, values_from = prevalence.p.100.y,
              names_prefix = "prev.week_lag_") %>%
  # Rename columns for consistency
  rename(date.surv = date.surv.x,
         prevalence.p.100 = prevalence.p.100.x) %>%
  # Remove rows with missing human prevalence values
  filter(!is.na(prevalence)) %>%
  # Join with average number of adult female Ae. aegypti (avg.aa.f) by area
  left_join(h.area.m[, c("area", "date.surv", "avg.aa.f")], by = "area",
            relationship = "many-to-many") %>%
  # Calculate week lag again for avg.aa.f variable
  mutate(week_lag = as.numeric((date.surv.x - date.surv.y) / 7)) %>%
  # Arrange by area and lag
  arrange(area, week_lag) %>%
  # Retain only lags from the past or same week (i.e., exclude future values)
  filter(week_lag >= 0) %>%
  # Drop extra date column
  dplyr::select(-c("date.surv.y")) %>%
  # Pivot wider for avg.aa.f at different lags
  pivot_wider(names_from = week_lag, values_from = avg.aa.f.y,
              names_prefix = "avg.aa.f.week_lag_") %>%
  # Rename the date column
  rename(date.surv = date.surv.x) %>%
  # Join with vector index (vi) data by area
  left_join(h.area.m[, c("area", "date.surv", "vi")], by = "area",
            relationship = "many-to-many") %>%
  # Compute lag between human and mosquito vi data
  mutate(week_lag = as.numeric((date.surv.x - date.surv.y) / 7)) %>%
  # Arrange by area and lag
  arrange(area, week_lag) %>%
  # Retain only lags from the past or same week (i.e., exclude future values)
  filter(week_lag >= 0) %>%
  # Remove extra date column
  dplyr::select(-c("date.surv.y")) %>%
  # Pivot wider for virus index at different lags
  pivot_wider(names_from = week_lag, values_from = vi.y, names_prefix = "vi.week_lag_") %>%
  # Rename date column again for consistency
  rename(date.surv = date.surv.x) %>%
  # Remove rows with missing human prevalence
  filter(!is.na(prevalence)) %>%
  # Drop unneeded columns including original mosquito variables and high-lag values
  dplyr::select(-c(
    "avg.aa.f.x", "vi.x",
    "prev.week_lag_7", "prev.week_lag_8", "prev.week_lag_9",
    "prev.week_lag_10", "prev.week_lag_11", "prev.week_lag_12",
    "prev.week_lag_13", "prev.week_lag_14", "prev.week_lag_15",
    "prev.week_lag_16", "prev.week_lag_17",
    "avg.aa.f.week_lag_7", "avg.aa.f.week_lag_8", "avg.aa.f.week_lag_9",
    "avg.aa.f.week_lag_10", "avg.aa.f.week_lag_11", "avg.aa.f.week_lag_12",
    "avg.aa.f.week_lag_13", "avg.aa.f.week_lag_14", "avg.aa.f.week_lag_15",
    "avg.aa.f.week_lag_16", "avg.aa.f.week_lag_17",
    "vi.week_lag_7", "vi.week_lag_8", "vi.week_lag_9",
    "vi.week_lag_10", "vi.week_lag_11", "vi.week_lag_12",
    "vi.week_lag_13", "vi.week_lag_14", "vi.week_lag_15",
    "vi.week_lag_16", "vi.week_lag_17"))


# Save as an RDS file
saveRDS(h.area.week.lag,  here("analysis", "data", "derived_data","area_level_data",
                               "h.area.week.lag.rds"))

# Select only weekly lags 0-4.
h.area.week.lag.4 <- h.area.week.lag %>%
  # Select only weekly lags 0-4
  filter(!is.na(prev.week_lag_0) &
           !is.na(prev.week_lag_1) &
           !is.na(prev.week_lag_2) &
           !is.na(prev.week_lag_3) &
           !is.na(prev.week_lag_4)) %>%
  # Remove the San Juan area
  filter(area!="san.juan")

# Save as an RDS file:
saveRDS(h.area.week.lag.4,
        here("analysis", "data", "derived_data","area_level_data",
                                "h.area.week.lag.4.rds"))

