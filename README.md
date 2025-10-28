
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![DOI](https://zenodo.org/ZENODO.svg)](https://doi.org/DOI)

# Detection of dengue virus in Aedes aegypti during an urban epidemic in Iquitos, Peru

This repository contains the data and code for our paper:

> Anna B. Kawiecki, Talia Wong, Danielle J. Harvey, Xiaoli Dong, Thomas W. Scott1, Amy C. Morrison, Christopher M. Barker (2025).
> *`Detection of dengue virus in Aedes aegypti during an urban epidemic in Iquitos, Peru`*.
> JOURNAL <https://doi.org/DOI>

--------------------------------------------------------------------------------
## Contents of the repository

The [:file_folder: analysis](/analysis) directory contains the scripts and outputs 
of the study analysis:

**`01.derived_data.R`**: `R` script to load and process data needed to perform 
the analysis and model fitting. This script calls datasets that are only available
upon request from the [:file_folder: raw_data](/analysis/data/raw_data) (this folder is
empty in this repository).
The script produces processed datasets at the household-level, that are also 
only available upon request, and at the city area-level, which are available in 
[:file_folder: area_level_data](/analysis/data/derived_data/area_level_data).

 
**`02.model_fitting.R`**: `R` script to fit models using R-INLA and STAN at the \
hosuehold and city area levels:

  * *Ae. aegypti* surveillance using a case-contact sampling strategy vs. 
broader surveillance (household-level models):
    * Association between surveillance strategy and *Ae. aegypti* abundance.
    * Association between surveillance strategy and probability
      of DENV detection in *Ae. aegypti*.
  * Association between *Ae. aegypti* abundance and probability of DENV detection
     (household-level models and area-level models).
  * Association between entomological metrics and DENV incidence in humans
     (area-level models).
 
**`03.model_results.R`**: `R` script to extract model outputs: 
estimated effect sizes and Watanabe-Akaike information criterion (WAIC) to measure 
goodness of fit. 

**`04.model_prior sensitivity.R`**: `R` script to perform prior sensitivity analysis
for Dirichlet-weighted 4-week lags area-level models in developed in STAN. 

[:file_folder: data](/analysis/data): Data used in the analysis.

- [:file_folder: raw_data](/analysis/data/raw_data): The datasets used in 
this project are not publicly available but can be shared upon request but were
originally stored in this subfolder. These datasets are called in the analysis code. 
The original data sets were: 
  * `ae.mid.rds`: Individual mosquito PCR data, including mosquito ID,
  household ID, date of collection, result of DENV PCR testing. 
  * `m.ae.pcr.rds`: Household entomological surveillance data, including
  household ID, mosquitoes collected, result of DENV PCR testing. 
  * `h.pcr.rds`: Human participant PCR data, including participant ID,
  household ID, result of DENV PCR testing.
  * `sf.moh.rds`: Iquitos Ministry of Health (MoH) polygons.

- [:file_folder: derived_data](/analysis/data/derived_data): This folder contains
processed datasets derived from the original datasets for analysis. 
Data aggregated by area of the city, is available in the sub-folder 
[:file_folder: area_level_data](/analysis/data/derived_data/area_level_data). 
Household-level data is available upon request and was originally stored in  [:file_folder:household_level_data](/analysis/data/derived_data/household_level_data) 
(this folder is empty in this repository).

The household-level data sets that are available upon request are: 

  * `d.m.rds`: Entomological surveillance household-level data for modelling with 
  numeric and factor IDs for relevant variables (required for some for certain
  formula structures in R-INLA).
  
  * `h.surv.rds`: Human participant PCR data
  
  * `h.surv.area.rds`: Human surveillance data assigned to city aggregation areas
  * `m.surv.rds`: Household entomological surveillance data with epiweeks and 
  surveillance categories
  
  * `m.surv.area.rds`: Household entomological surveillance data assigned to 
  city aggregation areas
  
  * `m.ae.mid.pcr.rds`: Mosquito surveillance merged with individual mosquito 
  PCR results
  
  * `m.h.surv.rds`: Combined human and mosquito PCR testing and surveillance 
  data with harmonized variables
  
  * `sf.city.poly.rds`: a polygon geometry representing the full extent 
  of the city of Iquitos (serves as the boundary for mesh construction for
  spatial analysis using INLA).


[:file_folder: functions](/analysis/functions): 
This folder contains:

  - **`model_functions.R`**: an `R` script with functions used to extract 
goodness-of-fit metrics and parameter estimates from fitted INLA and STAN models.
  - Pre-compiled function objects saved as `.rds` files for quick loading and reuse.

[:file_folder: outputs](/analysis/outputs): 
This folder contains:

  - **`figures.R`**: an `R` script that generates the figures included in the paper
  that are stored as `.jpg` files in [:file_folder: figures](/analysis/outputs/figures)

  - [:file_folder: figures](/analysis/outputs/figures): Stores `.jpg` files of 
  paper figures. 
  
  
  - [:file_folder: models](/analysis/outputs/models): Contains all fitted models
  and extracted model outputs produced in **`02.model_fitting.R`**,
  **`03.model_results.R`** and **`04.model_prior_sensitivity_analysis.R`** 
  as `.rds` files
  
  - **`results_and_tables.Rmd`** and **`results_and_tables.pdf`**: Contains the 
  code and output for generating the study's results and tables. Due to the use 
  of household-level data (available only upon request), the `.Rmd` file is 
  provided to demonstrate the code structure and analysis workflow, even without
  direct access to the datasets.
  
  - [:file_folder: tables](/analysis/outputs/tables): Contains tables produced
    in **`results_and_tables.Rmd`** as `.csv` files.

[:file_folder:supplementary-materials](/analysis/supplementary-materials):
Contains supplementary materials included in the paper:

 - **`supplementary_figures.R`**: `R` script used to generate the supplementary figures.
 - Corresponding output figures saved as `.jpg` files.

## How to run in your browser or download and run locally

This research compendium has been developed using the statistical
programming language R version 4.4.0 (2024-04-24). 

The repository contains **`aedes.denv.surveillance.Rproj`**, an `RStudio`
project file. To access the available data sets and code, download the repository
as a `.zip` file using the green button *Clone* or *download* above, then open 
the `.Rproj` file in `RStudio` to begin.

This project uses the [`renv`](https://rstudio.github.io/renv/) package to manage
the R environment and ensure reproducibility:

- **`renv.lock`**  
  This file records the exact versions of all R packages used during the analysis.
  It enables anyone to recreate the same environment.

- **`renv/` folder**  
  This folder contains metadata and the local project-specific R package library
  managed by `renv`. It is automatically generated and used to isolate the project
  environment from your global R setup.

To recreate the environment used for this analysis:

1. Open the project in RStudio.
2. Run the following in the R console:

   ```r
   install.packages("renv")
   renv::restore()
```

This project uses two packages not available on CRAN:

- **`INLA` version `24.05.10`**
- **`cmdstanr` version `0.7.1`**

These packages may not be restored automatically via `renv::restore()` due to 
their non-standard installation sources. Please install them manually.

If you want to replicate the exact environment this project was run in you will 
require an older version, whihc you will need to locate a 
local or archived source and install it manually. You can find older versions at 
the [INLA download website](https://www.r-inla.org/download-install) 

```r
# To install version 24.05.10 of INLA
remotes::install_version("INLA", version = "24.05.10",
repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/testing"),
dep = TRUE)

# To install version 0.7.1 of cmdstan
cmdstanr::install_cmdstan(version = "0.7.1")
```

## How to cite

Please cite this compendium as:

> Kawiecki, (2025). *Compendium of R code and data for
> `Detection of dengue virus in Aedes aegypti during an urban epidemic in Iquitos, Peru`*.
> Accessed [Date]. Online at
> <https://doi.org/>

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** De-identified data sets are available upon request to the
corresponding author.
