# Packages and shapefile -------------------------------------------------------
## NUTS3, exclude oversea regions/countries, calculate area, add Kosovo and Bosnia
source("code/00_libraries_functions.R")


# Download and prepare data ----------------------------------------------------

## EDGAR: GHG emissions
source("code/01_download_EDGAR.R")

## Eurostat: all independent variables
source("code/02_download_eurostat.R") 


# Combine data -----------------------------------------------------------------
source("code/03_combine_data.R")


# Provide and visualise input data ---------------------------------------------
source("code/04_visualise_input.R")


# Run OLS ----------------------------------------------------------------------
source("code/05_model_OLS.R")

# Run spatial tests ------------------------------------------------------------
source("code/06_spatial_tests.R")

# Run spatial cross-sectional models -------------------------------------------
source("code/07_model_spatial.R")

# Run spatial panel models -----------------------------------------------------
source("code/08_panel.R")
