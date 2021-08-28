# Packages and shapefile -------------------------------------------------------
## NUTS3, excluded oversea regions/countries, calculate area, add Kosovo and Bosnia
source("code/00_libraries_functions.R")


# Download and prepare data ----------------------------------------------------
## common standard: long tibble/data.frame, with year|nuts3_id|value|indicator

## EDGAR: GHG emissions
source("code/01_download_EDGAR.R")

## Eurostat: all independent variables
source("code/02_download_eurostat.R") 

# ## AGRI4CAST: Heating and cooling days
# source("code/03_download_temp_heat_cool.R") 
# -> HDD and CDD are now added via eurostat


# Combine data -----------------------------------------------------------------
source("code/04_combine_data.R")


# Provide and visualise input data ---------------------------------------------
source("code/05_visualise_input.R")


# Run OLS ----------------------------------------------------------------------
source("code/06_model_OLS.R")

# Prepare and run GWR ----------------------------------------------------------
source("code/07_model_GWR.R")

# Run spatial tests ------------------------------------------------------------
source("code/08_spatial_tests.R")

# Run spatial models -----------------------------------------------------------
source("code/09_model_spatial.R")

# Run (spatial) panel models ---------------------------------------------------
source("code/10_panel.R")
