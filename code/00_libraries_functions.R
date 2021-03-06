rm(list = ls()) # clean environment

# Load libraries ---------------------------------------------------------------
# install.packages("pacman")
library(pacman)

p_load(tidyverse)
p_load(eurostat)
p_load(stringr)
p_load(sf)
p_load(raster)
p_load(rgdal)
p_load(ncdf4)
p_load(foreach)
p_load(doMC)
p_load(ggplot2)
p_load(scales)
p_load(viridis)
p_load(spdep)
p_load(cartography)
p_load(summarytools)
p_load(stargazer)
p_load(ggpattern) # remotes::install_github("coolbutuseless/ggpattern")
p_load(spgwr)
p_load(nngeo)
p_load(gridExtra)
p_load(cowplot)
p_load(spatialreg)
p_load(splm)
p_load(naniar)
p_load(ggmosaic)
p_load(polycor)

## Eurostat NUTS 3 shapefile ---------------------------------------------------

# this is the shapefile for 2021, we use the eurostat package which has the year 2016
getShapefile <- function(replace = FALSE){

  if(file.exists("./input/shapefile/nuts3.shp") && !replace) {
  
    ## load the shapefile
    shape_nuts3 <- sf::st_read("./input/shapefile", "nuts3")
    
    return(shape_nuts3)
  } else {
  
    # create a directory
    dir.create("input/shapefile")
    
    shape_nuts3 <- eurostat::get_eurostat_geospatial(
      output_class="sf", resolution="1", nuts_level=3, year=2016, 
      make_valid = TRUE) %>% 
      dplyr::rename("nuts3_id" = "NUTS_ID")
    names(shape_nuts3) <- tolower(names(shape_nuts3))
    # plot(sf::st_geometry(shape_nuts3))
    
    shape_nuts3 <- shape_nuts3 %>% dplyr::mutate(urbn_type = as.factor(urbn_type),
                                  coast_type = as.factor(coast_type),
                                  mount_type = as.factor(mount_type))
    
    # filter overseas territories
    overseas <- c("FRY10", "FRY20", "FRY30", "FRY40", "FRY50", "FRM01", "FRM02", 
                  "PT200", "PT300", "ES630", "ES640", "ES703", "ES704", "ES705", 
                  "ES706", "ES707", "ES708", "ES709", "ES531", "ES532", "ES533", 
                  "IS001", "IS002", "ITG1", "ITG25", "ITG26", "ITG27", "ITG28", 
                  "ITG29", "ITG2A", "ITG2B", "ITG2C", "EL431", "EL432", "EL433", 
                  "EL434", "EL411", "EL412", "EL413", "EL421", "EL422", "CY000")
    
    shape_nuts3 <- shape_nuts3 %>% 
      dplyr::filter(!nuts3_id %in% overseas)
    
    # re-project into an azimuthal projection (keeping area constant, hence using true area representation)
    shape_equalarea <- st_transform(shape_nuts3, "epsg:3035")
    shape_equalarea$area <- as.numeric(st_area(shape_equalarea))/1e6
    # area is in km^2
    
    # geometry needs to be dropped so that we can join the attributes to the original shapefile
    shape_equalarea <- st_drop_geometry(shape_equalarea)
    
    shape_nuts3 <- left_join(shape_nuts3, shape_equalarea)

    # save shapefile with area
    unlink("./input/shapefile/nuts3.shp")
    sf::st_write(shape_nuts3, "./input/shapefile/nuts3.shp")
    
    return(shape_nuts3)
  }
}

if(file.exists("./input/shapefile/nuts0.shp")) {
  shape_nuts0 <- sf::st_read("./input/shapefile", "nuts0")
} else {
  # we need a NUTS0 shapefile so that we can add country borders to the plot
  # to exclude regions overseas, exclude them from NUTS1 level 
  shape_nuts1 <- eurostat::get_eurostat_geospatial(output_class="sf", resolution="1", nuts_level=1, year=2016) 
  names(shape_nuts1) <- tolower(names(shape_nuts1))
  
  # then aggregate based on ctnr_code (and exclude Turkey, Iceland and Cyprus for nicer and more centered plots)
  shape_nuts0 <- shape_nuts1 %>% 
    dplyr::filter(!nuts_id %in% c("FRY", "PT2", "PT3", "ES7") & (cntr_code != "TR" & cntr_code != "IS" & cntr_code != "CY") ) %>% 
    count(cntr_code)
  
  # this is needed for st_pattern 
  shape_nuts0 <- st_remove_holes(shape_nuts0)
  
  # add Kosovo and Bosnia to the shapefile for NA line pattern
  # downloaded here: https://gadm.org/download_country_v3.html
  if(!dir.exists("./input/shapefile_extra/")) {
    dir.create("./input/shapefile_extra")
    
    # BIH
    download.file("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_BIH_0_sp.rds",
                  "./input/shapefile_extra/gadm36_BIH_0_sf.rds")
    
    # XK
    download.file("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_XKO_0_sp.rds",
                  "./input/shapefile_extra/gadm36_XK0_0_sf.rds")
    
  }
  
  ba <- readRDS("./input/shapefile_extra/gadm36_BIH_0_sf.rds") %>%
    sf::st_as_sf() %>% 
    dplyr::rename(cntr_code = "GID_0",
                  n = "NAME_0",
                  geom = "geometry")
  ba$cntr_code <- "BA"
  ba$n <- 1
  
  xk <- readRDS("./input/shapefile_extra/gadm36_XK0_0_sf.rds") %>%
    sf::st_as_sf() %>% 
    rename(cntr_code = "GID_0",
           n = "NAME_0",
           geom = "geometry")
  xk$cntr_code <- "XK"
  xk$n <- 1
  
  # rbind to shapefile
  shape_nuts0 <- rbind(shape_nuts0, ba, xk)
  rm(ba, xk)
  
  # buffer one last time if something went wrong
  shape_nuts0 <- st_buffer(shape_nuts0, dist = 0)
  
  # save shapefile with area
  unlink("./input/shapefile/nuts0.shp")
  sf::st_write(shape_nuts0, "./input/shapefile/nuts0.shp")
}
# These functions are often used as prep for visualisations
get_ghg_name_from_indicator <- function(indicator) {
  ind <- ifelse(grepl("CO2", indicator),
                substr(indicator, 7, 10),
                substr(indicator, 7, 9))
  return(ind)
}
get_sector_name_from_indicator <- function(indicator) {
  start <- 0
  # if it's an aggregate sectors indicator, start to look for sector later
  start <- ifelse(grepl("_agg_", indicator), start + 4, start)
  
  # add specific start index based on indicator
  start <- ifelse(grepl("CO2o", indicator) | grepl("CO2f", indicator),
                  start + 12, 
                  ifelse(grepl("CO2total", indicator), start + 15, start + 11))
  
  sector <- substring(indicator, start)
  return(sector)
}
