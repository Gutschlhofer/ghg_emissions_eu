## This file downloads the EDGAR data-------------------------------------------
# use "CO2_excl_short-cycle_org_C" (same as Videras)
# also added CH4 and N20 but not used

if(! file.exists("./input/data_edgar.rds")) {
  
  # source("./code/00_libraries_functions.R")
  
  dir.create("input/edgar")
  
  # CO2-------------------------------------------------------------------------
  
  # create directory
  dir.create("input/edgar/co2")
  
  # download and unzip file
  download.file("https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=187465",
                "input/edgar/co2/v50_CO2_excl_short-cycle_org_C_TOTALS")
  
  unzip("input/edgar/co2/v50_CO2_excl_short-cycle_org_C_TOTALS", exdir = "./input/edgar/co2")
  
  fs <- list.files("input/edgar/co2", pattern = ".txt$", full.names = T)
  
  # use only years 2000-2018
  fs <- fs[31:49]
  
  # get the NUTS3 shapefile
  shape_nuts3 <- getShapefile(replace = FALSE) # set replace = TRUE if you change the resolution of the shapefile

  # read first file (2000), check for same projection
  r <- read.delim(file = fs[1], header = T, sep = ";", dec = ".", skip = 2) %>%
    dplyr::select(lon, lat, value = starts_with("emission")) %>%
    rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") #not sure about the datum but projection should be correct
  
  # check if .nc files have the same projection as shapefile, per construction
  if(!compareCRS(shape_nuts3, r)) print("Warning: shape and nc file have different CRS!")
  
  # create data.frame to save extracted data
  data_edgar_co2 <- data.frame()
  
  # use multiple cores from server
  registerDoMC(20)
  
  # # extract data
  data_edgar_co2 <- foreach(f = fs) %dopar% {

    r <- read.delim(file = f, header = T, sep = ";", dec = ".", skip = 2) %>%
      dplyr::select(lon, lat, value = starts_with("emission")) %>%
      rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
    
    r$a <- area(r) # calc area of raster grid
    r$avalue <- r$value/r$a # get area corrected value, value per km2

    er <- raster::extract(r, shape_nuts3, method = "simple", fun = mean, na.rm = TRUE, df = TRUE, sp = TRUE, weights = TRUE) 
    # weights = TRUE: approximate fraction of each cell that is covered by the polygon, should (hopefully) perform better at borders 

    d <- er@data %>%
      dplyr::select(nuts3_id, avalue, area) %>%
      mutate(year = as.numeric(substr(f, 48, 51)), 
             indicator = c("edgar_co2"),
             value = avalue*area) %>%
      dplyr::select(year, nuts3_id, value, indicator)

    return(d)
  }

  # save CO2 data
  data_edgar_co2 <- do.call(rbind, data_edgar_co2)
  #saveRDS(data_edgar_co2, "input/data_edgar_co2.rds")


  # # plot 2016 points
  # fs <- list.files("input/edgar/co2", pattern = ".txt$", full.names = T)
  # fs <- fs[47] #2016
  # 
  # shape_nuts3 <- getShapefile()
  # 
  # r <- read.delim(file = fs, header = T, sep = ";", dec = ".", skip = 2) %>%
  #   dplyr::select(lon, lat, value = starts_with("emission")) %>%
  #   rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") #not sure about the datum but projection should be correct
  # 
  # r@data@values <- ifelse(is.na(r@data@values), NA, 1)
  # plot(r)
  # plot(r, xlim=c(0,5),ylim=c(30,35))
  # 
  # 
  # xy <- xyFromCell(r, which(r[]==1))
  # 
  # mp <- st_multipoint(x = xy, dim = "XY")
  # geom <- st_cast(st_sfc(mp), "POINT")
  # pts <- st_sf(v = 1, geom, crs = NA) %>%
  #   st_set_crs(crs(shape_nuts3))
  # intpts <- st_intersection(shape_nuts3, pts)
  # 
  # # Europe
  # ggplot() +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) +
  # 
  #   geom_sf(data = shape_nuts3, color='#000000', fill = "white", size=0.1) +
  #   geom_sf(data = shape_nuts0, color='#000000', fill=NA, size=0.5) +
  #   geom_sf(data = intpts, color = "green", fill="green", size=0.3, alpha = 0.9) +
  #   theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))
  # 
  # # DE only
  # ggplot() +
  #   geom_sf(data = shape_nuts3 %>% dplyr::filter(cntr_code == "DE"), color='#000000', fill = "white", size=0.1) +
  #   geom_sf(data = shape_nuts0 %>% dplyr::filter(cntr_code == "DE"), color='#000000', fill=NA, size=0.5) +
  #   geom_sf(data = intpts %>% dplyr::filter(cntr_code == "DE"), color = "green", fill="green", size=0.3, alpha = 0.9) +
  #   theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))

  
  # CH4-------------------------------------------------------------------------
  # same procedure as for CO2
  # 
  # dir.create("input/edgar/ch4")
  # 
  # download.file("https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=185316",
  #               "input/edgar/ch4/v50_CH4_TOTALS")
  # 
  # unzip("input/edgar/ch4/v50_CH4_TOTALS", exdir = "./input/edgar/ch4")
  # 
  # fs <- list.files("input/edgar/ch4", pattern = ".txt$", full.names = T)
  # 
  # # only for 2000-2015
  # fs <- fs[31:46]
  # 
  # r <- read.delim(file = fs[1], header = T, sep = ";", dec = ".", skip = 2) %>%
  #   dplyr::select(lon, lat, value = starts_with("emission")) %>%
  #   rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") #not sure about the datum but projection should be correct
  # 
  # # check if .nc files have the same projection as shapefile
  # if(!compareCRS(shape_nuts3, r)) print("Warning: shape and nc file have different CRS!")
  # 
  # 
  # data_edgar_ch4 <- data.frame()
  # 
  # registerDoMC(20)
  # data_edgar_ch4 <- foreach(f = fs) %dopar% {
  #   
  #   r <- read.delim(file = f, header = T, sep = ";", dec = ".", skip = 2) %>%
  #     dplyr::select(lon, lat, value = starts_with("emission")) %>%
  #     rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
  #   
  #   er <- raster::extract(r, shape_nuts3, method = "simple", fun = sum, na.rm = TRUE, df = TRUE, sp = TRUE)
  #   
  #   d <- er@data %>%
  #     dplyr::select(nuts3_id, value) %>%
  #     mutate(year = as.numeric(substr(f, 25, 28)), indicator = c("edgar_ch4")) %>%
  #     dplyr::select(year, nuts3_id, value, indicator)
  #   
  #   return(d)
  # }
  # 
  # data_edgar_ch4 <- do.call(rbind, data_edgar_ch4)
  #saveRDS(data_edgar_ch4 , "input/data_edgar_ch4.rds")
  # 
  # 
  # 
  # N2O-------------------------------------------------------------------------
  # same procedure as for CO2
  # 
  # dir.create("input/edgar/n2o")
  # 
  # download.file("https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=183085",
  #               "input/edgar/n2o/v50_N2O_TOTALS")
  # 
  # unzip("input/edgar/n2o/v50_N2O_TOTALS", exdir = "./input/edgar/n2o")
  # 
  # fs <- list.files("input/edgar/n2o", pattern = ".txt$", full.names = T)
  # 
  # # only for 2000-2015
  # fs <- fs[31:46]
  # 
  # r <- read.delim(file = fs[1], header = T, sep = ";", dec = ".", skip = 2) %>%
  #   dplyr::select(lon, lat, value = starts_with("emission")) %>%
  #   rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") #not sure about the datum but projection should be correct
  # 
  # # check if .nc files have the same projection as shapefile
  # if(!compareCRS(shape_nuts3, r)) print("Warning: shape and nc file have different CRS!")
  # 
  # 
  # data_edgar_n2o <- data.frame()
  # 
  # registerDoMC(20)
  # data_edgar_n2o <- foreach(f = fs) %dopar% {
  #   
  #   r <- read.delim(file = f, header = T, sep = ";", dec = ".", skip = 2) %>%
  #     dplyr::select(lon, lat, value = starts_with("emission")) %>%
  #     rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
  #   
  #   er <- raster::extract(r, shape_nuts3, method = "simple", fun = sum, na.rm = TRUE, df = TRUE, sp = TRUE)
  #   
  #   d <- er@data %>%
  #     dplyr::select(nuts3_id, value) %>%
  #     mutate(year = as.numeric(substr(f, 25, 28)), indicator = c("edgar_n2o")) %>%
  #     dplyr::select(year, nuts3_id, value, indicator)
  #   
  #   return(d)
  # }
  # 
  # data_edgar_n2o <- do.call(rbind, data_edgar_n2o)
  #saveRDS(data_edgar_n2o , "input/data_edgar_n2o.rds")
  
  
  # rbind all
  # data_edgar <- rbind(data_edgar_co2, data_edgar_ch4, data_edgar_n2o)
  data_edgar <- rbind(data_edgar_co2)
  saveRDS(data_edgar , "input/data_edgar.rds")
  
}
