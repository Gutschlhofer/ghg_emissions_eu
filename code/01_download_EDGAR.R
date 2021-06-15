## This file downloads the EDGAR data-------------------------------------------
# use "CO2_excl_short-cycle_org_C" (same as Videras)
# also added CH4 and N20 but not used

dataframe_from_raster_file <- function(file_name, indicator_name, sector = "TOTALS") {
  
  if(!file.exists(file_name)) {
    warning(sprintf("File %s does not exist!", file_name))
    return(NULL)
  }
  
  r <- read.delim(file = file_name, header = T, sep = ";", dec = ".", skip = 2) %>%
    dplyr::select(lon, lat, value = starts_with("emission")) %>%
    rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
  
  # r$value Emission gridmaps are expressed in ton substance / 0.1degree x 0.1degree / year for the .txt files
  r$a <- area(r) # calc area of raster grid, in km2
  r$avalue <- r$value/r$a # get area corrected value, tons per km2
  
  er <- raster::extract(r, shape_nuts3, method = "simple", fun = mean, na.rm = TRUE, df = TRUE, sp = TRUE, weights = TRUE) 
  # weights = TRUE: approximate fraction of each cell that is covered by the polygon, should perform better at borders 
  
  # look for totals, which is just before the year e.g. "_2010_TOTALS.txt"
  year <- as.numeric(substr(file_name,
                            regexpr(paste0(sector, ".txt"), file_name)[1]-5,
                            regexpr(paste0(sector, ".txt"), file_name)[1]-2))
  
  d <- er@data %>%
    dplyr::select(nuts3_id, avalue, area) %>%
    mutate(year = year, 
           indicator = c(indicator_name),
           value = avalue*area # since we have value/km2 we multiply by area again
           ) %>%
    dplyr::select(year, nuts3_id, value, indicator)
  
  return(d)
}

get_edgar_data <- function(download_link, file_name, short_name, sector = "TOTALS") {
  
  # folder_name = "input/edgar/co2"
  folder_name <- paste0("input/edgar/",short_name)
  
  # create directory
  dir.create(folder_name)
  
  file_name <- paste0(folder_name, "/", file_name)
  
  if(! file.exists(file_name)) {
    # download and unzip file
    download.file(download_link,
                  file_name)
    
    unzip(file_name, exdir = folder_name)
  }
  
  fs <- list.files(folder_name, pattern = ".txt$", full.names = T)
  
  # use only years 2000-2018
  fs <- fs[31:49]
  # fs <- fs[41:49]
  
  if(is.na(fs[1])) {
    warning("Data is not available for all years and no data is available for the current selection.")
    return()
  }
  
  # get the NUTS3 shapefile
  shape_nuts3 <- getShapefile(replace = FALSE) # set replace = TRUE if you change the resolution of the shapefile
  
  # read first file (2000), check for same projection
  r <- read.delim(file = fs[1], header = T, sep = ";", dec = ".", skip = 2) %>%
    dplyr::select(lon, lat, value = starts_with("emission")) %>%
    rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") #not sure about the datum but projection should be correct
  
  # check if .nc files have the same projection as shapefile, per construction
  if(!compareCRS(shape_nuts3, r)) print("Warning: shape and nc file have different CRS!")
  
  # create data.frame to save extracted data
  data_edgar_ghg <- data.frame()
  
  # use multiple cores from server
  registerDoMC(12)
  
  print("starting")
  print(file_name)
  
  # # extract data
  data_edgar_ghg <- foreach(f = fs) %dopar% {
    dataframe_from_raster_file(f, paste0("edgar_", short_name), sector)
  }
  
  # save CO2 data
  data_edgar_ghg <- do.call(rbind, data_edgar_ghg)
  saveRDS(data_edgar_ghg, paste0("input/data_edgar_",short_name,".rds"))
}

if(! file.exists("./input/data_edgar.rds")) {
  
  dir.create("input/edgar")
  
  # CO2-------------------------------------------------------------------------
  get_edgar_data(download_link = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/CO2_excl_short-cycle_org_C/TOTALS/TOTALS_txt.zip",
                 file_name = "v60_CO2_excl_short-cycle_org_C_TOTALS.zip",
                 short_name = "co2")
  
  # CH4-------------------------------------------------------------------------
  get_edgar_data(download_link = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/CH4/TOTALS/TOTALS_txt.zip",
                 file_name = "v60_CH4_TOTALS.zip",
                 short_name = "ch4")
  # N2O-------------------------------------------------------------------------
  get_edgar_data(download_link = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/N2O/TOTALS/TOTALS_txt.zip",
                 file_name = "v60_N2O_TOTALS.zip",
                 short_name = "n2o")
  # CO2 short-------------------------------------------------------------------
  get_edgar_data(download_link = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/CO2_org_short-cycle_C/TOTALS/TOTALS_txt.zip",
                 file_name = "v60_CO2_org_short-cycle_TOTALS.zip",
                 short_name = "co2_short")
  
  # Now sector-specific data----------------------------------------------------
  # 
  sector_info <- read.csv("input/edgar/edgar_sectors.csv")
  substances <- c("CH4", "CO2_excl_short-cycle_org_C", "CO2_org_short-cycle_C", "N2O")
  # link <- sprintf(
  #   "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/%s/%s/%s_txt.zip",
  #   indicator, sector, sector
  # )
  link_template <- "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/%s/%s/%s_txt.zip"
  
  for(i in c(1:length(sector_info$short))) {
    
    sector <- sector_info$short[i]
    
    for(substance in substances) {
      if(sector_info[sector_info$short == sector,
                     str_replace(substance, "-", ".")] == "x") {
        print(sprintf("%s_%s", substance, sector))
        
        # generate the download link from the template
        link <- sprintf(link_template, substance, sector, sector)
        # download, unzip, and already extract the data to our shapefile
        get_edgar_data(download_link = link,
                       file_name = sprintf("v60_%s_%s.zip", substance, sector),
                       short_name = sprintf("%s_%s", substance, sector),
                       sector = sector)
      }
    }
  }
  
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

  # rbind all
  data_edgar <- rbind(readRDS("input/data_edgar_co2.rds"),
                      readRDS("input/data_edgar_ch4.rds"),
                      readRDS("input/data_edgar_n2o.rds"),
                      readRDS("input/data_edgar_co2_short.rds"))
  saveRDS(data_edgar, "input/data_edgar.rds")
  
  fs <- list.files("input", pattern = "*.rds", full.names = T)
  fs <- fs[grep("data_edgar_", fs)]
  fs <- fs[fs!="input/data_edgar_all.rds"]
  data_edgar_all <- purrr::map_df(fs, readRDS)
  saveRDS(data_edgar_all, "input/data_edgar_all.rds")
}
