## This file downloads the EDGAR data-------------------------------------------
shape_nuts3 <- getShapefile()
shp <- sp::spTransform(
  as(shape_nuts3, 'Spatial'), 
  "+proj=longlat +datum=WGS84 +lon_0=0 +x_0=0 +y_0=0"
) # raster::extract only works with sp, not sf

dataframe_from_raster_file <- function(file_name, indicator_name, sector = "TOTALS") {
  
  if(!file.exists(file_name)) {
    warning(sprintf("File %s does not exist!", file_name))
    return(NULL)
  }
  
  r <- read.delim(file = file_name, header = T, sep = ";", dec = ".", skip = 2) %>%
    dplyr::select(lon, lat, value = starts_with("emission")) %>%
    rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
  
  # set value to 0 in case it is NA
  r@data@values <- ifelse(is.na(r@data@values), 0, r@data@values)
  
  # r$value Emission gridmaps are expressed in ton substance / 0.1degree x 0.1degree / year for the .txt files
  # calculate value per area
  r_val_per_area <- r$value / area(r)
  r_val_per_area$layer
  
  er <- raster::extract(r_val_per_area
                        ,shp
                        ,method = "simple"
                        ,fun = mean
                        ,small = TRUE
                        ,na.rm = TRUE
                        ,df = TRUE
                        ,sp = TRUE
                        ,weights = TRUE
                        ,normalizeWeights = TRUE
                        ) 
  # get year (look for totals, which is just before the year e.g. "_2010_TOTALS.txt")
  year <- as.numeric(substr(file_name,
                            regexpr(paste0(sector, ".txt"), file_name)[1]-5,
                            regexpr(paste0(sector, ".txt"), file_name)[1]-2))
  
  d <- er@data %>%
    dplyr::select(
      nuts3_id
      ,value = layer
      ,area) %>%
    mutate(year = year
           ,indicator = c(indicator_name)
           ,value = value*area # since we have value/km2 we multiply by area again
    ) %>%
    dplyr::select(year, nuts3_id, value, indicator)
  
  return(d)
}

get_edgar_data <- function(download_link, file_name, short_name, sector = "TOTALS") {
  
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
  
  if(length(fs) == 0) {
    # this means that the download has happened but no files have been extracted
    unzip(file_name, exdir = folder_name)
    fs <- list.files(folder_name, pattern = ".txt$", full.names = T)
  }
  
  years <- 2002:2018
  
  # check if we already have parts (or all) of the years available
  if(file.exists(paste0("input/data_edgar_",short_name,".rds"))) {
    years_avail <- readRDS(paste0("input/data_edgar_",short_name,".rds")) %>% 
      distinct(year) %>% pull(year)
    
    years <- years[!years %in% years_avail]
  }
  if(length(years) == 0) {
    return()
  }
  
  # use only years not done yet
  fs <- fs[as.numeric(substr(fs,
                             regexpr(paste0(sector, ".txt"), fs)[1]-5,
                             regexpr(paste0(sector, ".txt"), fs)[1]-2))
             %in% years]
  
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
  
  # save GHG data
  data_edgar_ghg <- do.call(rbind, data_edgar_ghg)
  
  # check if we already have some data available,
  # then we just merge it
  if(file.exists(paste0("input/data_edgar_",short_name,".rds"))) {
    data_orig <- readRDS(paste0("input/data_edgar_",short_name,".rds"))
    data_edgar_ghg <- rbind(data_edgar_ghg, data_orig)
  }
  
  saveRDS(data_edgar_ghg, paste0("input/data_edgar_",short_name,".rds"))
}

if(! file.exists("./input/data_edgar.rds")) {
  
  dir.create("input/edgar")
  
  # CO2 fossil -----------------------------------------------------------------
  get_edgar_data(download_link = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/CO2_excl_short-cycle_org_C/TOTALS/TOTALS_txt.zip",
                 file_name = "v60_CO2_excl_short-cycle_org_C_TOTALS.zip",
                 short_name = "CO2f")
  
  # CH4 ------------------------------------------------------------------------
  get_edgar_data(download_link = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/CH4/TOTALS/TOTALS_txt.zip",
                 file_name = "v60_CH4_TOTALS.zip",
                 short_name = "CH4")
  # N2O ------------------------------------------------------------------------
  get_edgar_data(download_link = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/N2O/TOTALS/TOTALS_txt.zip",
                 file_name = "v60_N2O_TOTALS.zip",
                 short_name = "N2O")
  # CO2 organic ----------------------------------------------------------------
  get_edgar_data(download_link = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/CO2_org_short-cycle_C/TOTALS/TOTALS_txt.zip",
                 file_name = "v60_CO2_org_short-cycle_TOTALS.zip",
                 short_name = "CO2o")
  
  # Now sector-specific data ---------------------------------------------------
  sector_info <- read.csv("input/edgar/edgar_sectors.csv") %>% 
    dplyr::filter(!short %in% c("PRO_COAL","PRO_GAS","PRO_OIL")) # those are all aggregated in "PRO"
  substances <- c("CH4", "CO2_excl_short-cycle_org_C", "CO2_org_short-cycle_C", "N2O")
  link_template <- "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/%s/%s/%s_txt.zip"
  
  # the functions check for files present and data already extracted
  # so no need to adjust the looping variables
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
  
  # rbind all
  data_edgar <- rbind(readRDS("input/data_edgar_CO2f.rds"),
                      readRDS("input/data_edgar_CH4.rds"),
                      readRDS("input/data_edgar_N2O.rds"),
                      readRDS("input/data_edgar_CO2o.rds"))
  saveRDS(data_edgar, "input/data_edgar.rds")
  
  fs <- list.files("input", pattern = "*.rds", full.names = T)
  fs <- fs[grep("data_edgar_", fs)]
  fs <- fs[fs!="input/data_edgar_all.rds"] # exclude this file, otherwise we duplicate our data
  fs <- fs[!grepl("input/data_edgar_CH4_PRO_", fs)] # exclude these files, otherwise we duplicate CH4_PRO data
  data_edgar_all <- purrr::map_df(fs, readRDS)
  saveRDS(data_edgar_all, "input/data_edgar_all.rds")
}
