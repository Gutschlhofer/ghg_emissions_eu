## This file is used to download all desired data from eurostat-----------------

if(! file.exists("./input/data_eurostat.rds")) {
  
  # source("./code/00_libraries_functions.R")
  
  # Download Data following Videras:--------------------------------------------
  
  # 1.) Population
  # Total (men + women) over all agegroups are chosen, only data for 2014-2018
  pop <- get_eurostat("demo_r_pjangrp3") %>% 
    filter(sex == "T" & age == "TOTAL" & nchar(geo) == 5) %>% 
    mutate(unit = "pop",
           year = as.numeric(format(as.Date(time, format="%Y/%m/%d"),"%Y"))) %>%
    dplyr::select(year, nuts3_id = geo, value = values, indicator = unit)
  
  
  # Get NUTS3 population data for 2000-2013
  # use NUTS3 population distribution for all years and multiply with
  # total national numbers of respective year
  dist <- pop %>%
    filter(year == 2014) %>%
    mutate(iso2 = substr(nuts3_id, 1, 2)) %>%
    group_by(iso2) %>%
    mutate(value_nat = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(value = value/value_nat) %>%
    dplyr::select(year, nuts3_id, value)
  
  pop_0013 <- dist %>%
    complete(year = 2000:2013, nuts3_id = dist$nuts3_id) %>%
    mutate(iso2 = substr(nuts3_id, 1, 2)) %>%
    group_by(nuts3_id) %>%
    mutate(value = value[year == 2014]) %>%
    ungroup()
  
  pop_nat <- get_eurostat("nama_10_pe") %>%
    mutate(year = as.numeric(format(as.Date(time, format="%Y/%m/%d"),"%Y")),
           values = values*1000) %>%
    filter(unit == "THS_PER", na_item == "POP_NC", year %in% c(2000:2013)) %>%
    dplyr::select(year, iso2 = geo, value_nat = values)
  
  pop_0013 <- merge(pop_0013, pop_nat, by = c("year", "iso2")) %>%
    mutate(value = round(value*value_nat, digits = 0),
           indicator = "pop") %>%
    dplyr::select(year, nuts3_id, value, indicator)
  
  pop <- rbind(pop, pop_0013) %>%
    arrange(year, nuts3_id)
  
  
  # 2.) GDP and GPD/PC
  # PPP standards and PPP per capita are chosen here, other alternatives woud be:
  #   Million Euros (unit = "MIO_EUR")
  #   Million PPS (unit = "MIO_PPS")
  
  gdp <- get_eurostat("nama_10r_3gdp", filters = list(unit = "MIO_PPS")) %>% 
    filter(nchar(geo) == 5) 
  gdp$time <- format(as.Date(gdp$time, format="%Y/%m/%d"),"%Y") 
  gdp$unit <- "gdp"
  
  gdppc <- get_eurostat("nama_10r_3gdp", filters = list(unit = "PPS_HAB")) %>% 
    filter(nchar(geo) == 5) 
  gdppc$time <- format(as.Date(gdppc$time, format="%Y/%m/%d"),"%Y")
  gdppc$unit <- "gdppc"
  
  
  # 3.) Employment per sector
  # Indicator chosen:
  # V16910 = Persons employed in the population of active enterprises in t - number
  # Industries of Interest - available on eurostat; (Videras actually used)
  #   H = Transportation and storage; (Transportation and Utilities)
  #   B-E = Industry; (Manufacturing)
  # Videras also has extraction, this is not available explicit but included in
  # Industry B-E since B classifies "Mining and Quarrying"
  # see: https://ec.europa.eu/eurostat/documents/3859598/5902521/KS-RA-07-015-EN.PDF/dd5443f5-b886-40e4-920d-9df03590ff91
  # for a ENACE documentation
  
  empl <- get_eurostat("bd_enace2_r3") %>%
    filter(indic_sb == "V16910" & nchar(geo) == 5) %>%
    filter(nace_r2 != "B-S_X_K642") %>%  #exclude overlapping NACE
    group_by(geo, time) %>%
    mutate(empshares = round(values/sum(values),3)) #calculate industry shares
  empl$time <- format(as.Date(empl$time, format="%Y/%m/%d"),"%Y")
  
  #check if this worked
  xy <- filter(empl, geo == "AT111" & time == 2017)
  xy
  sum(xy$empshares) #looks good
  
  empl <- filter(empl, nace_r2 == "B-E" | nace_r2 == "H") %>%
    dplyr::select(nace_r2, geo, time, empshares)
  colnames(empl) <- c("unit", "geo", "time","values")
  
  emplind <- empl %>% filter(unit == "B-E")
  emplind$unit <- "emp_share_BE"
  
  
  empltrans <- empl %>% filter(unit == "H")
  empltrans$unit <- "emp_share_H"
  
  
  # 4.) Gross valua added per sector
  # Industries of Interest - available on eurostat; (Videras actually used)
  #   G-I = Wholesale and retail trade, transport, accommodation and food service activities ; (Transportation and Utilities)
  #   B-E = Industry; (Manufacturing)
  # Problem: Transportation (H) is not available explicit
  gwa <- get_eurostat("nama_10r_3gva") %>% 
    filter(nchar(geo) == 5 & currency == "MIO_EUR") %>% 
    filter(nace_r2 != "C" & nace_r2 != "G-J" & nace_r2 != "K-N" 
           & nace_r2 != "O-Q" & nace_r2 != "R-U" & nace_r2 != "TOTAL") %>% 
    group_by(geo, time) %>% 
    mutate(gwashare = round(values/sum(values),3)) %>% 
    dplyr::select(nace_r2, geo, time, gwashare)
  gwa$time <- format(as.Date(gwa$time, format="%Y/%m/%d"),"%Y")
  
  
  xy <- filter(gwa, geo == "AT111" & time == 2017)
  xy
  sum(xy$gwashare) #looks good
  
  gwaind <- gwa %>% filter(nace_r2 == "B-E")
  gwaind$nace_r2 <- "gwa_share_BE"
  colnames(gwaind) <- c("unit", "geo", "time","values")
  
  gwatrans <- gwa %>% filter(nace_r2 == "G-I")
  gwatrans$nace_r2 <- "gwa_share_GI"
  colnames(gwatrans) <- c("unit", "geo", "time","values")
  
  
  # Download Data additional data (not used in analysis):-----------------------
  # 1.) People at risk of poverty or social exclusion 
  # Only available on NUTS2, so not added to main data for now
  socexcl <- get_eurostat("ilc_peps11") %>% 
    filter(nchar(geo) == 4)
  socexcl$time <- format(as.Date(socexcl$time, format="%Y/%m/%d"),"%Y")
  socexcl$unit <- "socexcl"
  
  #2.) Annual road transport data
  # The data is available per:
  #region of loading
  #region of unloading
  #I used the sum of both for both (unloading and loading) for final data
  #maybe the data should be put in relative terms to GDP or something else later on
  
  roadload <- get_eurostat("road_go_na_rl3g") %>% 
    filter(nchar(geo) == 5 & nst07 == "TOTAL") #total goods
  
  roadunload <- get_eurostat("road_go_na_ru3g") %>% 
    filter(nchar(geo) == 5 & nst07 =="TOTAL") 
  
  roadtotal <- roadload %>% 
    left_join(roadunload, by = c("geo", "time", "unit")) %>% 
    mutate(values = values.x + values.y) %>% 
    dplyr::select(unit, geo, time, values)
  
  roadtotal$time <- format(as.Date(roadtotal$time, format="%Y/%m/%d"),"%Y")
  roadtotal$unit <- "roadtotal"
  colnames(roadtotal) <- c("unit", "geo", "time","values")
  
  
  # Combine Data and save:--------------------------------------------------------
  
  #combine
  fineurostat <- rbind(gdp, gdppc, emplind, empltrans, gwaind, gwatrans, roadtotal)
  colnames(fineurostat) <- c("indicator","nuts3_id", "year", "value")
  fineurostat <- rbind(pop, fineurostat)
  
  #save
  saveRDS(fineurostat, "./input/data_eurostat.rds")
  
}

