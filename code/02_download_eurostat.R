## This file is used to download all desired data from eurostat-----------------

if(! file.exists("./input/data_eurostat.rds")) {
  
  # source("./code/00_libraries_functions.R")
  
  # Download Data --------------------------------------------------------------
  
  # 1.) Population
  # Total (men + women) over all agegroups are chosen, only data for 2014-2020
  pop <- get_eurostat("demo_r_pjangrp3") %>% 
    filter(sex == "T" & age == "TOTAL" & nchar(geo) == 5) %>% 
    mutate(unit = "pop",
           year = as.numeric(format(as.Date(time, format="%Y/%m/%d"),"%Y"))) %>%
    dplyr::select(year, nuts3_id = geo, value = values, indicator = unit)
  
  # Get NUTS3 population data for 2000-2013
  # use NUTS3 population distribution for all years and multiply with
  # total national numbers of respective year
  dist <- pop %>%
    dplyr::filter(year == 2014) %>%
    dplyr::mutate(iso2 = substr(nuts3_id, 1, 2)) %>%
    dplyr::group_by(iso2) %>%
    dplyr::mutate(value_nat = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = value/value_nat) %>%
    dplyr::select(year, nuts3_id, value)
  
  pop_0013 <- dist %>%
    tidyr::complete(year = 2000:2013, nuts3_id = dist$nuts3_id) %>%
    dplyr::mutate(iso2 = substr(nuts3_id, 1, 2)) %>%
    dplyr::group_by(nuts3_id) %>%
    dplyr::mutate(value = value[year == 2014]) %>%
    dplyr::ungroup()
  
  pop_nat <- get_eurostat("nama_10_pe") %>%
    dplyr::mutate(year = as.numeric(format(as.Date(time, format="%Y/%m/%d"),"%Y")),
                  values = values*1000) %>%
    dplyr::filter(unit == "THS_PER", na_item == "POP_NC", year %in% c(2000:2013)) %>%
    dplyr::select(year, iso2 = geo, value_nat = values)
  
  pop_0013 <- merge(pop_0013, pop_nat, by = c("year", "iso2")) %>%
    dplyr::mutate(value = round(value*value_nat, digits = 0),
                  indicator = "pop") %>%
    dplyr::select(year, nuts3_id, value, indicator)
  
  pop <- rbind(pop, pop_0013) %>%
    dplyr::arrange(year, nuts3_id)
  
  # 2.) GDP and GPD/PC
  # PPP standards and PPP per capita are chosen here, other alternatives woud be:
  #   Million Euros (unit = "MIO_EUR")
  #   Million PPS (unit = "MIO_PPS_EU27_2020")
  
  gdp <- get_eurostat("nama_10r_3gdp", filters = list(unit = "MIO_PPS_EU27_2020")) %>% 
    dplyr::filter(nchar(geo) == 5) 
  gdp$time <- format(as.Date(gdp$time, format="%Y/%m/%d"),"%Y") 
  gdp$unit <- "gdp"
  
  gdppc <- get_eurostat("nama_10r_3gdp", filters = list(unit = "PPS_EU27_2020_HAB")) %>% 
    dplyr::filter(nchar(geo) == 5)
  gdppc$time <- format(as.Date(gdppc$time, format="%Y/%m/%d"),"%Y")
  gdppc$unit <- "gdppc"
  
  # 3.) Employment per sector
  # Indicator chosen:
  # V16910 = Persons employed in the population of active enterprises in t - number
  # Industries of Interest - available on eurostat; (Videras actually used)
  #   Code	      Label
  #   B-S_X_K642	Industry, construction and services except insurance activities of holding companies
  # * B-E	        Industry (except construction)
  # * F	          Construction
  #   G	          Wholesale and retail trade; repair of motor vehicles and motorcycles
  # * H	          Transportation and storage
  #   I	          Accommodation and food service activities
  #   J	          Information and communication
  #   K_L_X_K642	Financial and insurance activities; real estate activities except activities of holding companies
  #   M_N	        Professional, scientific and technical activities; administrative and support service activities
  #   P_Q	        Education; human health and social work activities
  #   R_S	        Arts, entertainment and recreation; other service activities
  # see: https://ec.europa.eu/eurostat/documents/3859598/5902521/KS-RA-07-015-EN.PDF/dd5443f5-b886-40e4-920d-9df03590ff91
  # for a ENACE documentation
  
  empl <- get_eurostat("bd_enace2_r3") %>%
    dplyr::filter(indic_sb == "V16910" & nchar(geo) == 5) %>%
    dplyr::filter(nace_r2 != "B-S_X_K642") %>%  #exclude overlapping NACE
    dplyr::group_by(geo, time) %>%
    dplyr::mutate(empshares = values/sum(values), #calculate industry shares
                  time = format(as.Date(time, format="%Y/%m/%d"),"%Y")) %>% 
    dplyr::filter(nace_r2 %in% c("B-E", "F", "H")) %>%
    dplyr::mutate(nace_r2 = gsub("-", "", nace_r2),
                  nace_r2 = paste0("emp_share_",nace_r2)) %>% 
    dplyr::select(unit = nace_r2, geo, time, values = empshares)
  
  # 4.) Gross valua added per sector
  # Industries of Interest - available on eurostat
  #   TOTAL	Total - all NACE activities
  # * A	    Agriculture, forestry and fishing
  # * B-E	  Industry (except construction)
  #   C	    Manufacturing
  # * F	    Construction
  # * G-J	  Wholesale and retail trade; transport; accommodation and food service activities; information and communication
  #   G-I	  Wholesale and retail trade, transport, accommodation and food service activities
  ##        G-J has a coverage of 98% whereas G-I is only available in 57% of observations
  #   J	    Information and communication
  #   K-N	  Financial and insurance activities; real estate activities; professional, scientific and technical activities; administrative and support service a...
  #   K	    Financial and insurance activities
  #   L	    Real estate activities
  #   M_N	  Professional, scientific and technical activities; administrative and support service activities
  #   O-U	  Public administration and defence; compulsory social security; education; human health and social work activities; arts, entertainment and recreati...
  #   O-Q	  Public administration, defence, education, human health and social work activities
  #   R-U	  Arts, entertainment and recreation; other service activities; activities of household and extra-territorial organizations and bodies
  
  # Problem: Transportation (H) is not available explicitly, G-I is not really well-covered
  gva <- get_eurostat("nama_10r_3gva") %>% 
    dplyr::filter(nchar(geo) == 5,
                  currency == "MIO_EUR",
                  !(nace_r2 %in% c("G-I", "J" ,"C", "O-Q","R-U"))) # filtering overlapping NACE

  gva_total <- gva %>% dplyr::filter(nace_r2 == "TOTAL")
  gva <- gva %>% 
    dplyr::filter(nace_r2 != "TOTAL") %>% 
    dplyr::left_join(gva_total[,c("geo","time","values")], 
                     by = c("geo" = "geo", "time" = "time"),
                     suffix = c("", "_total")) %>% 
    dplyr::mutate(gvashare = values / values_total) %>% 
    dplyr::select(nace_r2, geo, time, gvashare) %>% 
    dplyr::mutate(time = format(as.Date(time, format="%Y/%m/%d"),"%Y")) %>% 
    dplyr::filter(nace_r2 %in% c("A", "B-E", "F", "G-J")) %>% 
    # adjust nace_r2 to be used as the unit, add "gva_share_" before its name
    dplyr::mutate(nace_r2 = gsub("-", "", nace_r2),
                  nace_r2 = paste0("gva_share_",nace_r2)) %>% 
    dplyr::select(unit = nace_r2, geo, time, values = gvashare)
  
  rm(gva_total)
  
  # Download Data additional data (not used in analysis):-----------------------
  # 1.) People at risk of poverty or social exclusion 
  # Only available on NUTS2, so not added to main data for now
  socexcl <- get_eurostat("ilc_peps11") %>% 
    dplyr::filter(nchar(geo) == 4)
  socexcl$time <- format(as.Date(socexcl$time, format="%Y/%m/%d"),"%Y")
  socexcl$unit <- "socexcl"
  
  #2.) Annual road transport data
  # The data is available per:
  #region of loading
  #region of unloading
  #I used the sum of both for both (unloading and loading) for final data
  #maybe the data should be put in relative terms to GDP or something else later on
  
  # National annual road freight transport by regions of loading (NUTS 3) and by group of goods (1 000 t), from 2008 onwards
  roadload <- get_eurostat("road_go_na_rl3g") %>% 
    dplyr::filter(nchar(geo) == 5 & nst07 == "TOTAL") %>% 
    dplyr::mutate(unit = "road_load") %>% 
    dplyr::select(-nst07)
  
  # National annual road freight transport by regions of unloading (NUTS 3) and by group of goods (1 000 t), from 2008 onwards
  roadunload <- get_eurostat("road_go_na_ru3g") %>% 
    dplyr::filter(nchar(geo) == 5 & nst07 =="TOTAL")  %>% 
    dplyr::mutate(unit = "road_unload") %>% 
    dplyr::select(-nst07)
  
  roadtotal <- roadload %>%
    dplyr::full_join(roadunload, by = c("geo", "time")) %>%
    dplyr::mutate(values = values.x + values.y,
                  unit = "road_total") %>% 
    dplyr::select(unit, geo, time, values)
  
  road <- rbind(roadunload, roadload, roadtotal) %>% 
    dplyr::mutate(time = format(as.Date(time, format="%Y/%m/%d"),"%Y"))
  
  # Combine Data and save:--------------------------------------------------------
  
  #combine
  fineurostat <- rbind(gdp, gdppc, empl, gva, road)
  colnames(fineurostat) <- c("indicator","nuts3_id", "year", "value")
  fineurostat <- rbind(pop, fineurostat)
  
  #save
  saveRDS(fineurostat, "./input/data_eurostat.rds")
}

