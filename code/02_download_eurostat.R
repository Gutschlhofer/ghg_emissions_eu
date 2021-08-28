## This file is used to download all desired data from eurostat-----------------

if(! file.exists("./input/data_eurostat.rds")) {
  
  # source("./code/00_libraries_functions.R")
  
  # Download Data --------------------------------------------------------------
  
  # 1.) Population
  # this has only accurate information from 2007 onwards
  pop <- get_eurostat("demo_r_pjanaggr3") %>% 
    dplyr::filter(
      sex == "T"
      ,nchar(geo) == 5
      # exclude redundant or non-needed age groups
      ,!(age %in% c("UNK"))
    ) %>% 
    dplyr::mutate(
      # here we prepare the population numbers
      unit = ifelse(age == "TOTAL", 
                    "pop", 
                    # use gsub() to replace "-" which is problematic in col names
                    paste0("pop_", age %>% gsub("-", "_", .))),
      year = as.numeric(format(as.Date(time, format="%Y/%m/%d"),"%Y"))
    ) %>%
    dplyr::filter(
      year >= 2007 # data for all regions is only available from 2007 onwards
      # but some regions offer longer time series
    ) %>% 
    dplyr::select(year, nuts3_id = geo, value = values, indicator = unit)
  
  # calculate population shares for the age groups
  pop <- pop %>% 
    tidyr::pivot_wider(id_cols = c("year", "nuts3_id"), names_from = "indicator") %>% 
    dplyr::mutate(
      pop_share_Y_LT15 = pop_Y_LT15/pop,
      pop_share_Y15_64 = pop_Y15_64/pop,
      pop_share_Y_GE65 = pop_Y_GE65/pop
    ) %>% 
    dplyr::select(-starts_with("pop_Y")) %>% 
    tidyr::pivot_longer(cols = starts_with("pop"), names_to = "indicator")
  
  # 1.b) Population, more detailed age groups from 2014 onwards
  
  # groups based on Liddle, Lung 2010
  age_group <- c(
    "TOTAL" = "TOTAL", # we leave total as-is
    # younger working class
    "Y20-24" = "Y20_34",
    "Y25-29" = "Y20_34",
    "Y30-34" = "Y20_34",
    # middle aged working class
    "Y35-39" = "Y35_49",
    "Y40-44" = "Y35_49",
    "Y45-49" = "Y35_49",
    # older working class
    "Y50-54" = "Y50_64",
    "Y55-59" = "Y50_64",
    "Y60-64" = "Y50_64",
    # retirees
    "Y65-69" = "Y_GE64",
    "Y70-74" = "Y_GE64",
    "Y75-79" = "Y_GE64",
    "Y80-84" = "Y_GE64",
    "Y_GE85" = "Y_GE64"
  )
  
  pop_detailed <- get_eurostat("demo_r_pjangrp3") %>% 
    dplyr::filter(
      sex == "T"
      # ,age == "TOTAL"
      ,nchar(geo) == 5
      # exclude redundant or non-needed age groups
      ,!(age %in% c("Y85-89", "Y_GE90", "Y_LT5", "Y5-9", "Y10-14", "Y15-19", "UNK"))
    ) %>% 
    dplyr::mutate(age = age_group[age]) %>% 
    dplyr::group_by(geo, age, time) %>% 
    dplyr::summarise(values = sum(values, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # here we prepare the population numbers
      unit = ifelse(age == "TOTAL", "pop", paste0("pop_", age)),
      year = as.numeric(format(as.Date(time, format="%Y/%m/%d"),"%Y"))
    ) %>%
    dplyr::filter(
      !(unit %in% c("pop", "pop_Y_GE64")) # we already have those
    ) %>% 
    dplyr::select(year, nuts3_id = geo, value = values, indicator = unit)
  # problem: this is only available from 2014 onwards
  
  # calculate population shares for the age groups
  pop_detailed <- pop_detailed %>%
    dplyr::left_join(pop %>% dplyr::filter(indicator == "pop") %>% dplyr::select(year, nuts3_id, value),
                     by = c("year", "nuts3_id"), suffix = c("", ".total")) %>% 
    dplyr::mutate(value = value/value.total,
                  indicator = str_replace(indicator, "pop", "pop_share")) %>% 
    dplyr::select(-value.total)
  
  pop <- rbind(pop, pop_detailed)
  
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
    dplyr::ungroup() %>% 
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
  
  # 5.) Renewable Energy Shares -------------------------------------------
  # only available on a country level
  shares <- get_eurostat("nrg_ind_ren") %>% 
    dplyr::filter(nchar(geo) == 2) %>% 
    dplyr::mutate(time = format(as.Date(time, format="%Y/%m/%d"),"%Y")) %>% 
    dplyr::select(unit = nrg_bal, geo, time, values)
  
  shares <- gdppc %>% dplyr::select(nuts3_id = geo, year = time) %>% 
    distinct(nuts3_id, year) %>% 
    dplyr::mutate(cntr_id = substr(nuts3_id, 1,2),
                  year = as.character(year)) %>% 
    dplyr::right_join(shares, by = c("cntr_id" = "geo", "year" = "time")) %>% 
    dplyr::select(indicator = unit,
                  nuts3_id,
                  year,
                  value = values) %>% 
    dplyr::filter(!is.na(nuts3_id)) %>% 
    dplyr::mutate(value = value/100) # to make it range from 0-1
  
  # 6.) Annual road transport data
  # The data is available per:
  # region of loading
  # region of unloading
  # I used the sum of both for both (unloading and loading) for final data
  # maybe the data should be put in relative terms to GDP or something else later on
  
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
  
  # 7.) Heating and Cooling Degree Days
  # nrg_chdd_a
  heating_cooling <- get_eurostat("nrg_chddr2_a") %>% 
    dplyr::filter(nchar(geo) == 5,
                  unit == "NR") %>% 
    dplyr::mutate(time = format(as.Date(time, format="%Y/%m/%d"),"%Y"),
                  indic_nrg = tolower(indic_nrg)) %>% 
    dplyr::select(unit = indic_nrg,
                  geo, time, values)
  
  # Combine Data and save:--------------------------------------------------------
  
  # combine
  fineurostat <- rbind(gdp, gdppc, empl, gva, road, heating_cooling)
  colnames(fineurostat) <- c("indicator","nuts3_id", "year", "value")
  fineurostat <- rbind(pop, shares, fineurostat)
  
  # save
  saveRDS(fineurostat, "./input/data_eurostat.rds")
}

