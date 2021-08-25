data_panel_na <- data_panel %>% 
  dplyr::group_by(nuts3_id) %>% 
  # only select nuts3_ids that have missing population values
  dplyr::filter(any(is.na(pop))) %>% 
  dplyr::ungroup()

# # get numbers of regions that with NA values and the total values for the paper
# data_panel_na %>% filter(year == 2010) %>% group_by(cntr_code) %>% summarise(n = n())
# data_panel %>% filter(year == 2010, cntr_code %in% data_panel_na$cntr_code) %>% group_by(cntr_code) %>% summarise(n = n())

if(nrow(data_panel_na) > 0) {
  data_panel_na <- data_panel_na %>% 
    dplyr::mutate(nuts2_id = substr(nuts3_id, 1, 4)) %>% 
    dplyr::mutate(year = as.character(year))
  
  selected <- data_panel_na %>% 
    dplyr::select(nuts2_id, nuts3_id, year, starts_with("pop")) %>% 
    st_drop_geometry() %>% 
    group_by(nuts3_id) %>% 
    # get the first year, where data is present
    dplyr::mutate(first_year = min(year[!is.na(pop)],na.rm=T)) %>% 
    dplyr::ungroup()
  
  pop <- get_eurostat("demo_r_pjanaggr3") %>% 
    dplyr::filter(
      sex == "T"
      ,nchar(geo) == 4
      ,geo %in% data_panel_na$nuts2_id
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
    dplyr::select(year, nuts2_id = geo, value = values, indicator = unit) %>% 
    dplyr::mutate(year = as.character(year)) %>% 
    tidyr::pivot_wider(id_cols = c("year", "nuts2_id"), names_from = "indicator")
  
  # idea: 
  # 1. keep age groups shares of closest year (doesn't change that much)
  # 2. adjust population values by taking closest share of current population
  
  # 1. get information about NUTS2 region population -----------------------------
  
  test <- selected %>% 
    dplyr::left_join(pop, by = c("nuts2_id", "year"), suffix=c("",".nuts2")) %>% 
    dplyr::group_by(nuts3_id) %>% 
    dplyr::mutate(
      pop_share_of_nuts2 = pop/pop.nuts2,
      pop = ifelse(!is.na(pop),
                   pop, # use population value if we have it
                   pop_share_of_nuts2[year==first_year]*pop.nuts2),
      pop_share_Y_LT15 = ifelse(!is.na(pop_share_Y_LT15), pop_share_Y_LT15, pop_share_Y_LT15[year==first_year]),
      pop_share_Y15_64 = ifelse(!is.na(pop_share_Y15_64), pop_share_Y15_64, pop_share_Y15_64[year==first_year]),
      pop_share_Y_GE65 = ifelse(!is.na(pop_share_Y_GE65), pop_share_Y_GE65, pop_share_Y_GE65[year==first_year])
      # pop_share_Y20_34 = ifelse(!is.na(pop_share_Y20_34), pop_share_Y20_34, pop_share_Y20_34[year==first_year]),
      # pop_share_Y35_49 = ifelse(!is.na(pop_share_Y35_49), pop_share_Y35_49, pop_share_Y35_49[year==first_year]),
      # pop_share_Y50_64 = ifelse(!is.na(pop_share_Y50_64), pop_share_Y50_64, pop_share_Y50_64[year==first_year])
    ) %>% 
    dplyr::ungroup()
  
  # 2. get information about NUTS1 region population -----------------------------
  
  # issue: there are still some regions with missing 
  test2 <- test %>% 
    dplyr::group_by(nuts3_id) %>% 
    dplyr::filter(any(is.na(pop))) %>% 
    dplyr::mutate(nuts1_id = substr(nuts3_id, 1, 3)) %>% 
    dplyr::select(-contains("nuts2"))
  
  test <- test %>% group_by(nuts3_id) %>% dplyr::filter(!any(is.na(pop))) 
  
  pop <- get_eurostat("demo_r_pjanaggr3") %>% 
    dplyr::filter(
      sex == "T"
      ,nchar(geo) == 3
      ,geo %in% test2$nuts1_id
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
    dplyr::select(year, nuts1_id = geo, value = values, indicator = unit) %>% 
    dplyr::mutate(year = as.character(year)) %>% 
    tidyr::pivot_wider(id_cols = c("year", "nuts1_id"), names_from = "indicator")
  
  test3 <- test2 %>% 
    dplyr::left_join(pop, by = c("nuts1_id", "year"), suffix=c("",".nuts1")) %>% 
    dplyr::mutate(
      pop_share_of_nuts1 = pop/pop.nuts1,
      pop = ifelse(!is.na(pop),
                   pop, # use population value if we have it
                   pop_share_of_nuts1[year==first_year]*pop.nuts1)
    )
  
  # 3. get information about country population ----------------------------------
  
  # issue: there are still some regions in PL with missing pop values
  test4 <- test3 %>% 
    dplyr::filter(any(is.na(pop))) %>% 
    dplyr::mutate(cntr_code = substr(nuts3_id, 1, 2)) %>% 
    dplyr::select(-contains("nuts1"))
  
  test3 <- test3 %>% group_by(nuts3_id) %>% dplyr::filter(!any(is.na(pop))) 
  
  pop <- get_eurostat("demo_r_pjanaggr3") %>% 
    dplyr::filter(
      sex == "T"
      ,nchar(geo) == 2
      ,geo %in% test4$cntr_code
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
    dplyr::select(year, cntr_code = geo, value = values, indicator = unit) %>% 
    dplyr::mutate(year = as.character(year)) %>% 
    tidyr::pivot_wider(id_cols = c("year", "cntr_code"), names_from = "indicator")
  
  test5 <- test4 %>% 
    dplyr::left_join(pop, by = c("cntr_code", "year"), suffix=c("",".cntr_code")) %>% 
    dplyr::mutate(
      pop_share_of_cntr_code = pop/pop.cntr_code,
      pop = ifelse(!is.na(pop),
                   pop, # use population value if we have it
                   pop_share_of_cntr_code[year==first_year]*pop.cntr_code)
    )
  
  # test5 %>% filter(is.na(pop))
  # now there are no regions missing anymore
  
  new_data <- rbind(
    test %>% dplyr::ungroup() %>% dplyr::select("nuts3_id", "year", "pop", starts_with("pop_share_Y")),
    test3 %>% dplyr::ungroup() %>% dplyr::select("nuts3_id", "year", "pop", starts_with("pop_share_Y")),
    test5 %>% dplyr::ungroup() %>% dplyr::select("nuts3_id", "year", "pop", starts_with("pop_share_Y"))
  )
  
  # now we update the imputed numbers in the main data_panel
  data_panel <- data_panel %>% 
    dplyr::left_join(new_data, by = c("nuts3_id", "year"), suffix = c("", ".imputed")) %>% 
    dplyr::mutate(
      pop = ifelse(!is.na(pop), pop, pop.imputed),
      pop_share_Y_LT15 = ifelse(!is.na(pop_share_Y_LT15), pop_share_Y_LT15, pop_share_Y_LT15.imputed),
      pop_share_Y15_64 = ifelse(!is.na(pop_share_Y15_64), pop_share_Y15_64, pop_share_Y15_64.imputed),
      pop_share_Y_GE65 = ifelse(!is.na(pop_share_Y_GE65), pop_share_Y_GE65, pop_share_Y_GE65.imputed),
      pop_share_Y20_34 = ifelse(!is.na(pop_share_Y20_34), pop_share_Y20_34, pop_share_Y20_34.imputed),
      pop_share_Y35_49 = ifelse(!is.na(pop_share_Y35_49), pop_share_Y35_49, pop_share_Y35_49.imputed),
      pop_share_Y50_64 = ifelse(!is.na(pop_share_Y50_64), pop_share_Y50_64, pop_share_Y50_64.imputed),
      # since density has been calculated with missing population values
      density = (pop/area)/1000
    ) %>% 
    dplyr::select(-ends_with(".imputed"))
}
