## This file is used to put the CDD and HDD data from agricast to our format ---

if(! file.exists("./input/data_heating_cooling.rds")) {
  # heating and cooling days
  data_heating_cooling <- 
    read.csv("input/agri4cast/CDD_HDD_1979-2020_ver2021-1_0_20262_456289399.csv", 
             sep = ";") %>% 
    dplyr::filter(nchar(NUTS_CODE) == 5) %>% 
    pivot_longer(cols = c("CDD", "HDD"), names_to = "indicator") %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::rename(nuts3_id = nuts_code) %>% 
    # summarise to get the sum of each year
    dplyr::group_by(nuts3_id, year, indicator) %>% 
    dplyr::filter(year != 1978) %>% # 1978 is the average over all years (1979-2020)
    dplyr::summarize(value = sum(value, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  # save
  saveRDS(data_heating_cooling, "./input/data_heating_cooling.rds")
}