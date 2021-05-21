## This file is used to put the CDD and HDD data from agricast to our format ---

if(! file.exists("./input/data_heating_cooling.rds")) {
  # heating and cooling days
  data_heating_cooling <- 
    read.csv("input/temp_heating_cooling/agricast_cdd_hdd_ver2020-1_0_18494_868222495.csv", 
             sep = ";") %>% 
    dplyr::filter(nchar(NUTS_CODE) == 5) %>% 
    pivot_longer(cols = c("CDD", "HDD"), names_to = "indicator") %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::rename(nuts3_id = nuts_code) %>% 
    dplyr::group_by(nuts3_id, year, indicator) %>% 
    dplyr::summarize(value = sum(value, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  # save
  saveRDS(data_heating_cooling, "./input/data_heating_cooling.rds")
}