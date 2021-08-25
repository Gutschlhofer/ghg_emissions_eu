url <- "https://ec.europa.eu/eurostat/documents/24987/9752807/Release+dataset/e098dbcd-3af3-f1bd-5c65-bd51c93a0560"
filename <- "GDP_releases.xlsx"
dir = "./input/eurostat"
dir.create(dir)

filename <- paste0(dir, "/", filename)

if(!file.exists(filename)) download.file(url = url, destfile = filename)

# get desired variable, select only French NUTS2 regions
data_gdp <- readxl::read_xlsx(path = filename, sheet = 2) %>% 
  dplyr::filter(unit == "PPS_HAB") %>% 
  dplyr::select(nuts2_id = geo, version = vintage, starts_with("2")) %>% 
  tidyr::pivot_longer(cols = starts_with("2"), names_to = "year") %>% 
  dplyr::filter(!is.na(value), 
                value != "NA", 
                nchar(nuts2_id) == 4) %>% 
  dplyr::mutate(cntr_code = substr(nuts2_id, 1, 2),
                version = substr(version, 2, 5)) %>% 
  dplyr::filter(cntr_code == "FR")
# only choose most up-to-date version for each NUTS2 region
# 1. find most up-to-date version
latest <- data_gdp %>% 
  dplyr::select(-cntr_code) %>% 
  dplyr::filter(year == "2000") %>% 
  dplyr::group_by(nuts2_id) %>% 
  dplyr::summarise(max_version = max(version)) %>% 
  dplyr::ungroup()
# 2. filter everything else
data_gdp <- data_gdp %>% 
  dplyr::left_join(latest, by = c("nuts2_id")) %>% 
  dplyr::filter(version == max_version) %>% 
  dplyr::select(-max_version, -version, -cntr_code) %>% 
  dplyr::rename(value.nuts2 = value)

missing_gdp <- data_panel %>% 
  dplyr::group_by(nuts3_id) %>% 
  dplyr::filter(any(is.na(gdppc))) %>% 
  dplyr::mutate(first_year = min(year[!is.na(gdppc)],na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  st_drop_geometry() %>% 
  dplyr::select(year, first_year, nuts3_id, gdppc)

if(nrow(missing_gdp) > 0) {
  missing_gdp <- missing_gdp %>% 
    dplyr::mutate(nuts2_id = substr(nuts3_id, 1, 4)) %>% 
    dplyr::left_join(data_gdp, by = c("nuts2_id", "year")) %>% 
    # every region has a NUTS2 region with values!    
    dplyr::group_by(nuts3_id) %>%
    dplyr::mutate(
      share_to_nuts2 = gdppc/value.nuts2,
      gdppc = ifelse(!is.na(gdppc),
                     gdppc, # use real value if we have it
                     share_to_nuts2[year==first_year]*value.nuts2)
    ) %>% 
    dplyr::ungroup()
  
  # now we update the imputed numbers in the main data_panel
  data_panel <- data_panel %>% 
    dplyr::left_join(missing_gdp, by = c("nuts3_id", "year"), suffix = c("", ".imputed")) %>% 
    dplyr::mutate(
      gdppc = ifelse(!is.na(gdppc), gdppc, gdppc.imputed)
    ) %>% 
    dplyr::select(-ends_with(".imputed"), -ends_with("nuts2"))
}
