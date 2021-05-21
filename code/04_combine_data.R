# setup ------------------------------------------------------------------------
year_filter <- 2016

# get data ---------------------------------------------------------------------
# shapefile
shape_nuts3 <- getShapefile()
# eurostat
data_eurostat <- readRDS("input/data_eurostat.rds") %>% 
  dplyr::filter(year == year_filter) %>% 
  pivot_wider(names_from = "indicator", values_from = "value")
# EDGAR
data_edgar <- readRDS("input/data_edgar.rds") %>%  
  dplyr::filter(indicator == "edgar_co2") %>%
  dplyr::filter(year == year_filter) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  dplyr::mutate(year = as.character(year)) %>%
  rename(edgar = "edgar_co2") # only use co2 excl short cycle, we can add other greenhouse gases by summing them
# heating and cooling days
data_heating_cooling <- readRDS("input/data_heating_cooling.rds") %>% 
  dplyr::filter(year == year_filter) %>% 
  dplyr::mutate(indicator = tolower(indicator)) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>% 
  dplyr::mutate(year = as.character(year))

# combine data
data <- shape_nuts3 %>% 
  left_join(data_eurostat, by = c("nuts3_id")) %>% 
  left_join(data_edgar, by = c("nuts3_id", "year")) %>%
  left_join(data_heating_cooling, by = c("nuts3_id", "year"))

# remove loaded sub-data
rm(data_eurostat, data_edgar, data_heating_cooling)

summary(data)

# perfrom some calculations
data <- data %>% 
  dplyr::mutate(
    # gdppc_2 = gdppc^2,
    density = (pop/area)/1000, # pop per m2, density in 1000people/km2
    heating_or_cooling = hdd + cdd,
    cdd_fix = cdd+1, # move our scale by 1 to be able to log it
    cdd_log = ifelse(cdd == 0, 0, log(cdd))
  )

# exclude so we have gdp, population and GWA share BE
exclude <- c("NO","CH", "TR", "RS", "IE", "UK", "LI", "MT")
data <- data[!data$cntr_code%in%exclude,]
# exclude so we have CDD,HDD
exclude <- c("AL","MK","ME")
data <- data[!data$cntr_code%in%exclude,]

# fix outliers
data[data$edgar > quantile(data$edgar,0.99),]$edgar

data_fix_outlier <- data %>% 
  dplyr::mutate(edgar = ifelse(edgar > quantile(edgar,0.99), quantile(edgar,0.99), edgar),
                density = ifelse(density > quantile(density,0.99), quantile(density,0.99), density),
                gdppc = ifelse(gdppc > quantile(gdppc,0.99), quantile(gdppc,0.99), gdppc))

data_filter_outlier <- data %>% 
  dplyr::filter(!(edgar > quantile(edgar,0.99)),
                !(density > quantile(density,0.99)),
                !(gdppc > quantile(gdppc,0.99)))


# we check the EDGAR country aggregates with the EDGAR values
edgar_input <- readxl::read_xls("input/v50_CO2_excl_short-cycle_org_C_1970_2018.xls",
                                sheet = 3, skip = 9) %>%
  dplyr::mutate(cntr_code = countrycode::countrycode(ISO_A3, "iso3c", "iso2c",
                                                     custom_match = c("GRC" = "EL",
                                                                      "GBR" = "UK")))

data_with_edgar <- data %>%
  st_drop_geometry() %>%
  dplyr::group_by(cntr_code) %>%
  dplyr::summarise(edgar = sum(edgar)) %>%
  dplyr::left_join(edgar_input[,c("cntr_code", "ISO_A3", "2016")], by = "cntr_code") %>%
  dplyr::mutate(scale = edgar/`2016`/1e3) # gigagramms in t: 1e3

summary(data)
saveRDS(data, "input/data.rds")
saveRDS(data_fix_outlier, "input/data_fix_outlier.rds")
saveRDS(data_filter_outlier, "input/data_filter_outlier.rds")

# Summary stats for paper  -----------------------------------------------------

temp <- st_drop_geometry(data) %>% 
  dplyr::select(edgar, pop, density, gdppc,  gwa_share_BE, hdd, cdd_fix) %>%
  mutate(edgar = round(edgar, digits = 1),
         gdppc = round(gdppc, digits = 1),
         hdd = round(hdd, digits = 1),
         cdd_fix = round(cdd_fix, digits = 1)
         )

colnames(temp) <- c("CO2", "Population", "Density", "GDP/cap", "GWA", 
                    "HDD", "CDD")

stargazer(temp, digits = 2, median = TRUE)


# Create cor tables ------------------------------------------------------------
temp <- st_drop_geometry(data)
temp$gdppc2 <- data$gdppc^2
# correlation of actual values
cortab <- cor(temp %>% dplyr::select(edgar, pop, density, gdppc, gdppc2,  gwa_share_BE, hdd, cdd_fix))
rownames(cortab) <-  colnames(cortab) <- c("CO2", "Population", "Density", "GDP/cap", "GDP/cap^2", 
                                            "GWA", "HDD", "CDD")
stargazer(cortab, column.sep.width = "0pt", 
          title = "Correlation Coefficients",
          out = "output/tables/cor.tex")
# correlation of log values (care: different gdppc2 specification)
temp_log <- temp %>% mutate(
  edgar = log(edgar),
  pop = log(pop),
  gdppc = log(gdppc),
  gdppc2 = log(gdppc)^2,
  density = log(density),
  gwa_share_BE = gwa_share_BE,
  hdd = log(hdd),
  cdd_fix = log(cdd_fix))
cortab.log <- cor(temp_log %>% dplyr::select(edgar, pop, density, gdppc, gdppc2,  gwa_share_BE, hdd, cdd_fix))
rownames(cortab.log) <-  colnames(cortab.log) <- c("CO2", "Population", "Density", "GDP/cap", "GDP/cap, squared", 
                                           "GWA", "HDD", "CDD")

stargazer(cortab.log, column.sep.width = "0pt",
          title = "Correlation of Model Variables",
            out = "output/tables/cor_log.tex")

# Create dataset for NUTS2 Analysis (MAUP)--------------------------------------

data_nuts2 <- data
data_nuts2$nuts2_id <- substr(data_nuts2$nuts3_id, 1, 4) 

data_nuts2 <- data_nuts2 %>% 
  group_by(nuts2_id) %>% 
  summarise(cntr_code = first(cntr_code),
            edgar = sum(edgar),
            area = sum(area), 
            pop = sum(pop), 
            hdd = mean(hdd), 
            cdd = mean(cdd))


# gwa share
gwa_nuts2 <- get_eurostat("nama_10r_3gva") %>% 
  filter(nchar(geo) == 4 & currency == "MIO_EUR") %>% 
  filter(nace_r2 != "C" & nace_r2 != "G-J" & nace_r2 != "K-N" 
         & nace_r2 != "O-Q" & nace_r2 != "R-U" & nace_r2 != "TOTAL") %>% 
  group_by(geo, time) %>% 
  mutate(gwashare = round(values/sum(values),3)) %>% 
  dplyr::select(nace_r2, geo, time, gwashare)
gwa_nuts2$time <- format(as.Date(gwa_nuts2$time, format="%Y/%m/%d"),"%Y")

gwaind_nuts2 <- gwa_nuts2 %>% 
  filter(nace_r2 == "B-E" & time == 2016 ) 

gwaind_nuts2 <- gwaind_nuts2 %>%  rename(nuts2_id = geo, gwa_share_BE = gwashare)
gwaind_nuts2$time <- format(as.Date(gwaind_nuts2$time, format="%Y/%m/%d"),"%Y")
data_nuts2 <- left_join(data_nuts2, gwaind_nuts2 %>% dplyr::select(-c(time, nace_r2)), by="nuts2_id")


# gdp
gdp_nuts2 <- get_eurostat("nama_10r_3gdp", filters = list(unit = "MIO_PPS")) %>% 
  filter(nchar(geo) == 4 & time == "2016-01-01") 
gdp_nuts2 <- gdp_nuts2 %>% rename(nuts2_id = geo, gdp = values)
data_nuts2 <- left_join(data_nuts2, gdp_nuts2 %>% dplyr::select(-c(time, unit)), by="nuts2_id")


# gdppc 
gdppc_nuts2 <- get_eurostat("nama_10r_3gdp", filters = list(unit = "PPS_HAB")) %>% 
  filter(nchar(geo) == 4 & time == "2016-01-01")  
gdppc_nuts2 <- gdppc_nuts2 %>% rename(nuts2_id = geo, gdppc = values)
gdppc_nuts2$time <- format(as.Date(gdppc_nuts2$time, format="%Y/%m/%d"),"%Y")
data_nuts2 <- left_join(data_nuts2, gdppc_nuts2 %>% dplyr::select(-c(time, unit)), by="nuts2_id")

data_nuts2 <- data_nuts2 %>%
dplyr::mutate(
  heating_or_cooling = hdd + cdd,
  density = pop/area,
  cdd_fix = cdd+1, 
  cdd_log = ifelse(cdd == 0, 0, log(cdd)) )

# save the nuts2 data
saveRDS(data_nuts2, "input/data_nuts2.rds")

