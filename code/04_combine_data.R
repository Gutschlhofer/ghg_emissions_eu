# setup ------------------------------------------------------------------------
year_single <- 2018
year_filter <- c(2007:2018) # age groups are only available from 2007 onwards

# get independent variables ----------------------------------------------------
# shapefile
shape_nuts3 <- getShapefile()
# eurostat
data_eurostat <- readRDS("input/data_eurostat.rds") %>% 
  dplyr::filter(year %in% year_filter) %>% 
  tidyr::pivot_wider(names_from = "indicator", values_from = "value")
# heating and cooling days
data_heating_cooling <- readRDS("input/data_heating_cooling.rds") %>% 
  dplyr::filter(year %in% year_filter) %>% 
  dplyr::mutate(indicator = tolower(indicator)) %>% 
  tidyr::pivot_wider(names_from = "indicator", values_from = "value") %>% 
  dplyr::mutate(year = as.character(year))

# get emission data ------------------------------------------------------------

# prepare to transform CH4 and N2O to "tonnes CO2 equivalents"
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Carbon_dioxide_equivalent
is_not_co2 <- c("CH4", "N2O")
transform_to_co2_eq <- function(value, type) {
  factor <- switch(type,
                   "CH4" = 25,
                   "N2O" = 298)
  return(value*factor)
}

data_edgar <- readRDS("input/data_edgar_all.rds") %>%  
  dplyr::filter(year %in% year_filter) %>% 
  dplyr::mutate(indicator = str_replace(indicator, "CO2_excl_short-cycle_org_C", "CO2_long")) %>% 
  dplyr::mutate(indicator = str_replace(indicator, "CO2_org_short-cycle_C", "CO2_short")) %>% 
  # convert CH4 to tonnes CO2 equiv
  dplyr::mutate(value = ifelse(grepl("ch4", tolower(indicator)), 
                               transform_to_co2_eq(value, "CH4"), 
                               value)) %>% 
  # convert N2O to tonnes CO2 equiv
  dplyr::mutate(value = ifelse(grepl("n2o", tolower(indicator)), 
                               transform_to_co2_eq(value, "N2O"), 
                               value)) %>% 
  # exclude CH4_PRO_COAL, OIL, GAS since they are already contained in CH4_PRO
  dplyr::filter(!grepl("edgar_CH4_PRO_", indicator, fixed = TRUE)) %>% 
  # in case there are any NA values in the edgar data, replace them with 0
  dplyr::mutate(value = ifelse(is.na(value), 0, value))

# add total sectoral GHG emissions
sectors <- read.csv("input/edgar/edgar_sectors.csv")$short
sectors <- sectors[!grepl("PRO_", sectors, fixed = TRUE)]

for(sector in sectors) {
  # 1. filter for one sector, aggregate
  tmp <- data_edgar %>% 
    dplyr::filter(grepl(paste0("_",sector), indicator)) %>% 
    dplyr::group_by(year, nuts3_id) %>% 
    dplyr::summarise(value = sum(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(indicator = paste0("edgar_GHG_", sector))
  
  # 2. add GHG sector aggregate to original data
  data_edgar <- data_edgar %>% 
    rbind(tmp)
}

# get all dependent variables
dep_variables <- c(unique(data_edgar$indicator), "edgar", "edgar_co2_total")

data_edgar <- data_edgar %>% 
  tidyr::pivot_wider(names_from = "indicator", values_from = "value") %>%
  dplyr::mutate(year = as.character(year)) %>%
  dplyr::mutate(
    # GHG
    edgar = edgar_co2 + edgar_co2_short + edgar_ch4 + edgar_n2o,
    # CO2 aggregate
    edgar_co2_total = edgar_co2 + edgar_co2_short
)

# combine data -----------------------------------------------------------------
data <- shape_nuts3 %>% 
  dplyr::left_join(data_eurostat, by = c("nuts3_id")) %>% 
  dplyr::left_join(data_edgar, by = c("nuts3_id", "year")) %>%
  dplyr::left_join(data_heating_cooling, by = c("nuts3_id", "year"))

# remove loaded sub-data
rm(data_eurostat, data_edgar, data_heating_cooling)

summary(data)

# quick check for how many variables have observations with missing values
gg_miss_var(data)
# notes:
# gva:share has much better coverage than emp_share

# perform some calculations
data <- data %>% 
  dplyr::mutate(
    density = (pop/area)/1000, # pop per m2, density in 1000people/km2
    heating_or_cooling = hdd + cdd,
    cdd_fix = cdd + 1, # move our scale by 1 to be able to log it
    hdd_non_REN = hdd * (1-REN_HEAT_CL),
    cdd_non_REN = cdd_fix * (1-REN_HEAT_CL)
  )

# # check for an exclude countries with missing values in 2016
# data %>% filter(gva_share_F %>% is.na, year == 2016) %>% pull(cntr_code) %>% unique()
# data %>% filter(gdppc %>% is.na, year == 2016) %>% pull(cntr_code) %>% unique()
# data %>% filter(pop %>% is.na, year == 2016) %>% pull(cntr_code) %>% unique()
# data %>% filter(heating_or_cooling %>% is.na, year == 2016) %>% pull(cntr_code) %>% unique()

# exclude countries for data availability reasons and/or for being an island
exclude <- c(
  "AL" # CDD, HDD
  , "CH" # gdppc, gva_share
  , "IE" # island, gdppc, gva_share
  , "LI" # CDD, HDD, gdppc, gva_share
  , "ME" # CDD, HDD
  , "MK" # CDD, HDD
  , "MT" # island
  , "NO" # pop, gva_share, gdppc not available
  , "RS" # pop, CDD, HDD
  , "SE" # since March 2021, Sweden is missing NUTS3 level GVA shares
  , "TR" # CDD, HDD
  , "UK" # island, gdppc, gva_share
)
data <- data %>% dplyr::filter(!cntr_code %in% exclude)

# data %>% pull(cntr_code) %>% unique %>% sort

# # remove some observations that have no direct neighbours (for queen contiguity)
# lw_queen <- poly2nb(data %>% dplyr::filter(year == 2016), queen = TRUE) # 7 regions with no links: 505 506 521 539 570 571 608
# (data %>% dplyr::filter(year == 2016))[c(505,506,521,539,570,571,608),] %>% pull(nuts3_id) %>% unique
no_neigh <- c("EL623", "EL624", "DK014", "FI200", "EL621", "EL622", "DK031")
data <- data %>% dplyr::filter(!(nuts3_id %in% no_neigh))

data_panel <- data
data <- data %>% dplyr::filter(year == year_single)

# some treatment for outliers in new data objects
data[data$edgar > quantile(data$edgar,0.99),]$edgar

data_fix_outlier <- data %>% 
  dplyr::mutate(edgar = ifelse(edgar > quantile(edgar,0.99), quantile(edgar,0.99), edgar),
                density = ifelse(density > quantile(density,0.99), quantile(density,0.99), density),
                gdppc = ifelse(gdppc > quantile(gdppc,0.99), quantile(gdppc,0.99), gdppc))

data_filter_outlier <- data %>% 
  dplyr::filter(!(edgar > quantile(edgar,0.99)),
                !(density > quantile(density,0.99)),
                !(gdppc > quantile(gdppc,0.99)))

# # we check the EDGAR country aggregates with the EDGAR values
# edgar_input <- readxl::read_xls("input/v50_CO2_excl_short-cycle_org_C_1970_2018.xls",
#                                 sheet = 3, skip = 9) %>%
#   dplyr::mutate(cntr_code = countrycode::countrycode(ISO_A3, "iso3c", "iso2c",
#                                                      custom_match = c("GRC" = "EL",
#                                                                       "GBR" = "UK")))
# 
# data_with_edgar <- data %>%
#   st_drop_geometry() %>%
#   dplyr::group_by(cntr_code) %>%
#   dplyr::summarise(edgar = sum(edgar)) %>%
#   dplyr::left_join(edgar_input[,c("cntr_code", "ISO_A3", "2016")], by = "cntr_code") %>%
#   dplyr::mutate(scale = edgar/`2016`/1e3) # gigagramms in t: 1e3

# Create dataset for NUTS2 Analysis (MAUP)--------------------------------------
data_nuts2 <- data
data_nuts2$nuts2_id <- substr(data_nuts2$nuts3_id, 1, 4) 

ghg_aggregate_over_sector <- c("edgar", "edgar_co2", "edgar_co2_short", "edgar_co2_total", "edgar_ch4", "edgar_n2o")

data_nuts2 <- data_nuts2 %>% 
  group_by(nuts2_id) %>% 
  summarise(cntr_code = first(cntr_code),
            edgar = sum(edgar),
            edgar_co2 = sum(edgar_co2),
            edgar_co2_short = sum(edgar_co2_short),
            edgar_co2_total = sum(edgar_co2_total),
            edgar_ch4 = sum(edgar_ch4),
            edgar_n2o = sum(edgar_n2o),
            REN = mean(REN),
            REN_ELC = mean(REN_ELC),
            REN_HEAT_CL = mean(REN_HEAT_CL),
            REN_TRA = mean(REN_TRA),
            area = sum(area), 
            pop = sum(pop), 
            hdd = mean(hdd), 
            cdd = mean(cdd))

# gva share
gva_nuts2 <- get_eurostat("nama_10r_3gva") %>% 
  dplyr::filter(nchar(geo) == 4,
                currency == "MIO_EUR",
                nace_r2 %in% c("A", "B-E", "F", "G-J", "TOTAL"))
gva_nuts2_total <- gva_nuts2 %>% dplyr::filter(nace_r2 == "TOTAL")
gva_nuts2 <- gva_nuts2 %>% 
  dplyr::filter(nace_r2 != "TOTAL") %>% 
  dplyr::left_join(gva_nuts2_total[,c("geo","time","values")], 
                   by = c("geo" = "geo", "time" = "time"),
                   suffix = c("", "_total")) %>% 
  dplyr::mutate(gvashare = values / values_total) %>% 
  dplyr::select(nace_r2, geo, time, gvashare) %>% 
  dplyr::mutate(time = format(as.Date(time, format="%Y/%m/%d"),"%Y")) %>% 
  dplyr::filter(time == year_single) %>% 
  dplyr::mutate(nace_r2 = gsub("-", "", nace_r2),
                nace_r2 = paste0("gva_share_",nace_r2)) %>% 
  dplyr::select(indicator = nace_r2, nuts2_id = geo, year = time, value = gvashare) %>% 
  tidyr::pivot_wider(names_from = "indicator", values_from = "value") %>% 
  dplyr::select(-year)

rm(gva_nuts2_total)

data_nuts2 <- dplyr::left_join(data_nuts2, gva_nuts2, by="nuts2_id")

# gdp
gdp_nuts2 <- get_eurostat("nama_10r_3gdp", filters = list(unit = "MIO_PPS_EU27_2020")) %>% 
  filter(nchar(geo) == 4 & time == "2018-01-01") 
gdp_nuts2 <- gdp_nuts2 %>% dplyr::rename(nuts2_id = geo, gdp = values)
data_nuts2 <- dplyr::left_join(data_nuts2, gdp_nuts2 %>% dplyr::select(-c(time, unit)), by="nuts2_id")

# gdppc 
gdppc_nuts2 <- get_eurostat("nama_10r_3gdp", filters = list(unit = "PPS_EU27_2020_HAB")) %>% 
  filter(nchar(geo) == 4 & time == "2018-01-01")  
gdppc_nuts2 <- gdppc_nuts2 %>% dplyr::rename(nuts2_id = geo, gdppc = values)
gdppc_nuts2$time <- format(as.Date(gdppc_nuts2$time, format="%Y/%m/%d"),"%Y")
data_nuts2 <- dplyr::left_join(data_nuts2, gdppc_nuts2 %>% dplyr::select(-c(time, unit)), by="nuts2_id")

data_nuts2 <- data_nuts2 %>%
  dplyr::mutate(
    heating_or_cooling = hdd + cdd,
    density = pop/area,
    cdd_fix = cdd+1, 
    cdd_log = ifelse(cdd == 0, 0, log(cdd)) )

summary(data_nuts2)

# save the nuts2 data
saveRDS(data_nuts2, "input/data_nuts2.rds")

# Further prepare data for analysis --------------------------------------------

# prepare log_gdppc, the logged value of gdppc centered around the mean
data <- data %>% 
  dplyr::mutate(log_gdppc = log(gdppc) - mean(log(gdppc)))
data_nuts2 <- data_nuts2 %>% 
  dplyr::mutate(log_gdppc = log(gdppc) - mean(log(gdppc)))
data_panel <- data_panel %>% 
  dplyr::mutate(log_gdppc = log(gdppc) - mean(log(gdppc)))

# Summary stats for paper  -----------------------------------------------------
dep_var <- c("edgar", "edgar_co2_total", "edgar_ch4", "edgar_n2o")
dep_var_label <- c("GHG", "CO2 total", "CH4", "N2O")

temp <- st_drop_geometry(data) %>% 
  dplyr::select(all_of(dep_var), pop, density, gdppc,
                gva_share_A, gva_share_BE, gva_share_F, gva_share_GJ,
                hdd, cdd_fix) %>%
  dplyr::mutate_at(all_of(dep_var), round, digits = 1) %>% 
  mutate(gdppc = round(gdppc, digits = 1),
         hdd = round(hdd, digits = 1),
         cdd_fix = round(cdd_fix, digits = 1)
  )

colnames(temp) <- c(dep_var_label, "Population", "Density", "GDP/cap", 
                    "GVA A", "GVA BE", "GVA F", "GVA GJ",
                    "HDD", "CDD")

stargazer(temp, digits = 2, median = TRUE, type = "text")
stargazer(temp, digits = 2, median = TRUE, out = "output/tables/summary_abs.tex")

# Create cor tables ------------------------------------------------------------
temp <- st_drop_geometry(data)
# correlation of actual values
cortab <- cor(temp %>% dplyr::select(edgar = all_of(dep_var), 
                                     pop, pop_share_Y15_64, pop_share_Y_GE65,
                                     density, gdppc, 
                                     gva_share_A, gva_share_BE, gva_share_F, gva_share_GJ, 
                                     hdd, cdd_fix))
rownames(cortab) <-  colnames(cortab) <- c(dep_var, 
                                           "Population",
                                           "Population share (15-64)",
                                           "Population share (65+)",
                                           "Density", "GDP/cap",
                                           "GVA A", "GVA BE", "GVA F", "GVA GJ",
                                           "HDD", "CDD")
stargazer(cortab, column.sep.width = "0pt",
          title = "Correlation Coefficients",
          out = "output/tables/summary_cor.tex")

temp_dep <- temp %>% 
  dplyr::select(all_of(dep_var)) %>% 
  dplyr::mutate_all(log)

temp_log <- temp %>% 
  dplyr::mutate(
    pop = log(pop),
    pop_share_Y15_64 = log(pop_share_Y15_64),
    pop_share_Y_GE65 = log(pop_share_Y_GE65),
    gdppc = log_gdppc,
    gdppc2 = log_gdppc^2,
    density = log(density),
    gva_share_A = log(gva_share_A),
    gva_share_BE = log(gva_share_BE),
    gva_share_F = log(gva_share_F),
    gva_share_GJ = log(gva_share_GJ),
    hdd = log(hdd),
    cdd_fix = log(cdd_fix))
cortab.log <- cor(cbind(temp_log, temp_dep) %>% 
                    dplyr::select(all_of(dep_var), 
                                  pop, pop_share_Y15_64, pop_share_Y_GE65,
                                  density, gdppc, gdppc2,
                                  starts_with("gva_share_"), hdd, cdd_fix))
rownames(cortab.log) <- colnames(cortab.log) <- 
  c(dep_var_label, "Population", "Population share (15-64)", "Population share (65+)",
    "Density", "GDP/cap", "GDP/cap, squared", 
    "GVA A", "GVA BE", "GVA F", "GVA GJ",
    "HDD", "CDD")

stargazer(cortab.log, column.sep.width = "0pt",
          title = "Correlation of logged model variables",
          out = "output/tables/summary_cor_log.tex")

# check for correlation between urban type and urbanisation
data$urbn_type_1 <- ifelse(data$urbn_type == "1", 1, 0)
polycor::hetcor(data %>% st_drop_geometry %>%  dplyr::select(density, urbn_type_1))

# ------------------------------------------------------------------------------
# include sector aggregates in the data panel

sector_detail <- "category_main"

data_m <- data_panel %>% 
  dplyr::select(year, 
                nuts3_id,
                starts_with("edgar_"),
                # -starts_with("edgar_GHG"),
                # small caps is only totals
                -starts_with("edgar_co2", ignore.case = FALSE),
                -edgar_n2o, -edgar_ch4
  ) %>% 
  tidyr::pivot_longer(cols = starts_with("edgar_"), #c(nuts3_id, cntr_code, year),
                      names_to = "indicator")

data_m <- data_m %>% 
  dplyr::mutate(ghg = get_ghg_name_from_indicator(indicator),
                sector = get_sector_name_from_indicator(indicator))

# now we join the sectors list to either aggregate by 
# sector name, category or even main category
data_m <- data_m %>% 
  dplyr::left_join(read.csv("input/edgar/edgar_sectors.csv")[,c("short", sector_detail)],
                   by = c("sector" = "short")) %>% 
  dplyr::rename(short = sector) %>% 
  # dplyr::select(-sector) %>% 
  dplyr::rename(sector = all_of(sector_detail))

# group all transport sectors into one
data_m <- data_m %>% 
  dplyr::mutate(sector = ifelse(grepl("TNR", short), ifelse(sector_detail != "category_main", "Transport", "Energy - Transport"), sector)) %>% 
  dplyr::mutate(sector = ifelse(grepl("TRO", short), ifelse(sector_detail != "category_main", "Transport", "Energy - Transport"), sector)) %>% 
  dplyr::mutate(sector = ifelse(sector == "Energy", "Energy - Industry", sector)) %>% 
  dplyr::mutate(sector = ifelse(sector == "Fuel combustion activities", "Fuel combustion activities - Industry", sector))

data_ghg_sector_longer <- data_m %>% 
  dplyr::group_by(year, ghg, sector, nuts3_id) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    sector = gsub(" ", "", sector),
    sector = gsub("-", "", sector),
    sector = gsub(",", "", sector),
    indicator = sprintf("edgar_agg_%s_%s", ghg, sector)
  )

data_ghg_sector <- data_ghg_sector_longer %>% 
  dplyr::select(year, nuts3_id, value, indicator) %>% 
  tidyr::pivot_wider(id_cols = c("year", "nuts3_id"), names_from = "indicator")

rm(data_m)

data <- data %>% 
  dplyr::left_join(data_ghg_sector %>% filter(year == year_single), by = c("nuts3_id", "year"))
data_panel <- data_panel %>% 
  dplyr::left_join(data_ghg_sector, by = c("nuts3_id", "year"))

dep_variables <- colnames(data)[grepl("edgar", colnames(data))]
dep_variables_agg_ghg <- colnames(data)[grepl("edgar_agg_GHG_", colnames(data))]
dep_variables_agg <- colnames(data)[grepl("edgar_agg_", colnames(data))]
dep_variables_sel <- c("edgar", 
                       "edgar_co2_total", "edgar_co2", "edgar_co2_short",
                       "edgar_ch4",
                       "edgar_n2o",
                       dep_variables_agg)

# ------------------------------------------------------------------------------

summary(data)
saveRDS(data, "input/data.rds")
saveRDS(data_panel, "input/data_panel.rds")
saveRDS(data_fix_outlier, "input/data_fix_outlier.rds")
saveRDS(data_filter_outlier, "input/data_filter_outlier.rds")
