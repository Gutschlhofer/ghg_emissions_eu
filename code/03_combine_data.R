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
  dplyr::mutate(indicator = str_replace(indicator, "CO2_excl_short-cycle_org_C", "CO2f")) %>% 
  dplyr::mutate(indicator = str_replace(indicator, "CO2_org_short-cycle_C", "CO2o")) %>% 
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
sectors <- read.csv("input/edgar/edgar_sectors.csv", stringsAsFactors = FALSE)$short
# exclude "PRO" subsectors that are already part of "PRO"
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
dep_variables <- c(unique(data_edgar$indicator), "edgar", "edgar_CO2total")

data_edgar <- data_edgar %>% 
  tidyr::pivot_wider(names_from = "indicator", values_from = "value") %>%
  dplyr::mutate(year = as.character(year)) %>%
  dplyr::mutate(
    # GHG
    edgar = edgar_CO2f + edgar_CO2o + edgar_CH4 + edgar_N2O,
    # CO2 aggregate
    edgar_CO2total = edgar_CO2f + edgar_CO2o
)

# combine data -----------------------------------------------------------------
data <- shape_nuts3 %>% 
  dplyr::left_join(data_eurostat, by = c("nuts3_id")) %>% 
  dplyr::left_join(data_edgar, by = c("nuts3_id", "year"))

# remove loaded sub-data
rm(data_eurostat, data_edgar)

summary(data)

# quick check for how many variables have observations with missing values
gg_miss_var(data)
# notes:
# gva share has much better coverage than emp_share

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

# # remove some observations that have no direct neighbours (for queen contiguity)
# lw_queen <- poly2nb(data %>% dplyr::filter(year == 2016), queen = TRUE) # 7 regions with no links: 505 506 521 539 570 571 608
# (data %>% dplyr::filter(year == 2016))[c(505,506,521,539,570,571,608),] %>% pull(nuts3_id) %>% unique
no_neigh <- c("EL623", "EL624", "DK014", "FI200", "EL621", "EL622", "DK031")
data <- data %>% dplyr::filter(!(nuts3_id %in% no_neigh))

data_panel <- data
data <- data %>% dplyr::filter(year == year_single)

# Further prepare data for analysis --------------------------------------------

# prepare log_gdppc, the logged value of gdppc centered around the mean
data <- data %>% 
  dplyr::mutate(log_gdppc = log(gdppc) - mean(log(gdppc)))
data_panel <- data_panel %>% 
  dplyr::mutate(log_gdppc = log(gdppc) - mean(log(gdppc)))

# Summary stats for paper  -----------------------------------------------------
dep_var <- c("edgar", "edgar_CH4","edgar_CO2f", "edgar_CO2o", "edgar_N2O")
dep_var_label <- c("GHG", "CH4", "CO2f", "CO2o", "N2O")

temp <- st_drop_geometry(data) %>% 
  dplyr::select(all_of(dep_var), pop, pop_share_Y_GE65, density, gdppc,
                gva_share_A, gva_share_BE, gva_share_F,
                hdd, cdd_fix, REN) %>%
  dplyr::mutate_at(all_of(dep_var), round, digits = 1) %>% 
  mutate(gdppc = round(gdppc, digits = 1),
         hdd = round(hdd, digits = 1),
         cdd_fix = round(cdd_fix, digits = 1)
  )

colnames(temp) <- c(dep_var_label, "Population", "Pop.share >=65", "Density", "GDP/cap", 
                    "GVA share A", "GVA share B-E", "GVA share F",
                    "HDD", "CDD", "REN")

stargazer(temp, digits = 2, median = TRUE, type = "text")
stargazer(temp, digits = 2, median = TRUE, out = "output/tables/summary_abs.tex")

# Create cor tables ------------------------------------------------------------
temp <- st_drop_geometry(data)
# correlation of absolute values
cortab <- cor(temp %>% dplyr::select(edgar = all_of(dep_var), 
                                     pop, pop_share_Y_GE65, density, gdppc, 
                                     gva_share_A, gva_share_BE, gva_share_F, 
                                     hdd, cdd_fix, REN))
rownames(cortab) <-  colnames(cortab) <- c(dep_var, 
                                           "Population",
                                           "Pop.share >=65",
                                           "Density", "GDP/cap",
                                           "GVA A", "GVA B-E", "GVA F",
                                           "HDD", "CDD", "REN")
stargazer(cortab, column.sep.width = "0pt", digits = 2,
          title = "Correlation Coefficients",
          out = "output/tables/summary_cor.tex")

temp_dep <- temp %>% 
  dplyr::select(all_of(dep_var)) %>% 
  dplyr::mutate_all(log)

temp_log <- temp %>% 
  dplyr::mutate(
    pop = log(pop),
    # pop_share_Y15_64 = log(pop_share_Y15_64),
    pop_share_Y_GE65 = log(pop_share_Y_GE65),
    gdppc = log_gdppc,
    # gdppc2 = log_gdppc^2,
    density = log(density),
    gva_share_A = log(gva_share_A),
    gva_share_BE = log(gva_share_BE),
    gva_share_F = log(gva_share_F),
    # gva_share_GJ = log(gva_share_GJ),
    hdd = log(hdd),
    cdd_fix = log(cdd_fix),
    REN = log(REN))
cortab.log <- cor(cbind(temp_log, temp_dep) %>% 
                    dplyr::select(all_of(dep_var), 
                                  pop, pop_share_Y_GE65,
                                  density, gdppc,
                                  starts_with("gva_share_"), hdd, cdd_fix, REN,
                                  -gva_share_GJ))
rownames(cortab.log) <- colnames(cortab.log) <- 
  c(dep_var_label, "Population", "Pop.share >=65",
    "Density", "GDP/cap", "GVA A", "GVA BE", "GVA F", "HDD", "CDD", "REN")

stargazer(cortab.log, column.sep.width = "0pt", digits = 2,
          title = "Correlation of logged model variables",
          out = "output/tables/summary_cor_log.tex")

# check for correlation between urban type and urbanisation
data$urbn_type_1 <- ifelse(data$urbn_type != "3", 1, 0)
polycor::hetcor(data %>% st_drop_geometry %>%  dplyr::select(density, urbn_type_1))

# ------------------------------------------------------------------------------
# include sector aggregates in the data panel

sector_detail <- "category_main"

data_m <- data_panel %>% 
  st_drop_geometry() %>% 
  dplyr::select(year, 
                nuts3_id,
                starts_with("edgar_"),
                -edgar_CO2total,
                -edgar_CO2f,
                -edgar_CO2o,
                -edgar_N2O, 
                -edgar_CH4
  ) %>% 
  tidyr::pivot_longer(cols = starts_with("edgar_"), #c(nuts3_id, cntr_code, year),
                      names_to = "indicator")

data_m <- data_m %>% 
  dplyr::mutate(ghg = get_ghg_name_from_indicator(indicator),
                sector = get_sector_name_from_indicator(indicator))

# now we join the sectors list to either aggregate by 
# sector name, category or even main category
data_m <- data_m %>% 
  dplyr::left_join(read.csv("input/edgar/edgar_sectors.csv", stringsAsFactors = FALSE)[,c("short", sector_detail)],
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
                       "edgar_CO2total", "edgar_CO2f", "edgar_CO2o",
                       "edgar_CH4",
                       "edgar_N2O",
                       dep_variables_agg)

# ------------------------------------------------------------------------------

summary(data)
saveRDS(data, "input/data.rds")
saveRDS(data_panel, "input/data_panel.rds")
