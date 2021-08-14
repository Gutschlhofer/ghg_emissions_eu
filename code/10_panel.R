# setup ------------------------------------------------------------------------
year_filter <- 2002:2018

# get data ---------------------------------------------------------------------
data_panel <- readRDS("input/data_panel.rds")
data_panel <- data_panel %>% 
  dplyr::filter(year %in% year_filter)

summary(data_panel)

gg_miss_var(data_panel, show_pct = TRUE)
# notes:
# gva:share has much better coverage than emp_share

# Visualise panel --------------------------------------------------------------
# ensure that all plots have the same theme
theme_set(theme_minimal())

# save plots under
plot_path <- "output/plots/"
plot_template <- paste0("raw_%s",".png")

plot_stack <- function(data, sector_detail = c("sector_name", "category", "category_main")) {
  
  sectors_to_show <- 5
  data_m <- data
  
  if("sf" %in% class(data)){
    data_m <- data_m %>% 
      st_drop_geometry()
  }
  
  data_m <- data_m %>% 
    dplyr::select(year, 
                  starts_with("edgar_"),
                  -starts_with("edgar_GHG"),
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
    dplyr::rename(sector = all_of(sector_detail)) %>% 
    dplyr::filter(!is.na(sector))
  
  # group all transport sectors into one
  data_m <- data_m %>% 
    dplyr::mutate(sector = ifelse(grepl("TNR", short), ifelse(sector_detail != "category_main", "Transport", "Energy - Transport"), sector)) %>% 
    dplyr::mutate(sector = ifelse(grepl("TRO", short), ifelse(sector_detail != "category_main", "Transport", "Energy - Transport"), sector)) %>% 
    dplyr::mutate(sector = ifelse(sector == "Energy", "Energy - Industry", sector)) %>% 
    dplyr::mutate(sector = ifelse(sector == "Fuel combustion activities", "Fuel combustion activities - Industry", sector))
  
  data_m <- data_m %>% dplyr::select(sector, ghg, year, value) %>% 
    dplyr::filter(!is.na(value))
  
  # get most important sectors
  data_s <- data_m %>% 
    dplyr::group_by(sector) %>% 
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(desc(value)) %>% 
    dplyr::slice_head(n = sectors_to_show)
  
  data_m <- data_m %>% 
    dplyr::mutate(sector = ifelse(sector %in% data_s$sector, sector, "n.e.c.")) %>% 
    dplyr::group_by(sector, ghg, year) %>% 
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(sector = factor(sector, levels = c(unique(data_s$sector), "n.e.c.")),
                  ghg = as.factor(ghg),
                  year = as.numeric(year))
  
  plot <- data_m %>% 
    dplyr::filter(value != 0) %>% 
    dplyr::group_by(sector, year) %>% 
    dplyr::summarise(value = sum(value)) %>%   
    dplyr::mutate(value = value / 1e9) %>% # transform to billion tonnes co2 equiv
    dplyr::rename(Sector = sector) %>% 
    ggplot(aes(x=year, y=value, fill=Sector)) +
    geom_area() +
    scale_fill_viridis_d() +
    ylab(bquote('billion tonnes ' ~CO[2]* '-eq/year')) +
    xlab("")
  
  if(s) ggsave(plot = plot, device = NULL, path = plot_path, filename = sprintf(plot_template, paste0("sector_panel_stack_", sector_detail)), width = ifelse(sector_detail == "category", 12, 8), height = 5)
}
#   
#         plot_stack(data_edgar, "sector_name")
#         plot_stack(data_edgar, "category")
#         plot_stack(data_edgar, "category_main")

plot_stack(data_panel, "sector_name")
plot_stack(data_panel, "category")
plot_stack(data_panel, "category_main")


missing_gdp <- data_panel %>% 
  dplyr::filter(is.na(gdp))

# ggplot(data = missing_gdp %>% 
#          dplyr::group_by(nuts3_id) %>% 
#          dplyr::summarise(count = length(unique(year)))) + 
#   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
#                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
#                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
#   geom_sf(aes(fill = count), color = "white", size=0.01) +
#   scale_fill_viridis_c(option = "magma", direction = -1) +  
#   theme(legend.title = element_blank()) +
#   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1) + 
#   theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))

data_panel <- data_panel %>% dplyr::filter(!cntr_code %in% 
                                             (missing_gdp %>% pull(cntr_code) %>% unique()))
# perform some calculations
# data_panel <- data_panel %>% 
#   dplyr::mutate(
#     density = (pop/area)/1000, # pop per m2, density in 1000people/km2
#     heating_or_cooling = hdd + cdd,
#     cdd_fix = cdd+1, # move our scale by 1 to be able to log it
#     cdd_log = ifelse(cdd == 0, 0, log(cdd))
#   )

# # exclude so we have gdp, population and GVA share BE
# exclude <- c("NO","CH", "TR", "RS", "IE", "UK", "LI", "MT")
# data_panel <- data_panel[!data_panel$cntr_code%in%exclude,]
# # exclude so we have CDD,HDD
# exclude <- c("AL","MK","ME")
# data_panel <- data_panel[!data_panel$cntr_code%in%exclude,]
# 
# # test <- data_panel %>% filter(gva_share_F %>% is.na) # it's Sweden
# data_panel <- data_panel %>% dplyr::filter(cntr_code != "SE")
# 
# # this filters out regions in france that also don't have GDP
# data_panel <- data_panel %>% dplyr::filter(!is.na(gva_share_BE))
# 
# summary(data_panel)

not_all_years <- data_panel %>%
  st_drop_geometry %>% 
  dplyr::group_by(nuts3_id) %>% 
  dplyr::summarise(n_row = length(unique(year))) %>% 
  dplyr::arrange(n_row) %>% 
  dplyr::filter(n_row != max(n_row)) %>% 
  dplyr::pull(nuts3_id)

data_panel <- data_panel %>% 
  dplyr::filter(!(nuts3_id %in% not_all_years))

data_one_year <- data_panel %>% dplyr::filter(year == year_single)

# create W matrix for 
lw_queen <- poly2nb(data_one_year, queen = TRUE) %>% 
  nb2listw()
# inverse distance
data_coords <- st_coordinates(st_centroid(data_one_year$geometry))
k <- round(0.163404391231319 * nrow(data)) # use amount of neighbours determined by gwr_sel
lw_knn <- knearneigh(data_coords, k=k) %>% 
  knn2nb()

### inverse distance (based on knn)
dlist <- nbdists(lw_knn, data_coords, longlat = TRUE)
idlist <- lapply(dlist, function(x) 1/x)
lw_inversedist <- nb2listw(lw_knn, glist=idlist, style="W")
m <- listw2mat(lw_inversedist)
# inverse distance all
lw_d <- dnearneigh(data_coords, 0, Inf, longlat = TRUE)
dlist <- nbdists(lw_d, data_coords, longlat = TRUE)
idlist <- lapply(dlist, function(x) 1/x)
lw_inversedist_all <- nb2listw(lw_d, glist=idlist, style="W"); rm(lw_d)
# m_all <- listw2mat(lw_inversedist_all)

lw_spatial <- lw_queen
lw_spatial <- lw_inversedist_all
lw_spatial <- lw_inversedist

data_panel$year <- as.factor(data_panel$year)
data_panel$nuts3_id <- as.factor(data_panel$nuts3_id)

data_panel <- data_panel %>% filter(as.numeric(as.character(year)) > 2004)

gg_miss_var(data_panel, show_pct = TRUE)
summary(data_panel)

data_panel <- data_panel %>% 
  dplyr::mutate(log_gdppc = log(gdppc) - mean(log(gdppc)),
                gva_share_BE = ifelse(gva_share_BE < 0.0001, 0.0001, gva_share_BE)) # there is a negative value!

# Base model -------------------------------------------------------------------

dep_vars <- ghg_aggregate_over_sector
dep_vars <- dep_variables

spatial_coefficients <- NULL

# for all variables on the current architecture, it takes approx 35 minutes
for(dep_var in dep_vars){

print(dep_var)
  
try({
  
# dep_var <- "edgar"
dep_variable <- sprintf("log(%s)", dep_var) #"log(edgar)"
# dep_variable <- "log(edgar_co2)"

lw_spatial <- lw_queen
lw_spatial <- lw_inversedist

# 1. Model with everything
base_variables <- c(
  "log(pop)",
  "log(pop_share_Y15_64)",
  "log(pop_share_Y_GE65)",
  "log(density)",
  "log_gdppc",
  "I(log_gdppc^2)",
  "log(gva_share_A)",
  "log(gva_share_BE)",
  "log(gva_share_F)",
  "log(gva_share_GJ)",
  "log(hdd)",
  "log(cdd_fix)",
  "log(REN)",
  "urbn_type",
  "coast_type" #,
  # "mount_type"
)

model_base_ev <- as.formula(paste(dep_variable, "~", paste(c(base_variables, paste0("ev", 1:n_ev)), collapse= "+")))

model <- as.formula(paste(dep_variable, "~", paste(c(base_variables), collapse= "+")))
test1 <- lm(model, data)

model <- model_base_ev

test <- lm(model_base_ev, data_ev)
summary(test)

moran.test(test1$residuals, lw_queen)
moran.test(test$residuals, lw_queen)

model_base_ev <- as.formula(paste(dep_variable, "~", paste(c(base_variables, "fitted(ev_filter)"), collapse= "+")))
test3 <- lm(model_base_ev, data)
moran.test(test3$residuals, lw_queen)


run_gwr(data_ev, method = "bisq", model = model_base_ev, file_name_add = "_ev", adapt = .2)



model_base <- as.formula(paste(dep_variable, "~", paste(base_variables, collapse= "+")))

# summary(data_panel %>% dplyr::select(-starts_with("edgar_")))

# panel_sac_random <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
#                                ,model = "random"
#                                # effect = "twoway"
#                                ,lag = TRUE
#                                # ,spatial.error = "kkp"
# )
# saveRDS(panel_sac_random, sprintf("output/regressions/panel_sac_random_%s.rds", dep_var))
# summary(panel_sac_random)
file_name <- sprintf("output/regressions/panel_sac_fixed_%s.rds", dep_var)
if(file.exists(file_name)) {
  panel_sac_fixed <- readRDS(file_name)
} else {
  panel_sac_fixed <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                                # ,model = "random"
                                # effect = "twoway"
                                ,lag = TRUE
                                # ,spatial.error = "kkp"
  )
  saveRDS(panel_sac_fixed, file_name)
}
# summary(panel_sac_fixed) %>% print

row <- data.frame(
  dep_var = c(dep_variable),
  type = "SAC",
  # tricky thing here is that lambda and rho are swapped
  rho_autoreg = panel_sac_fixed$coefficients["lambda"],
  lambda_error = panel_sac_fixed$coefficients["rho"]
)

if(is.null(spatial_coefficients)) {
  spatial_coefficients <- row
} else {
  spatial_coefficients <- spatial_coefficients %>% rbind(row)
}

# if FE and not RE, then KKP = Baltagi
file_name <- sprintf("output/regressions/panel_sem_%s.rds", dep_var)
if(file.exists(file_name)) {
  panel_sem <- readRDS(file_name)
} else {
  panel_sem <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                          ,lag = FALSE 
                          # ,spatial.error = "kkp"
  )
  saveRDS(panel_sem, file_name)
}
# summary(panel_sem) %>% print

spatial_coefficients <- spatial_coefficients %>% add_row(
  dep_var = c(dep_variable),
  type = "SEM",
  # tricky thing here is that lambda and rho are swapped
  rho_autoreg = NA,
  lambda_error = panel_sem$coefficients["rho"]
)

})
}

rownames(spatial_coefficients) <- NULL
saveRDS(spatial_coefficients, "output/regressions/spatial_coefficients.rds")
summary(spatial_coefficients$rho_autoreg)
summary(spatial_coefficients$lambda_error)

spatial_coefficients %>% dplyr::filter(grepl("GHG", dep_var)) %>% pull(rho_autoreg) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.1014  0.2614  0.3553  0.3820  0.4981  0.6229      15 
spatial_coefficients %>% dplyr::filter(grepl("CH4", dep_var)) %>% pull(rho_autoreg) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.1691  0.3317  0.3830  0.4149  0.5427  0.6380      11 
spatial_coefficients %>% dplyr::filter(grepl("CO2", dep_var)) %>% pull(rho_autoreg) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.1719  0.2226  0.3110  0.2890  0.3448  0.4424       9 
spatial_coefficients %>% dplyr::filter(grepl("N2O", dep_var)) %>% pull(rho_autoreg) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.07766 0.22457 0.33645 0.33429 0.39533 0.72381      11 

spatial_coefficients %>% dplyr::filter(grepl("GHG", dep_var)) %>% pull(lambda_error) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8735  0.9436  0.9691  0.9579  0.9758  0.9864 
spatial_coefficients %>% dplyr::filter(grepl("CH4", dep_var)) %>% pull(lambda_error) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8714  0.9575  0.9705  0.9616  0.9780  0.9864 
spatial_coefficients %>% dplyr::filter(grepl("CO2", dep_var)) %>% pull(lambda_error) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.9211  0.9611  0.9699  0.9626  0.9728  0.9864 
spatial_coefficients %>% dplyr::filter(grepl("N2O", dep_var)) %>% pull(lambda_error) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.9314  0.9624  0.9758  0.9706  0.9819  0.9892 

spatial_coefficients <- spatial_coefficients %>% 
  dplyr::mutate(
    dep_var = gsub("log(", "", dep_var, fixed = TRUE),
    dep_var = gsub(")", "", dep_var, fixed = TRUE),
    sector = get_sector_name_from_indicator(dep_var),
    sector = ifelse(sector %in% c("", "short"), "total", sector))

for(i in unique(spatial_coefficients$sector)) {
  print(i)
  spatial_coefficients %>% dplyr::filter(grepl(i, sector)) %>% pull(rho_autoreg) %>% length %>% print
  spatial_coefficients %>% dplyr::filter(grepl(i, sector)) %>% pull(rho_autoreg) %>% summary %>% print
  spatial_coefficients %>% dplyr::filter(grepl(i, sector)) %>% pull(lambda_error) %>% summary %>% print
}

fs <- list.files("output/regressions", pattern = "*.rds", full.names = T)
fs <- fs[grep("panel_sem_", fs)]
fs <- fs[!grepl("co2.rds", fs)]
fs <- fs[!grepl("co2_short.rds", fs)]

reg1 <- readRDS(fs[1])
reg2 <- readRDS(fs[2])
reg3 <- readRDS(fs[3])
reg4 <- readRDS(fs[4])

# stargazer::stargazer(reg1, reg2, reg3, reg4, type = "text")
# % Error: Unrecognized object type.

# they need to be manually inserted

