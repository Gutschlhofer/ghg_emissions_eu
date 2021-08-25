# # uncomment this to run as a job
# setwd("..")
# source("code/00_libraries_functions.R", local = TRUE)

# setup ------------------------------------------------------------------------
year_filter <- 2002:2018
year_single <- 2018

# get data ---------------------------------------------------------------------
data_panel <- readRDS("input/data_panel.rds")
data_panel <- data_panel %>% 
  dplyr::filter(year %in% year_filter)

summary(data_panel)

gg_miss_var(data_panel, show_pct = TRUE)
# notes:
# gva_share has much better coverage than emp_share

# Visualise panel --------------------------------------------------------------
# ensure that all plots have the same theme
theme_set(theme_minimal())

# save plots under
plot_path <- "output/plots/"
plot_template <- paste0("raw_%s",".png")
s <- TRUE

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

# France is missing GDP/cap values until 2014
source("./code/02b_impute_gdppc.R", local = TRUE)
# Some NUTS3 regions are missing their population
source("./code/02a_impute_population.R", local = TRUE)

# impute GVA shares for 2007-2014 for France 
# because we'd need to discard all observations from France otherwise
# since GVA shares don't change all that much that shouldn't change a lot
data_panel <- data_panel %>% 
  dplyr::group_by(nuts3_id) %>% 
  # first_year is the same for all gva_shares
  dplyr::mutate(first_year = min(year[!is.na(gva_share_GJ)],na.rm=T)) %>% 
  dplyr::mutate(
    gva_share_A = ifelse(is.na(gva_share_A), gva_share_A[year == first_year], gva_share_A),
    gva_share_BE = ifelse(is.na(gva_share_BE), gva_share_BE[year == first_year], gva_share_BE),
    gva_share_F = ifelse(is.na(gva_share_F), gva_share_F[year == first_year], gva_share_F),
    gva_share_GJ = ifelse(is.na(gva_share_GJ), gva_share_GJ[year == first_year], gva_share_GJ)
  ) %>% 
  dplyr::ungroup()

# if any nuts3 region would not have values for all years, we would filter it now
# but there are none
not_all_years <- data_panel %>%
  st_drop_geometry %>% 
  dplyr::group_by(nuts3_id) %>% 
  dplyr::summarise(n_row = length(unique(year))) %>% 
  dplyr::arrange(n_row) %>% 
  dplyr::filter(n_row != max(n_row)) %>% 
  dplyr::pull(nuts3_id)
data_panel <- data_panel %>% 
  dplyr::filter(!(nuts3_id %in% not_all_years))

# create data-variable to create W matrices
data_one_year <- data_panel %>% dplyr::filter(year == year_single)

get_lw_inversedist <- function(data) {
  data_one_year <- data %>% dplyr::filter(year == year_single)
  
  # # create W matrix for 
  # lw_queen <- poly2nb(data_one_year, queen = TRUE) %>% 
  #   nb2listw()
  # inverse distance
  data_coords <- st_coordinates(st_centroid(data_one_year$geometry))
  k <- round(0.1 * nrow(data_one_year)) # use 10% of the data as neighbours (109)
  lw_knn <- knearneigh(data_coords, k=k) %>% 
    knn2nb()
  
  ### inverse distance (based on knn)
  dlist <- nbdists(lw_knn, data_coords, longlat = TRUE)
  idlist <- lapply(dlist, function(x) 1/x)
  lw_inversedist <- nb2listw(lw_knn, glist=idlist, style="W")
  
  return(lw_inversedist)
}

# create W matrix for 
lw_queen <- poly2nb(data_one_year, queen = TRUE) %>% 
  nb2listw()
# inverse distance
data_coords <- st_coordinates(st_centroid(data_one_year$geometry))
k <- round(0.1 * nrow(data_one_year)) # use 10% of the data as neighbours (109)
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

gg_miss_var(data_panel, show_pct = TRUE)
summary(data_panel)

data_panel <- data_panel %>% 
  dplyr::mutate(log_gdppc = log(gdppc) - mean(log(gdppc)),
                gva_share_BE = ifelse(gva_share_BE < 0.0001, 0.0001, gva_share_BE)) # there is a negative value!

saveRDS(data_panel, "input/data_panel_nomissing.rds")
# data_panel <- readRDS("input/data_panel_nomissing.rds")

west_europe <- c("AT", "DE", "BE", "ES", "FI", "EL", "DK", "FR", "NL", "IT", "LU", "PT")

data_panel <- data_panel %>% 
  dplyr::mutate(
    is_west = ifelse(cntr_code %in% west_europe, 1, 0)
  )

lw_spatial_west <- get_lw_inversedist(data_panel %>% dplyr::filter(is_west == 1))
lw_spatial_east <- get_lw_inversedist(data_panel %>% dplyr::filter(is_west == 0))

# dep_vars <- dep_variables
dep_vars <- dep_variables_sel
spatial_coefficients <- NULL
spatial_coefficients_west <- NULL
spatial_coefficients_east <- NULL
is_print_summaries <- TRUE

# capture.output() is used to store the summary output for easier readability
capture.output(
# for all variables on the current architecture, it takes approx 35 minutes
for(dep_var in dep_vars){
  
  print("--------------------------------------------------")
  print(dep_var)
  
  try({
    
    # CO2s for TRN_Ship is NA for 2007
    if(any(is.na(data_panel %>% st_drop_geometry %>% pull(dep_var)))) {
      print("OMITTED!")
      print(dep_var)
      
      omitted <- data_panel[is.na(data_panel %>% st_drop_geometry %>% pull(dep_var)),]
      
      data_panel <- data_panel[!is.na(data_panel %>% st_drop_geometry %>% pull(dep_var)),]
      
      output <- sprintf("%s: %.0f rows omitted, countries: %s", dep_var, nrow(omitted), paste(omitted$cntr_code %>% unique, collapse = ","))
      
      cat(output, file = sprintf("./output/tables/panel_%s_omitted.txt", dep_var), sep = "\n")
    }
    
    # issue: a few regions have 0-values in their GHG emissions for some sectors and GHGs
    # fix as done in 06_OLS as well: add 1kg CO2 equiv to all observations
    if(any(data_panel %>% st_drop_geometry() %>% pull(dep_var) == 0)) {
      # separate the column we want to change (otherwise selection without geometry is hard)
      temp <- data_panel %>% st_drop_geometry %>% dplyr::select(all_of(dep_var))
      data_panel <- data_panel %>% dplyr::select(-all_of(dep_var))
      
      # add 1kg CO2 equiv to all observations
      temp[,dep_var] <- temp[,dep_var]+0.001
      # TODO: robustness check with Inverse hyperbolic sine (IHS) transformation
      
      data_panel <- data_panel %>% cbind(temp)
    }

    # dep_var <- "edgar"
    dep_variable <- sprintf("log(%s)", dep_var) # "log(edgar)"
    # dep_variable <- "log(edgar_co2)"
    
    # 1. Model with everything
    base_variables <- c(
      "log(pop)",
      # "log(pop_share_Y15_64)",
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

    # model <- as.formula(paste(dep_variable, "~", paste(c(base_variables), collapse= "+")))
    # model_base_ev <- as.formula(paste(dep_variable, "~", paste(c(base_variables, paste0("ev", 1:n_ev)), collapse= "+")))
    # 
    # test1 <- lm(model, data)
    # 
    # model <- model_base_ev
    # 
    # test <- lm(model_base_ev, data_ev)
    # summary(test)
    # 
    # moran.test(test1$residuals, lw_queen)
    # moran.test(test$residuals, lw_queen)
    # 
    # model_base_ev <- as.formula(paste(dep_variable, "~", paste(c(base_variables, "fitted(ev_filter)"), collapse= "+")))
    # test3 <- lm(model_base_ev, data)
    # moran.test(test3$residuals, lw_queen)
    # 
    # run_gwr(data_ev, method = "bisq", model = model_base_ev, file_name_add = "_ev", adapt = .2)
    
    model_base <- as.formula(paste(dep_variable, "~", paste(base_variables, collapse= "+")))
    model_base_cntr <- as.formula(paste(dep_variable, "~", paste(c(base_variables, "cntr_code"), collapse= "+")))
    
    model_eastwest <- as.formula(paste(dep_variable, "~", paste(c(base_variables, "is_west"), collapse= "+")))
    
    # summary(data_panel %>% dplyr::select(-starts_with("edgar_")))
    
    # panel_sac_random <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
    #                                ,model = "random"
    #                                # effect = "twoway"
    #                                ,lag = TRUE
    #                                # ,spatial.error = "kkp"
    # )
    # saveRDS(panel_sac_random, sprintf("output/regressions/panel_sac_random_%s.rds", dep_var))
    # summary(panel_sac_random)
    
    # SAC ----------------------------------------------------------------------
    # Model Base #
    
    file_name <- sprintf("output/regressions/panel_sac_fixed_newk_%s.rds", dep_var)
    if(file.exists(file_name)) {
      panel_sac <- readRDS(file_name)
    } else {
      panel_sac <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                              # ,model = "random"
                              ,effect = "twoways" # individual and  time
                              ,lag = TRUE
                              # ,spatial.error = "kkp"
      )
      # panel_sac <- splm::spml(model_base_cntr, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
      #                         # ,model = "random"
      #                         ,effect = "time"
      #                         ,lag = TRUE
      #                         # ,spatial.error = "kkp"
      # )
      saveRDS(panel_sac, file_name)
    }
    if(is_print_summaries) summary(panel_sac) %>% print
    
    row <- data.frame(
      dep_var = c(dep_variable),
      type = "SAC",
      # tricky thing here is that lambda and rho are swapped
      rho_autoreg = panel_sac$coefficients["lambda"],
      rho_p = summary(panel_sac)$CoefTable[1, 4],
      lambda_error = panel_sac$coefficients["rho"],
      lambda_p = summary(panel_sac)$CoefTable[2, 4],
      is_west = NA,
      is_west_p = NA
    )
    if(is.null(spatial_coefficients)) {
      spatial_coefficients <- row
    } else {
      spatial_coefficients <- spatial_coefficients %>% rbind(row)
    }

    # Model Base for west only
    file_name <- sprintf("output/regressions/panel_sac_fixed_westonly_%s.rds", dep_var)
    if(file.exists(file_name)) {
      panel_sac <- readRDS(file_name)
    } else {
      panel_sac <- splm::spml(model_base
                              ,data_panel %>% dplyr::filter(is_west == 1)
                              ,index = c("nuts3_id", "year")
                              , listw = lw_spatial_west
                              # ,model = "random"
                              ,effect = "twoways" # individual and  time
                              ,lag = TRUE
                              # ,spatial.error = "kkp"
      )
      saveRDS(panel_sac, file_name)
    }
    if(is_print_summaries) summary(panel_sac) %>% print
    row <- data.frame(
      dep_var = c(dep_variable),
      type = "SAC",
      # tricky thing here is that lambda and rho are swapped
      rho_autoreg = panel_sac$coefficients["lambda"],
      rho_p = summary(panel_sac)$CoefTable[1, 4],
      lambda_error = panel_sac$coefficients["rho"],
      lambda_p = summary(panel_sac)$CoefTable[2, 4],
      is_west = NA,
      is_west_p = NA
    )
    if(is.null(spatial_coefficients_west)) {
      spatial_coefficients_west <- row
    } else {
      spatial_coefficients_west <- spatial_coefficients_west %>% rbind(row)
    }

    # Model Base for east only
    file_name <- sprintf("output/regressions/panel_sac_fixed_eastonly_%s.rds", dep_var)
    if(file.exists(file_name)) {
      panel_sac <- readRDS(file_name)
    } else {
      panel_sac <- splm::spml(model_base
                              ,data_panel %>% dplyr::filter(is_west == 0)
                              ,index = c("nuts3_id", "year")
                              , listw = lw_spatial_east
                              # ,model = "random"
                              ,effect = "twoways" # individual and  time
                              ,lag = TRUE
                              # ,spatial.error = "kkp"
      )
      saveRDS(panel_sac, file_name)
    }
    if(is_print_summaries) summary(panel_sac) %>% print
    row <- data.frame(
      dep_var = c(dep_variable),
      type = "SAC",
      # tricky thing here is that lambda and rho are swapped
      rho_autoreg = panel_sac$coefficients["lambda"],
      rho_p = summary(panel_sac)$CoefTable[1, 4],
      lambda_error = panel_sac$coefficients["rho"],
      lambda_p = summary(panel_sac)$CoefTable[2, 4],
      is_west = NA,
      is_west_p = NA
    )
    if(is.null(spatial_coefficients_east)) {
      spatial_coefficients_east <- row
    } else {
      spatial_coefficients_east <- spatial_coefficients_east %>% rbind(row)
    }

    # Model Base + West Dummy #
    file_name <- sprintf("output/regressions/panel_sac_fixed_west_%s.rds", dep_var)
    if(file.exists(file_name)) {
      panel_sac <- readRDS(file_name)
    } else {
      panel_sac <- splm::spml(model_eastwest, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                                    # ,model = "random"
                              ,effect = "twoways" # individual and  time
                                    ,lag = TRUE
                                    # ,spatial.error = "kkp"
      )
      saveRDS(panel_sac, file_name)
    }
    if(is_print_summaries) summary(panel_sac) %>% print
    spatial_coefficients <- spatial_coefficients %>% add_row(
      dep_var = c(dep_variable),
      type = "SAC_WestDummy",
      # tricky thing here is that lambda and rho are swapped
      rho_autoreg = panel_sac$coefficients["lambda"],
      rho_p = summary(panel_sac)$CoefTable[1, 4],
      lambda_error = panel_sac$coefficients["rho"],
      lambda_p = summary(panel_sac)$CoefTable[2, 4],
      is_west = panel_sac$coefficients["is_west"],
      is_west_p = summary(panel_sac)$CoefTable[nrow(summary(panel_sac)$CoefTable), 4]
    )

    # SEM ----------------------------------------------------------------------
    # Model Base

    # if FE and not RE, then KKP = Baltagi
    file_name <- sprintf("output/regressions/panel_sem_%s.rds", dep_var)
    if(file.exists(file_name)) {
      panel_sem <- readRDS(file_name)
    } else {
      panel_sem <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                              ,lag = FALSE
                              ,effect = "twoways" # individual and  time
                              # ,spatial.error = "kkp"
      )
      saveRDS(panel_sem, file_name)
    }
    if(is_print_summaries) summary(panel_sem) %>% print

    spatial_coefficients <- spatial_coefficients %>% add_row(
      dep_var = c(dep_variable),
      type = "SEM",
      # tricky thing here is that lambda and rho are swapped
      rho_autoreg = NA,
      rho_p = NA,
      lambda_error = panel_sem$coefficients["rho"],
      lambda_p = summary(panel_sem)$CoefTable[1, 4],
      is_west = NA,
      is_west_p = NA
    )

    # Model Base + West Dummy #
    # if FE and not RE, then KKP = Baltagi
    file_name <- sprintf("output/regressions/panel_sem_west_%s.rds", dep_var)
    if(file.exists(file_name)) {
      panel_sem <- readRDS(file_name)
    } else {
      panel_sem <- splm::spml(model_eastwest, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                              ,lag = FALSE
                              # ,spatial.error = "kkp"
      )
      saveRDS(panel_sem, file_name)
    }
    if(is_print_summaries) summary(panel_sem) %>% print

    spatial_coefficients <- spatial_coefficients %>% add_row(
      dep_var = c(dep_variable),
      type = "SEM_WestDummy",
      # tricky thing here is that lambda and rho are swapped
      rho_autoreg = NA,
      rho_p = NA,
      lambda_error = panel_sem$coefficients["rho"],
      lambda_p = summary(panel_sem)$CoefTable[1, 4],
      is_west = panel_sem$coefficients["is_west"],
      is_west_p = summary(panel_sem)$CoefTable[nrow(summary(panel_sem)$CoefTable), 4]
    )

  })
}
, file = "output/regressions/spatial_output.txt")

rownames(spatial_coefficients) <- NULL
rownames(spatial_coefficients_west) <- NULL
rownames(spatial_coefficients_east) <- NULL

saveRDS(spatial_coefficients, "output/regressions/spatial_coefficients.rds")
saveRDS(spatial_coefficients_west, "output/regressions/spatial_coefficients_west.rds")
saveRDS(spatial_coefficients_east, "output/regressions/spatial_coefficients_east.rds")

# summary(spatial_coefficients$rho_autoreg)
summary(spatial_coefficients$rho_autoreg[spatial_coefficients$rho_p < 0.05])
# summary(spatial_coefficients$lambda_error)
summary(spatial_coefficients$lambda_error[spatial_coefficients$lambda_p < 0.05])
summary(spatial_coefficients$is_west[spatial_coefficients$is_west_p < 0.05])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# -0.2618  0.1262  0.2808  0.2839  0.4558  1.3597     140

summary(spatial_coefficients_west$rho_autoreg[spatial_coefficients_west$rho_p < 0.05])
summary(spatial_coefficients_east$rho_autoreg[spatial_coefficients_east$rho_p < 0.05])
summary(spatial_coefficients_west$lambda_error[spatial_coefficients_west$lambda_p < 0.05])
summary(spatial_coefficients_east$lambda_error[spatial_coefficients_east$lambda_p < 0.05])


spatial_coefficients %>% dplyr::filter(grepl("GHG", dep_var)) %>% dplyr::filter(rho_p < 0.05) %>% pull(rho_autoreg) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.1014  0.2614  0.3553  0.3820  0.4981  0.6229      15
spatial_coefficients %>% dplyr::filter(grepl("CH4", dep_var)) %>% dplyr::filter(rho_p < 0.05) %>% pull(rho_autoreg) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.1691  0.3317  0.3830  0.4149  0.5427  0.6380      11
spatial_coefficients %>% dplyr::filter(grepl("CO2", dep_var)) %>% dplyr::filter(rho_p < 0.05) %>% pull(rho_autoreg) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.1719  0.2226  0.3110  0.2890  0.3448  0.4424       9
spatial_coefficients %>% dplyr::filter(grepl("N2O", dep_var)) %>% dplyr::filter(rho_p < 0.05) %>% pull(rho_autoreg) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.07766 0.22457 0.33645 0.33429 0.39533 0.72381      11

spatial_coefficients %>% dplyr::filter(grepl("GHG", dep_var)) %>% dplyr::filter(lambda_p < 0.05) %>% pull(lambda_error) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.8735  0.9436  0.9691  0.9579  0.9758  0.9864
spatial_coefficients %>% dplyr::filter(grepl("CH4", dep_var)) %>% dplyr::filter(lambda_p < 0.05) %>% pull(lambda_error) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.8714  0.9575  0.9705  0.9616  0.9780  0.9864
spatial_coefficients %>% dplyr::filter(grepl("CO2", dep_var)) %>% dplyr::filter(lambda_p < 0.05) %>% pull(lambda_error) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.9211  0.9611  0.9699  0.9626  0.9728  0.9864
spatial_coefficients %>% dplyr::filter(grepl("N2O", dep_var)) %>% dplyr::filter(lambda_p < 0.05) %>% pull(lambda_error) %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.9314  0.9624  0.9758  0.9706  0.9819  0.9892

spatial_coefficients <- spatial_coefficients %>%
  dplyr::mutate(
    dep_var = gsub("log(", "", dep_var, fixed = TRUE),
    dep_var = gsub(")", "", dep_var, fixed = TRUE),
    sector = get_sector_name_from_indicator(dep_var),
    sector = ifelse(sector %in% c("", "short"), "total", sector),
    ghg = get_ghg_name_from_indicator(dep_var),
    ghg = ifelse(grepl("agg", ghg) & grepl("GHG_", dep_var), "GHG", ghg),
    ghg = ifelse(grepl("_CH4_", dep_var), "CH4", ghg),
    ghg = ifelse(grepl("_N2O_", dep_var), "N2O", ghg),
    ghg = ifelse(grepl("_CO2f_", dep_var), "CO2f", ghg),
    ghg = ifelse(grepl("_CO2o_", dep_var), "CO2o", ghg),
    ghg = toupper(ghg)
  )

# overview of rho and lambda by sector
for(i in unique(spatial_coefficients$sector)) {
  print("------------------------------------------------")
  print(i)
  print(spatial_coefficients %>% dplyr::filter(sector == !!i) %>% nrow())
  spatial_coefficients %>% dplyr::filter(sector == !!i) %>% dplyr::filter(rho_p < 0.05) %>% pull(rho_autoreg) %>% length %>% print
  spatial_coefficients %>% dplyr::filter(sector == !!i) %>% dplyr::filter(rho_p < 0.05) %>% pull(rho_autoreg) %>% summary %>% print
  spatial_coefficients %>% dplyr::filter(sector == !!i) %>% dplyr::filter(lambda_p < 0.05) %>% pull(lambda_error) %>% length %>% print
  spatial_coefficients %>% dplyr::filter(sector == !!i) %>% dplyr::filter(lambda_p < 0.05) %>% pull(lambda_error) %>% summary %>% print
}

# overview of rho and lambda by GHG
for(i in unique(spatial_coefficients$ghg)) {
  print("------------------------------------------------")
  print(i)
  spatial_coefficients %>% dplyr::filter(ghg == !!i) %>% dplyr::filter(rho_p < 0.05) %>% pull(rho_autoreg) %>% length %>% print
  spatial_coefficients %>% dplyr::filter(ghg == !!i) %>% dplyr::filter(rho_p < 0.05) %>% pull(rho_autoreg) %>% summary %>% print
  spatial_coefficients %>% dplyr::filter(ghg == !!i) %>% dplyr::filter(lambda_p < 0.05) %>% pull(lambda_error) %>% length %>% print
  spatial_coefficients %>% dplyr::filter(ghg == !!i) %>% dplyr::filter(lambda_p < 0.05) %>% pull(lambda_error) %>% summary %>% print
}

# very interesting
# seems like there are no huge differences between GHGs but between sectors
# in their spatial coefficients

# overview of West-Dummy by GHG
for(i in unique(spatial_coefficients$ghg)) {
  print("------------------------------------------------")
  print(i)
  print((spatial_coefficients %>% dplyr::filter(ghg == !!i) %>% nrow())/2)
  spatial_coefficients %>% dplyr::filter(ghg == !!i) %>% dplyr::filter(is_west_p < 0.05) %>% pull(is_west) %>% length %>% print
  spatial_coefficients %>% dplyr::filter(ghg == !!i) %>% dplyr::filter(is_west_p < 0.05) %>% pull(is_west) %>% summary %>% print
}


fs <- list.files("output/regressions", pattern = "*.rds", full.names = T)
fs <- fs[grepl("panel_sac_fixed_", fs)]
fs <- fs[!grepl("panel_sac_fixed_west", fs)]
fs <- fs[!grepl("CO2f.rds", fs)]
fs <- fs[!grepl("CO2o.rds", fs)]

reg <- readRDS("output/regressions/panel_sac_fixed_edgar.rds")
summary(reg)
reg <- readRDS("output/regressions/panel_sac_fixed_west_edgar.rds")
summary(reg)
reg <- readRDS("output/regressions/panel_sac_fixed_westonly_edgar.rds")
summary(reg)
reg <- readRDS("output/regressions/panel_sac_fixed_eastonly_edgar.rds")
summary(reg)

reg1 <- readRDS(fs[1])
reg2 <- readRDS(fs[2])
reg3 <- readRDS(fs[3])
reg4 <- readRDS(fs[4])

# stargazer::stargazer(reg1, reg2, reg3, reg4, type = "text")
# % Error: Unrecognized object type.

# they need to be manually inserted

# Run spatial models with merged urban and coast types -------------------------

# now new variables
data_panel <- data_panel %>%
  dplyr::mutate(
    urbn_type_2_3 = ifelse(urbn_type %in% c("2", "3"), "1", "0"),
    coast_type_2_3 = ifelse(coast_type %in% c("2", "3"), "1", "0"),
    urbn_type_2_3 = as.factor(urbn_type_2_3),
    coast_type_2_3 = as.factor(coast_type_2_3)
  )

dep_vars <- dep_variables
spatial_coefficients <- NULL
spatial_coefficients_west <- NULL
spatial_coefficients_east <- NULL
is_print_summaries <- TRUE

# capture.output() is used to store the summary output for easier readability
capture.output(
  # for all variables on the current architecture, it takes approx 35 minutes
  for(dep_var in dep_vars){

    print("--------------------------------------------------")
    print(dep_var)

    try({

      # issue: a few regions have 0-values in their GHG emissions for some sectors and GHGs
      # fix as done in 06_OLS as well: add 1kg CO2 equiv to all observations
      if(any(data_panel %>% st_drop_geometry() %>% pull(dep_var) == 0)) {
        # separate the column we want to change (otherwise selection without geometry is hard)
        temp <- data_panel %>% st_drop_geometry %>% dplyr::select(all_of(dep_var))
        data_panel <- data_panel %>% dplyr::select(-all_of(dep_var))

        # add 1kg CO2 equiv to all observations
        temp[,dep_var] <- temp[,dep_var]+0.001
        # TODO: robustness check with Inverse hyperbolic sine (IHS) transformation

        data_panel <- data_panel %>% cbind(temp)
      }

      # dep_var <- "edgar"
      dep_variable <- sprintf("log(%s)", dep_var) # "log(edgar)"
      # dep_variable <- "log(edgar_co2)"

      # lw_spatial <- lw_queen
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
        "log(hdd)",
        "log(cdd_fix)",
        "log(REN)",
        "urbn_type_2_3",
        "coast_type_2_3" #,
        # "mount_type"
      )
      model_base <- as.formula(paste(dep_variable, "~", paste(base_variables, collapse= "+")))

      model_eastwest <- as.formula(paste(dep_variable, "~", paste(c(base_variables, "is_west"), collapse= "+")))

      # SAC ----------------------------------------------------------------------
      # Model Base #

      file_name <- sprintf("output/regressions/panel_mod_sac_fixed_%s.rds", dep_var)
      if(file.exists(file_name)) {
        panel_sac <- readRDS(file_name)
      } else {
        panel_sac <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                                # ,model = "random"
                                # , effect = "twoway"
                                ,lag = TRUE
                                # ,spatial.error = "kkp"
        )
        # spdep::impacts(panel_sac, listw = lw_spatial, time = data_panel$year %>% levels %>% length)
        saveRDS(panel_sac, file_name)
      }
      if(is_print_summaries) summary(panel_sac) %>% print

      row <- data.frame(
        dep_var = c(dep_variable),
        type = "SAC",
        # tricky thing here is that lambda and rho are swapped
        rho_autoreg = panel_sac$coefficients["lambda"],
        rho_p = summary(panel_sac)$CoefTable[1, 4],
        lambda_error = panel_sac$coefficients["rho"],
        lambda_p = summary(panel_sac)$CoefTable[2, 4],
        is_west = NA,
        is_west_p = NA
      )
      if(is.null(spatial_coefficients)) {
        spatial_coefficients <- row
      } else {
        spatial_coefficients <- spatial_coefficients %>% rbind(row)
      }

      # Model Base for west only
      file_name <- sprintf("output/regressions/panel_mod_sac_fixed_westonly_%s.rds", dep_var)
      if(file.exists(file_name)) {
        panel_sac <- readRDS(file_name)
      } else {
        panel_sac <- splm::spml(model_base
                                ,data_panel %>% dplyr::filter(is_west == 1)
                                ,index = c("nuts3_id", "year")
                                , listw = lw_spatial_west
                                # ,model = "random"
                                # effect = "twoway"
                                ,lag = TRUE
                                # ,spatial.error = "kkp"
        )
        saveRDS(panel_sac, file_name)
      }
      if(is_print_summaries) summary(panel_sac) %>% print
      row <- data.frame(
        dep_var = c(dep_variable),
        type = "SAC",
        # tricky thing here is that lambda and rho are swapped
        rho_autoreg = panel_sac$coefficients["lambda"],
        rho_p = summary(panel_sac)$CoefTable[1, 4],
        lambda_error = panel_sac$coefficients["rho"],
        lambda_p = summary(panel_sac)$CoefTable[2, 4],
        is_west = NA,
        is_west_p = NA
      )
      if(is.null(spatial_coefficients_west)) {
        spatial_coefficients_west <- row
      } else {
        spatial_coefficients_west <- spatial_coefficients_west %>% rbind(row)
      }

      # Model Base for east only
      file_name <- sprintf("output/regressions/panel_mod_sac_fixed_eastonly_%s.rds", dep_var)
      if(file.exists(file_name)) {
        panel_sac <- readRDS(file_name)
      } else {
        panel_sac <- splm::spml(model_base
                                ,data_panel %>% dplyr::filter(is_west == 0)
                                ,index = c("nuts3_id", "year")
                                , listw = lw_spatial_east
                                # ,model = "random"
                                # effect = "twoway"
                                ,lag = TRUE
                                # ,spatial.error = "kkp"
        )
        saveRDS(panel_sac, file_name)
      }
      if(is_print_summaries) summary(panel_sac) %>% print
      row <- data.frame(
        dep_var = c(dep_variable),
        type = "SAC",
        # tricky thing here is that lambda and rho are swapped
        rho_autoreg = panel_sac$coefficients["lambda"],
        rho_p = summary(panel_sac)$CoefTable[1, 4],
        lambda_error = panel_sac$coefficients["rho"],
        lambda_p = summary(panel_sac)$CoefTable[2, 4],
        is_west = NA,
        is_west_p = NA
      )
      if(is.null(spatial_coefficients_east)) {
        spatial_coefficients_east <- row
      } else {
        spatial_coefficients_east <- spatial_coefficients_east %>% rbind(row)
      }

      # Model Base + West Dummy #
      file_name <- sprintf("output/regressions/panel_mod_sac_fixed_west_%s.rds", dep_var)
      if(file.exists(file_name)) {
        panel_sac <- readRDS(file_name)
      } else {
        panel_sac <- splm::spml(model_eastwest, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                                # ,model = "random"
                                # effect = "twoway"
                                ,lag = TRUE
                                # ,spatial.error = "kkp"
        )
        saveRDS(panel_sac, file_name)
      }
      if(is_print_summaries) summary(panel_sac) %>% print
      spatial_coefficients <- spatial_coefficients %>% add_row(
        dep_var = c(dep_variable),
        type = "SAC_WestDummy",
        # tricky thing here is that lambda and rho are swapped
        rho_autoreg = panel_sac$coefficients["lambda"],
        rho_p = summary(panel_sac)$CoefTable[1, 4],
        lambda_error = panel_sac$coefficients["rho"],
        lambda_p = summary(panel_sac)$CoefTable[2, 4],
        is_west = panel_sac$coefficients["is_west"],
        is_west_p = summary(panel_sac)$CoefTable[nrow(summary(panel_sac)$CoefTable), 4]
      )

      # SEM ----------------------------------------------------------------------
      # Model Base

      # if FE and not RE, then KKP = Baltagi
      file_name <- sprintf("output/regressions/panel_mod_sem_%s.rds", dep_var)
      if(file.exists(file_name)) {
        panel_sem <- readRDS(file_name)
      } else {
        panel_sem <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                                ,lag = FALSE
                                # ,spatial.error = "kkp"
        )
        saveRDS(panel_sem, file_name)
      }
      if(is_print_summaries) summary(panel_sem) %>% print

      spatial_coefficients <- spatial_coefficients %>% add_row(
        dep_var = c(dep_variable),
        type = "SEM",
        # tricky thing here is that lambda and rho are swapped
        rho_autoreg = NA,
        rho_p = NA,
        lambda_error = panel_sem$coefficients["rho"],
        lambda_p = summary(panel_sem)$CoefTable[1, 4],
        is_west = NA,
        is_west_p = NA
      )

      # Model Base + West Dummy #
      # if FE and not RE, then KKP = Baltagi
      file_name <- sprintf("output/regressions/panel_mod_sem_west_%s.rds", dep_var)
      if(file.exists(file_name)) {
        panel_sem <- readRDS(file_name)
      } else {
        panel_sem <- splm::spml(model_eastwest, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                                ,lag = FALSE
                                # ,spatial.error = "kkp"
        )
        saveRDS(panel_sem, file_name)
      }
      if(is_print_summaries) summary(panel_sem) %>% print

      spatial_coefficients <- spatial_coefficients %>% add_row(
        dep_var = c(dep_variable),
        type = "SEM_WestDummy",
        # tricky thing here is that lambda and rho are swapped
        rho_autoreg = NA,
        rho_p = NA,
        lambda_error = panel_sem$coefficients["rho"],
        lambda_p = summary(panel_sem)$CoefTable[1, 4],
        is_west = panel_sem$coefficients["is_west"],
        is_west_p = summary(panel_sem)$CoefTable[nrow(summary(panel_sem)$CoefTable), 4]
      )

    })
  }
  , file = "output/regressions/spatial_output_mod.txt")

rownames(spatial_coefficients) <- NULL
rownames(spatial_coefficients_west) <- NULL
rownames(spatial_coefficients_east) <- NULL

saveRDS(spatial_coefficients, "output/regressions/spatial_coefficients_mod.rds")
saveRDS(spatial_coefficients_west, "output/regressions/spatial_coefficients_mod_west.rds")
saveRDS(spatial_coefficients_east, "output/regressions/spatial_coefficients_mod_east.rds")
