# this function is a helper-function that creates a latex-table from the 
# summary tables I generate below
create_latex <- function(summary_table_wide) {
  for(row in 1:nrow(summary_table_wide)) {
    # check how many column triplets we have
    triplets <- ncol(summary_table_wide) / 3
    row_str <- ""
    
    for(i in 1:triplets) {
      first <- i * 3 - 2
      second <- i * 3 - 1
      third <- i * 3
      
      row_str <- paste(row_str, 
                       sprintf("%s%s (%.2f)",
                               summary_table_wide[row,first] %>% str_replace("-", "$-$"),
                               ifelse(summary_table_wide[row,second] %>% nchar > 0, paste0("$^{",summary_table_wide[row,second],"}$"), ""),
                               summary_table_wide[row,third]))
      
      row_str <- paste(row_str, ifelse(i == triplets, "\\", "&"))
    }
    
    print(row_str)
  }
}

### 0. copy paste until capture.output from 10_panel.R -------------------------

# setup ------------------------------------------------------------------------
year_filter <- 2002:2018
year_single <- 2018

# get data ---------------------------------------------------------------------
data_panel <- readRDS("input/data_panel.rds")
data_panel <- data_panel %>% 
  dplyr::filter(year %in% year_filter)

# summary(data_panel)

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

data_panel$year <- as.factor(data_panel$year)
data_panel$nuts3_id <- as.factor(data_panel$nuts3_id)

gg_miss_var(data_panel, show_pct = TRUE)
summary(data_panel)

data_panel <- data_panel %>% 
  dplyr::mutate(log_gdppc = log(gdppc) - mean(log(gdppc)),
                gva_share_BE = ifelse(gva_share_BE < 0.0001, 0.0001, gva_share_BE)) # there is a negative value!

saveRDS(data_panel, "input/data_panel_nomissing.rds")
# data_panel <- readRDS("input/data_panel_nomissing.rds")

# dep_vars <- dep_variables
dep_vars <- dep_variables_sel
spatial_coefficients <- NULL
spatial_coefficients_west <- NULL
spatial_coefficients_east <- NULL
is_print_summaries <- TRUE

lw_spatial <- lw_inversedist

coefs <- data.frame(
  estimate = c(""),
  std_error = c(""),
  t_value = c(""),
  p = c(""),
  variable = c(""),
  dep_var = c("")
)
coefs <- coefs[0,] # remove initialising row

impacts_total <- c()
impacts_colnames <- c()

# ------------------------------------------------------------------------------
# 1. Aggregating everything -> provide SARAR (+ impacts) of GHG, CO2f, CO2o, N2O, CH4

# 1. Model with everything
base_variables <- c(
  "log(pop)",
  # "log(pop_share_Y15_64)",
  "log(pop_share_Y_GE65)",
  "log(density)",
  "log_gdppc",
  # "I(log_gdppc^2)",
  "log(gva_share_A)",
  "log(gva_share_BE)",
  "log(gva_share_F)",
  # "log(gva_share_GJ)",
  "log(hdd)",
  "log(cdd_fix)",
  "log(REN)",
  "urbn_type",
  "coast_type" #,
  # "mount_type"
)

dep_variables2 <- c(
  "edgar",
  "edgar_CH4",
  "edgar_CO2f",
  "edgar_CO2o",
  "edgar_N2O"
)

summary_table <- data.frame(
  variable = "",
  coefficient = 0.00,
  significance = "***",
  std_error = 0.00,
  dep_var = ""
)
summary_table <- summary_table[0,]
summary_table_wide <- NULL
impacts_total <- c()
impacts_colnames <- c()

for(dep_var in dep_variables2) {
  dep_variable <- sprintf("log(%s)", dep_var)
  
  model_base <- as.formula(paste(dep_variable, "~", paste(base_variables, collapse= "+")))
  
  panel_sac <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                          ,effect = "twoways" # individual and  time
                          ,lag = TRUE
  )
  panel_sac %>% summary %>% print
  
  panel_summary <- summary(panel_sac)
  
  sign <- panel_summary$CoefTable[,"Pr(>|t|)"]
  
  # prepare table with summary coefficients
  ## estimate 2 digits signif*** (std.error 2 digits)
  summary_table <- summary_table %>% dplyr::add_row(
    variable = rownames(panel_summary$CoefTable),
    coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
    significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
    std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2),
    dep_var = dep_var
  )
  
  if(summary_table_wide %>% is.null || rownames(summary_table_wide) %>% is.null) {
    summary_table_wide <- data.frame(
      coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
      significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
      std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2)
    )
    rownames(summary_table_wide) <- rownames(panel_summary$CoefTable)
  } else {
    summary_table_wide <- summary_table_wide %>% cbind(
      data.frame(
        coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
        significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
        std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2)
      )
    )
  }
  
  impac <- spdep::impacts(panel_sac, listw = lw_spatial, time = length(unique(data_panel$year)))
  impac_total <- impac$res$total
  impacts_total <- cbind(impacts_total, impac_total)
  impacts_colnames <- c(impacts_colnames, dep_var)
}

colnames(impacts_total) <- impacts_colnames
rownames(impacts_total) <- colnames(impac$sres$total)
impacts_total %>% round(2)

create_latex(summary_table_wide)

# ------------------------------------------------------------------------------
# 2. Aggregating per GHG -> provide SARAR (+ impacts) of CO2f aggregated and per sector

dep_variables2 <- c(
  "edgar_CO2f",
  "edgar_agg_CO2f_Agricultureforestryandotherlanduse",
  "edgar_agg_CO2f_EnergyIndustry",
  "edgar_agg_CO2f_EnergyTransport",
  "edgar_agg_CO2f_Industrialprocessesandproductuse"
)
# dep_variables2 <- c(
#   "edgar_CO2o",
#   dep_variables[dep_variables %>% grepl("edgar_agg_CO2o", .)]
# )
# dep_variables2 <- dep_variables2[!grepl("Waste", dep_variables2)]
# 
# dep_variables2 <- c(
#   "edgar_CH4",
#   dep_variables[dep_variables %>% grepl("edgar_agg_CH4", .)]
# )
# dep_variables2 <- dep_variables2[!grepl("Other", dep_variables2)]
# dep_variables2 <- c(
#   "edgar_N2O",
#   dep_variables[dep_variables %>% grepl("edgar_agg_N2O", .)]
# )

dep_variables2 <- dep_variables2[!grepl("Other", dep_variables2)]

summary_table <- data.frame(
  variable = "",
  coefficient = 0.00,
  significance = "***",
  std_error = 0.00,
  dep_var = ""
)
summary_table <- summary_table[0,]
summary_table_wide <- NULL
impacts_total <- c()
impacts_colnames <- c()

for(dep_var in dep_variables2) {
  dep_variable <- sprintf("log(%s)", dep_var)
  
  model_base <- as.formula(paste(dep_variable, "~", paste(base_variables, collapse= "+")))
  
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
  
  panel_sac <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                          ,effect = "twoways" # individual and  time
                          ,lag = TRUE
  )
  panel_sac %>% summary %>% print
  
  panel_summary <- summary(panel_sac)
  
  sign <- panel_summary$CoefTable[,"Pr(>|t|)"]
  
  # prepare table with summary coefficients
  ## estimate 2 digits signif*** (std.error 2 digits)
  summary_table <- summary_table %>% dplyr::add_row(
    variable = rownames(panel_summary$CoefTable),
    coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
    significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
    std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2),
    dep_var = dep_var
  )
  
  if(summary_table_wide %>% is.null || rownames(summary_table_wide) %>% is.null) {
    summary_table_wide <- data.frame(
      coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
      significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
      std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2)
    )
    rownames(summary_table_wide) <- rownames(panel_summary$CoefTable)
  } else {
    summary_table_wide <- summary_table_wide %>% cbind(
      data.frame(
        coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
        significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
        std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2)
      )
    )
  }
  
  impac <- spdep::impacts(panel_sac, listw = lw_spatial, time = length(unique(data_panel$year)))
  impac_total <- impac$res$total
  impacts_total <- cbind(impacts_total, impac_total)
  impacts_colnames <- c(impacts_colnames, dep_var)
}

colnames(impacts_total) <- impacts_colnames
rownames(impacts_total) <- colnames(impac$sres$total)

create_latex(summary_table_wide)

impacts_total_co2f <- impacts_total
summary_table_wide_co2f <- summary_table_wide

# vars_by_ghg <- data.frame(
#   co2f = matrixStats::rowVars(impacts_total_co2f),
#   co2o = matrixStats::rowVars(impacts_total_co2o),
#   ch4 = matrixStats::rowVars(impacts_total_ch4),
#   n2o = matrixStats::rowVars(impacts_total_n2o)
# )

# ------------------------------------------------------------------------------
# 3. Aggregating per sector group -> provide SARAR (+ impacts) for one sector (EnergyIndustry) aggregated and per GHG

dep_variables2 <- c(
  "edgar_agg_GHG_EnergyIndustry",
  "edgar_agg_CH4_EnergyIndustry",
  "edgar_agg_CO2f_EnergyIndustry",
  "edgar_agg_CO2o_EnergyIndustry",
  "edgar_agg_N2O_EnergyIndustry"
)

# dep_variables2 <- c(
#   "edgar_agg_GHG_EnergyTransport",
#   "edgar_agg_CH4_EnergyTransport",
#   "edgar_agg_CO2f_EnergyTransport",
#   "edgar_agg_CO2o_EnergyTransport",
#   "edgar_agg_N2O_EnergyTransport"
# )
# 
# dep_variables2 <- c(
#   "edgar_agg_GHG_Industrialprocessesandproductuse",
#   "edgar_agg_CH4_Industrialprocessesandproductuse",
#   "edgar_agg_CO2f_Industrialprocessesandproductuse",
#   "edgar_agg_N2O_Industrialprocessesandproductuse"
# )
# dep_variables2 <- c(
#   "edgar_agg_GHG_Agricultureforestryandotherlanduse",
#   "edgar_agg_CH4_Agricultureforestryandotherlanduse",
#   "edgar_agg_CO2f_Agricultureforestryandotherlanduse",
#   "edgar_agg_CO2o_Agricultureforestryandotherlanduse",
#   "edgar_agg_N2O_Agricultureforestryandotherlanduse"
# )

summary_table <- data.frame(
  variable = "",
  coefficient = 0.00,
  significance = "***",
  std_error = 0.00,
  dep_var = ""
)
summary_table <- summary_table[0,]
summary_table_wide <- NULL
impacts_total <- c()
impacts_colnames <- c()

for(dep_var in dep_variables2) {
  dep_variable <- sprintf("log(%s)", dep_var)
  
  model_base <- as.formula(paste(dep_variable, "~", paste(base_variables, collapse= "+")))
  
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
  
  panel_sac <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                          ,effect = "twoways" # individual and  time
                          ,lag = TRUE
  )
  panel_sac %>% summary %>% print
  
  panel_summary <- summary(panel_sac)
  
  sign <- panel_summary$CoefTable[,"Pr(>|t|)"]
  
  # prepare table with summary coefficients
  ## estimate 2 digits signif*** (std.error 2 digits)
  summary_table <- summary_table %>% dplyr::add_row(
    variable = rownames(panel_summary$CoefTable),
    coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
    significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
    std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2),
    dep_var = dep_var
  )
  
  if(summary_table_wide %>% is.null || rownames(summary_table_wide) %>% is.null) {
    summary_table_wide <- data.frame(
      coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
      significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
      std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2)
    )
    rownames(summary_table_wide) <- rownames(panel_summary$CoefTable)
  } else {
    summary_table_wide <- summary_table_wide %>% cbind(
      data.frame(
        coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
        significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
        std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2)
      )
    )
  }
  
  impac <- spdep::impacts(panel_sac, listw = lw_spatial, time = length(unique(data_panel$year)))
  impac_total <- impac$res$total
  impacts_total <- cbind(impacts_total, impac_total)
  impacts_colnames <- c(impacts_colnames, dep_var)
}

colnames(impacts_total) <- impacts_colnames
rownames(impacts_total) <- colnames(impac$sres$total)

create_latex(summary_table_wide)

impacts_total_energyindustry <- impacts_total
summary_table_wide_energyindustry <- summary_table_wide
# impacts_total_energytransport <- impacts_total
# summary_table_wide_energytransport <- summary_table_wide
# impacts_total_industry <- impacts_total
# summary_table_wide_industry <- summary_table_wide
# impacts_total_waste <- impacts_total
# summary_table_wide_waste <- summary_table_wide
# impacts_total_agri <- impacts_total
# summary_table_wide_agri <- summary_table_wide

# vars_by_sector <- data.frame(
#   agri = matrixStats::rowVars(impacts_total_agri),
#   energyindustry = matrixStats::rowVars(impacts_total_energyindustry),
#   energytransport = matrixStats::rowVars(impacts_total_energytransport),
#   industry = matrixStats::rowVars(impacts_total_industry)
# )
# 
# sums <- data.frame(
#   ghg = vars_by_ghg %>% mutate(sum = co2f + co2o + ch4 + n2o) %>% pull(sum),
#   sector = vars_by_sector %>% mutate(sum = agri + energyindustry + energytransport + industry) %>% pull(sum)
# )
# rownames(sums) <- rownames(impacts_total)
# 
# all_vars <- cbind(vars_by_ghg, vars_by_sector) %>% round(2)
# rownames(all_vars) <- rownames(impacts_total)

# ------------------------------------------------------------------------------
# 4. Different panel models
# a-spatial panel, SEM, SAR, SARAR

impacts_total <- c()
impacts_colnames <- c()

model_base <- as.formula(paste("log(edgar)", "~", paste(base_variables, collapse= "+")))

panel_aspatial <- plm::plm(model_base, data_panel, index = c("nuts3_id", "year"), 
                           effect = "time", # "twoways"
                           model = "within")
panel_summary <- summary(panel_aspatial)
sign <- panel_summary$coefficients[,"Pr(>|t|)"]
summary_table_wide_aspatial <- data.frame(
  coefficient = panel_summary$coefficients[,"Estimate"] %>% round(digits = 2),
  significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
  std_error = panel_summary$coefficients[,"Std. Error"] %>% round(digits = 2)
)
summary_table_wide_aspatial <- rbind(data.frame(coefficient = NA, significance = "", std_error = NA), 
                                     data.frame(coefficient = NA, significance = "", std_error = NA),
                                     summary_table_wide_aspatial)

panel_sac <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                        # ,model = "random"
                        ,effect = "twoways" # individual and  time
                        ,lag = TRUE
                        # ,spatial.error = "kkp"
)
panel_summary <- summary(panel_sac)
sign <- panel_summary$CoefTable[,"Pr(>|t|)"]
summary_table_wide_sac <- data.frame(
  coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
  significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
  std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2)
)

panel_sar <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                        # ,model = "random"
                        ,effect = "twoways" # individual and  time
                        ,lag = TRUE
                        ,spatial.error = "none"
)
panel_summary <- summary(panel_sar)
sign <- panel_summary$CoefTable[,"Pr(>|t|)"]
summary_table_wide_sar <- data.frame(
  coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
  significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
  std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2)
)
summary_table_wide_sar <- rbind(summary_table_wide_sar[1,], data.frame(coefficient = NA, significance = "", std_error = NA), summary_table_wide_sar[2:nrow(summary_table_wide_sar),])

panel_sem <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial
                        ,effect = "twoways" # individual and  time
                        ,lag = FALSE
)
panel_summary <- summary(panel_sem)
sign <- panel_summary$CoefTable[,"Pr(>|t|)"]
summary_table_wide_sem <- data.frame(
  coefficient = panel_summary$CoefTable[,"Estimate"] %>% round(digits = 2),
  significance = ifelse(sign < 0.01, "***", ifelse(sign < 0.05, "**", ifelse(sign < 0.1, "*", ""))),
  std_error = panel_summary$CoefTable[,"Std. Error"] %>% round(digits = 2)
)                  
summary_table_wide_sem <- rbind(data.frame(coefficient = NA, significance = "", std_error = NA), summary_table_wide_sem)

# store impacts
impacts_total <- c()
impacts_colnames <- c()

impac_sar <- spdep::impacts(panel_sar, listw = lw_spatial, time = length(unique(data_panel$year)))
impac_total <- impac_sar$res$total
impacts_total <- cbind(impacts_total, impac_total)
impacts_colnames <- c(impacts_colnames, "SAR")

impac_sac <- spdep::impacts(panel_sac, listw = lw_spatial, time = length(unique(data_panel$year)))
impac_total <- impac_sac$res$total
impacts_total <- cbind(impacts_total, impac_total)
impacts_colnames <- c(impacts_colnames, "SAC")

impacts_total %>% round(2)

summary_table <- cbind(
  summary_table_wide_aspatial,
  summary_table_wide_sem,
  summary_table_wide_sar,
  summary_table_wide_sac
)
create_latex(summary_table)