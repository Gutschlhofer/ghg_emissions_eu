base_variables <- c(
  "log(pop)",
  # "log(pop_share_Y15_64)",
  # "log(pop_share_Y20_34)",
  # "log(pop_share_Y35_49)",
  # "log(pop_share_Y50_64)",
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

dep_var <- "edgar"
dep_variable <- sprintf("log(%s)", dep_var)

model_base <- as.formula(paste(dep_variable, "~", paste(base_variables, collapse= "+")))

lw_spatial <- lw_inversedist

model_ols <- lm(model_base, data)

model_sdm <- lagsarlm(model_base, data = data, listw = lw_spatial, Durbin = TRUE)

model_sem <- spatialreg::errorsarlm(model_base, data = data, listw = lw_spatial)

model_sac <- sacsarlm(model_base, data = data, listw = lw_spatial)

model_sar <- lagsarlm(model_base, data = data, listw = lw_spatial, Durbin = FALSE)

model_sdem <- spatialreg::errorsarlm(model_base, data = data, listw = lw_spatial, etype = "emixed")

model_gns <- spatialreg::sacsarlm(model_base, data = data, listw = lw_spatial, type="sacmixed")

summary(model_sem)

summary(model_sar)
stargazer(model_sar, type = "text", single.row = T, digits = 2)

summary(model_sac)

summary(model_sdm)
stargazer(model_sdm, type = "text", single.row = T, digits = 2)

summary(model_sdem)

round_impacts <- function(model, listw, digits = 2) {
  df <- impacts(model, listw = listw)
  is_sdem <- FALSE
  if(!is.null(df$impacts)) {
    df <- df$impacts
    is_sdem <- TRUE
  }
  df$direct <- round(df$direct, digits = digits)
  df$indirect <- round(df$indirect, digits = digits)
  df$total <- round(df$total, digits = digits)
  if(is_sdem) df <- df %>% as.data.frame()
  return(df)
}
t_impacts <- Matrix(nrow = length(base_variables)+2, ncol = 4, data = 0)
t_impacts[,1] <- round_impacts(model = model_sar, listw = lw_spatial, digits = 2)$total
t_impacts[,2] <- round_impacts(model = model_sac, listw = lw_spatial, digits = 2)$total
t_impacts[,3] <- round_impacts(model = model_sdm, listw = lw_spatial, digits = 2)$total
t_impacts[,4] <- round_impacts(model = model_sdem, listw = lw_spatial, digits = 2)$total

stargazer(t_impacts %>% as.matrix(), digits = 2)
