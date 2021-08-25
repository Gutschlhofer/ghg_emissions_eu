
lw_spatial <- lw_queen
lw_spatial <- lw_inversedist

dep_variable <- "log(edgar)"

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

model_base <- as.formula(paste(dep_variable, "~", paste(base_variables, collapse= "+")))
model_cntr <- as.formula(paste(dep_variable, "~", paste(base_variables[base_variables!="log(REN)"], collapse= "+"), " + cntr_code"))
model_base_no_density <- as.formula(paste(dep_variable, "~", paste(base_variables[base_variables!="log(density)"], collapse= "+")))


model_mess <- spatialreg::lagmess(model_base, data = data, listw = lw_spatial)
summary(model_mess)
# Residual standard error: 0.63392 on 1081 degrees of freedom
# Multiple R-squared:  0.56312,	Adjusted R-squared:  0.55908 
# F-statistic: 139.34 on 10 and 1081 DF,  p-value: < 2.22e-16
# 
# Alpha: -0.30404, standard error: 0.0355
# z-value: -8.5644, p-value: < 2.22e-16
# LR test value: 79.291, p-value: < 2.22e-16
# Implied rho: 0.2621655 


model_slx <- spatialreg::lmSLX(model_base, data = data, listw = lw_spatial)
summary(model_slx)

stuff <- spatialreg::eigenw(lw_spatial)
# stuff <- spatialreg::subgraph_eigenw(model_base, data = data, listw = lw_spatial)


# sacsarlm(formula, data = list(), listw, listw2 = NULL, na.action, Durbin, type,
#          method = "eigen", quiet = NULL, zero.policy = NULL, tol.solve = 1e-10,
#          llprof=NULL, interval1=NULL, interval2=NULL, trs1=NULL, trs2=NULL,
#          control = list())

model_sac <- sacsarlm(model_base, data = data, listw = lw_spatial)
summary(model_sac)
model_sac

impacts(model_sac, listw = lw_spatial)

summary(sacsarlm(model_base_ghg, data, listw = lw_spatial))
summary(sacsarlm(model_base_co2_total, data, listw = lw_spatial))
summary(sacsarlm(model_base_co2, data, listw = lw_spatial))
summary(sacsarlm(model_base_co2_short, data, listw = lw_spatial))
summary(sacsarlm(model_base_n2o, data, listw = lw_spatial))
summary(sacsarlm(model_base_ch4, data, listw = lw_spatial))


sac_gmm <- gstsls(model_base, data, listw = lw_spatial)
summary(sac_gmm)
sac_gmm

####

# now let's try a SEM, then a SAR
model_sem <- spatialreg::errorsarlm(model_base, data = data, listw = lw_spatial)
summary(model_sem, Hausman = TRUE)

HausmanTest <- Hausman.test(model_sem)
HausmanTest

####

model_sar <- spatialreg::lagsarlm(model_base, data = data, listw = lw_spatial)
summary(model_sar)

LR.Sarlm(model_sac, model_sar)
LR.Sarlm(model_sar, model_sac)

#####

model_sdem <- spatialreg::errorsarlm(model_base, 
                                     data = data, 
                                     listw = lw_spatial, 
                                     etype = "emixed")
model_sdem %>% summary()
# no significant lag

# spatial filtering ------------------------------------------------------------
# but, do spatial methods actually help overcome our probable problem of varying coefficients in GWR?
ev_filter <- spatialreg::SpatialFiltering(model_base, data = data, nb = poly2nb(data, queen = TRUE), style = "W")
ev_filter$dataset
ev_filter$selection

n_ev <- 10

ev_sel <- ev_filter$dataset[,1:n_ev]
colnames(ev_sel) = paste0("ev", 1:n_ev)

data_ev <- data %>% cbind(ev_sel)

dep_var <- "edgar"

dep_variable <- sprintf("log(%s)", dep_var) #"log(edgar)"

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

