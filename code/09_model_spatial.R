
lw_spatial <- lw_queen
lw_spatial <- lw_inversedist

# 1. Model with everything

# Then we test downwards

# Also we need a pooled model soon!

# also other dep variables

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


# negative rho (-.2, positive lambda .7)

# Rho: -0.2019
# Asymptotic standard error: 0.052748
# z-value: -3.8276, p-value: 0.00012942
# Lambda: 0.73791
# Asymptotic standard error: 0.032176
# z-value: 22.933, p-value: < 2.22e-16
# 
# LR test value: 251.36, p-value: < 2.22e-16
# --> this should mean that it's better than the comparison models
# 
# Log likelihood: -960.146 for sac model
# ML residual variance (sigma squared): 0.29207, (sigma: 0.54044)
# Number of observations: 1092 
# Number of parameters estimated: 14 
# AIC: 1948.3, (AIC for lm: 2195.7)

sac_gmm <- gstsls(model_base, data, listw = lw_spatial)
summary(sac_gmm)
sac_gmm # again negative rho

####

# now let's try a SEM, then a SAR
model_sem <- spatialreg::errorsarlm(model_base, data = data, listw = lw_spatial)
summary(model_sem, Hausman = TRUE)
# Lambda: 0.64495, LR test value: 236.66, p-value: < 2.22e-16
# Asymptotic standard error: 0.02793
# z-value: 23.092, p-value: < 2.22e-16
# Wald statistic: 533.22, p-value: < 2.22e-16
# 
# Log likelihood: -967.4956 for error model
# ML residual variance (sigma squared): 0.31095, (sigma: 0.55763)
# Number of observations: 1092 
# Number of parameters estimated: 13 
# AIC: 1961, (AIC for lm: 2195.7)

HausmanTest <- Hausman.test(model_sem)
HausmanTest

####

model_sar <- spatialreg::lagsarlm(model_base, data = data, listw = lw_spatial)
summary(model_sar)
# Rho: 0.2868, LR test value: 80.426, p-value: < 2.22e-16
# Asymptotic standard error: 0.028923
# z-value: 9.9162, p-value: < 2.22e-16
# Wald statistic: 98.331, p-value: < 2.22e-16
# 
# Log likelihood: -1045.614 for lag model
# ML residual variance (sigma squared): 0.3907, (sigma: 0.62506)
# Number of observations: 1092 
# Number of parameters estimated: 13 
# AIC: 2117.2, (AIC for lm: 2195.7)
# LM test for residual autocorrelation
# test value: 101.61, p-value: < 2.22e-16

LR.Sarlm(model_sac, model_sar)
LR.Sarlm(model_sar, model_sac)

#####

model_sdem <- spatialreg::errorsarlm(model_base, data = data, listw = lw_spatial, etype = "emixed")
model_sdem %>% summary()
# only significant lag:
#                       Estimate Std. Error  z value  Pr(>|z|)
# log(density)         -0.416501   0.027539 -15.1242 < 2.2e-16
# lag.log(density)      0.251348   0.049941   5.0329 4.830e-07


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
  "log(density)",
  "log_gdppc",
  "I(log_gdppc^2)",
  "log(gva_share_A)",
  "log(gva_share_BE)",
  "log(gva_share_F)",
  "log(gva_share_GJ)",
  "log(hdd)",
  "log(cdd_fix)",
  "urbn_type",
  "coast_type",
  "mount_type"
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

