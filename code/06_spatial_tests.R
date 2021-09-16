# Moran's I test ---------------------------------------------------------------

# save plots under
plot_path <- "output/plots/ghg_select/"
theme_set(theme_minimal())

## 1. Create proper W matrix

data_coords <- st_coordinates(st_centroid(data$geometry))

### queen
lw_queen <- poly2nb(data, queen = TRUE)

### knn
k <- round(0.1 * nrow(data)) # use 10% of the sample as neighbours (109)
lw_knn <- knearneigh(data_coords, k=k) %>% 
  knn2nb()

### inverse distance (based on knn)
dlist <- nbdists(lw_knn, data_coords, longlat = TRUE)
idlist <- lapply(dlist, function(x) 1/x)
lw_inversedist <- nb2listw(lw_knn, glist=idlist, style="W")
m <- listw2mat(lw_inversedist)

### inverse distance (for all)
lw_d <- dnearneigh(data_coords, 0, Inf, longlat = TRUE)
dlist <- nbdists(lw_d, data_coords, longlat = TRUE)
idlist <- lapply(dlist, function(x) 1/x)
lw_inversedist_all <- nb2listw(lw_d, glist=idlist, style="W"); rm(lw_d)
m_all <- listw2mat(lw_inversedist_all)

### convert all to lw objects
lw_knn <- lw_knn %>% nb2listw()
lw_queen <- lw_queen %>% nb2listw()

#####################

perform_spatial_tests <- function(dep_var = dep_var, data = data) {
  
  print(dep_var)
  
  if(dep_var != "edgar") {
    data_test <- data %>% 
      dplyr::select(-edgar) %>% 
      dplyr::rename(edgar = all_of(dep_var))
  } else {
    data_test <- data
  }
  
  ols_base <- readRDS(sprintf("./output/regressions/OLS_%s.rds", dep_var))
  # ols_cntr <- readRDS(sprintf("./output/regressions/OLS_%s_cntr.rds", dep_var))
  
  ## 2. Moran's I Test
  moran.test(data_test$edgar, lw_knn) %>% print()
  moran.test(data_test$edgar, lw_inversedist) %>% print()
  moran.test(data_test$edgar, lw_inversedist_all) %>% print()
  moran.test(data_test$edgar, lw_queen) %>% print()

  moran.test(ols_base$residuals, lw_knn) %>% print()
  # moran.test(ols_cntr$residuals, lw_knn) %>% print()

  moran.test(ols_base$residuals, lw_inversedist) %>% print()
  # moran.test(ols_cntr$residuals, lw_inversedist) %>% print()

  moran.test(ols_base$residuals, lw_inversedist_all) %>% print()
  # moran.test(ols_cntr$residuals, lw_inversedist_all) %>% print()

  moran.test(ols_base$residuals, lw_queen) %>% print()
  # moran.test(ols_cntr$residuals, lw_queen) %>% print()
  
  # Local Moran's I Test ---------------------------------------------------------
  
  plot_template <- paste0("localmoran_sig_%s",".png")
  not_sig_str <- "n.sig"
  
  plot_localmoran_sig <- function(test_input, data, lw, subtitle) {
    localI <- data.frame(localmoran(test_input, lw))
    data_I <- cbind(data, localI)
    
    data_I <- data_I %>% dplyr::mutate(p = `Pr.z...0.`, 
                                       sig=ifelse(p < 0.001, "p < 0.001", 
                                                  ifelse(p < 0.05, 
                                                         "p < 0.05", not_sig_str)),
                                       sig=ifelse(sig == not_sig_str,sig,
                                                  paste(ifelse(Ii>0,"pos,","neg,"),sig)))
    # set as factor to have logical (and not alphabetical) ordering
    data_I$sig <- factor(data_I$sig, 
                         levels = c(unique(data_I$sig)[unique(data_I$sig)!=not_sig_str] %>% 
                                      sort(),
                                    not_sig_str))
    
    ggplot(data = data_I) +
      geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                      pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                      pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
      geom_sf(aes(fill = sig), color = "white", size=0.01) + 
      scale_fill_manual(values = magma(4)[2:4]) +
      theme(legend.title = element_blank()) +
      geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
    
    # replace special characters subtitle 
    subtitle <- stringr::str_replace(subtitle, " / ", "_")
    subtitle <- stringr::str_replace(subtitle, "-", "_")
    subtitle <- stringr::str_replace(subtitle, " ", "_")
    
    ggsave(path = plot_path, filename = sprintf(plot_template, subtitle), scale=1, width = 4, height = 5)
  }
  
  # plot_localmoran_sig(data_test$edgar, data_test, lw = lw_knn, subtitle = sprintf("%s / k-nearest-neighbours",dep_var))
  # plot_localmoran_sig(data_test$edgar, data_test, lw = lw_inversedist, subtitle = sprintf("%s / KNN-inverse-distance",dep_var))
  # plot_localmoran_sig(data_test$edgar, data_test, lw = lw_inversedist_all, subtitle = sprintf("%s / inverse-distance",dep_var))
  # plot_localmoran_sig(data_test$edgar, data_test, lw = lw_queen, subtitle = sprintf("%s / queen",dep_var))

  # plot_localmoran_sig(ols_base$residuals, data_test, lw = lw_knn, subtitle = sprintf("%s OLS residuals / k-nearest-neighbours",dep_var))
  # plot_localmoran_sig(ols_cntr$residuals, data_test, lw = lw_knn, subtitle = sprintf("%s OLS CFE residuals / k-nearest-neighbours", dep_var))

  plot_localmoran_sig(ols_base$residuals, data_test, lw = lw_inversedist, subtitle = sprintf("%s OLS residuals / inverse distance", dep_var))
  # plot_localmoran_sig(ols_cntr$residuals, data_test, lw = lw_inversedist, subtitle = sprintf("%s OLS CFE residuals / inverse distance", dep_var))

  plot_localmoran_sig(ols_base$residuals, data_test, lw = lw_inversedist_all, subtitle = sprintf("%s OLS residuals / inverse distance all", dep_var))
  # plot_localmoran_sig(ols_cntr$residuals, data_test, lw = lw_inversedist_all, subtitle = sprintf("%s OLS CFE residuals / inverse distance all", dep_var))

  plot_localmoran_sig(ols_base$residuals, data_test, lw = lw_queen, subtitle = sprintf("%s OLS residuals / queen", dep_var))
  # plot_localmoran_sig(ols_cntr$residuals, data_test, lw = lw_queen, subtitle = sprintf("%s OLS CFE residuals / queen", dep_var))
}

# dep_variables2 <- dep_variables
# dep_variables2 <- ghg_aggregate_over_sector
# dep_variables2 <- dep_variables_agg
dep_variables2 <- dep_variables_sel

capture.output(
for(dep_var in dep_variables2) {
  # try is here because sometimes we have no regression result or not enough rows
  try ( perform_spatial_tests(dep_var, data) )
}
, file = "code/08a_moran_tests.txt")

# 3. Model selection tests -----------------------------------------------------

# ML-based Testing

# 2.1 LR Test
## constrained vs unconstrained LR comparison

# 2.2 Wald Test
## compare distance between estimated parameters in constrained and unconstrained form

# 2.3 Lagrange Multiplier Tests
## we only estimate H0 model

make_model_recommendation <- function(lm_test) {
  result <- "OLS"
  
  if(lm_test$LMerr$p.value < 0.05 &
     lm_test$LMlag$p.value < 0.05){
    # check robust versions
    if(lm_test$RLMerr$p.value < 0.05 &
       lm_test$RLMlag$p.value < 0.05){
      # this indicates that SARMA might be suitable
      if(lm_test$SARMA$p.value < 0.05) {
        result <- "SARMA"
      } else {
        result <- "inconclusive (both RLMerr and lag are <0.05 but SARMA is >0.05"
      }
    } else if(lm_test$RLMerr$p.value < 0.05) {
      result <- "SEM"
    } else if(lm_test$RLMlag$p.value < 0.05) {
      result <- "SAR"
    }
  } else if (lm_test$LMerr$p.value < 0.05) {
    result <- "SEM"
  } else if (lm_test$LMlag$p.value < 0.05) {
    result <- "SAR"
  }
  return(result)
}

# dep_variables2 <- dep_variables
# dep_variables2 <- ghg_aggregate_over_sector
dep_variables2 <- dep_variables_sel

capture.output(
for(dep_var in dep_variables2) {
  
  print(dep_var)
  
  if(!file.exists(sprintf("./output/regressions/OLS_%s.rds", dep_var))) next()
  
  ols_base <- readRDS(sprintf("./output/regressions/OLS_%s.rds", dep_var))
  # ols_cntr <- readRDS(sprintf("./output/regressions/OLS_%s_cntr.rds", dep_var))
  # ols_no_density <- readRDS(sprintf("./output/regressions/OLS_%s_no_density.rds", dep_var))
  
  if(nobs(ols_base) < nrow(data)) next()
  
  lw_spatial <- lw_inversedist
  
  # # different models with lw_spatial
  # lm.LMtests(ols_cntr, lw_spatial, test = c("all")) %>% make_model_recommendation %>% print
  # # lm.LMtests(ols_maup, lw_spatial, test = c("all")) 
  # lm.LMtests(ols_no_density, lw_spatial, test = c("all")) %>% make_model_recommendation %>% print
  
  # test remaining neighbourhoods
  lm.LMtests(ols_base, lw_queen, test = c("all")) %>% make_model_recommendation %>% print 
  lm.LMtests(ols_base, lw_inversedist, test = c("all")) %>% make_model_recommendation %>% print 
  lm.LMtests(ols_base, lw_inversedist_all, test = c("all")) %>% make_model_recommendation %>% print
  lm.LMtests(ols_base, lw_knn, test = c("all")) %>% make_model_recommendation %>% print
}
, file = "code/08b_model_recommendations.txt")

recommendations <- read_lines("code/08b_model_recommendations.txt")
# remove unnecessary parts
recommendations <- gsub('[1]', "", recommendations, fixed = T)
recommendations <- gsub(' \"', "", recommendations)
recommendations <- gsub('\"', "", recommendations)
# exclude variable names
recommendations <- recommendations[!grepl("edgar", recommendations)]
# show distribution of recommendations
table(recommendations)

# Elhorst 2010 approach --------------------------------------------------------
lw_spatial <- lw_inversedist

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

elhorst_suggestion <- data.frame(
  variable = c(""),
  weights_matrix = c(""),
  suggestion = c("")
)
elhorst_suggestion <- elhorst_suggestion[0,]

capture.output(
for(dep_var in dep_variables2){

  print("-------------------------------------------")
  print(dep_var)
  
  dep_variable <- sprintf("log(%s)", dep_var)
  model_base <- as.formula(paste(dep_variable, "~", paste(base_variables, collapse= "+")))
  
  ## made modifications like we did with OLS -----------------------------------
  if(any(is.na(data %>% st_drop_geometry %>% pull(dep_var)))) {
    omitted <- data[is.na(data %>% st_drop_geometry %>% pull(dep_var)),]
    
    data <- data[!is.na(data %>% st_drop_geometry %>% pull(dep_var)),]
    
    output <- sprintf("%s: %.0f rows omitted, countries: %s", dep_var, nrow(omitted), paste(omitted$cntr_code %>% unique, collapse = ","))
    
    cat(output, file = sprintf("./output/tables/OLS_%s_omitted.txt", dep_var), sep = "\n")
  }
  
  # check if there are 0 values in the dependent variable
  # this is an issue since we take the log
  if(any(data %>% st_drop_geometry %>% pull(dep_var) == 0)) {
    # separate the column we want to change (otherwise selection without geometry is hard)
    temp <- data %>% st_drop_geometry %>% dplyr::select(all_of(dep_var))
    data <- data %>% dplyr::select(-all_of(dep_var))
    
    # add 1kg CO2 equiv to all observations
    temp[,dep_var] <- temp[,dep_var]+0.001
    # TODO: robustness check with Inverse hyperbolic sine (IHS) transformation
    
    data <- data %>% cbind(temp)
  }
  
  if(nrow(data) == 0) next()
  ## back to testing -----------------------------------------------------------
  
  # run for different weights matrices
  for(i in 1:4) {
    
    sugg <- "" # string for Elhorst suggested model
    
    print("-----------------")
    lw_string <- switch(i,
                        "lw_inversedist",
                        "lw_inversedist_all",
                        "lw_queen",
                        "lw_knn")
    print(lw_string)
    
    lw_spatial <- switch(i,
                         lw_inversedist,
                         lw_inversedist_all,
                         lw_queen,
                         lw_knn)
    # ols_base <- readRDS(sprintf("./output/regressions/OLS_%s.rds", dep_var))
    ols_base <- lm(model_base, data) # not much of a time-gain to get it from files and increased certainty that we deal with same data
    
    lm_test <- lm.LMtests(ols_base, lw_spatial, test = c("all"))
    
    # first, check robust LM for p of either rho or lambda < 0.05
    if(lm_test$RLMerr$p.value < 0.05 |
       lm_test$RLMlag$p.value < 0.05){
      # try SDM
      model_sdm <- lagsarlm(model_base, data = data, listw = lw_spatial, Durbin = TRUE)
      summary(model_sdm)
      
      # now check between SEM and SDM (common factor hypothesis)
      model_sem <- spatialreg::errorsarlm(model_base, data = data, listw = lw_spatial)
      summary(model_sem, Hausman = TRUE)
      ht <- Hausman.test(model_sem)
      if(ht$p.value < 0.05) { print("Hausman: SEM is better than OLS.") } else { print("Hausman: SEM is not significantly better than OLS.") }
      
      # model_sdm: constraint-free model
      # model_sem: constrained model
      cfh <- spatialreg::LR.Sarlm(model_sdm, model_sem)
      # -> SDM might be SEM
      if(cfh$p.value < 0.05) { print("Common factor hypothesis rejected: SDM different from and more likely than SEM.") } else {
        print("Common factor hypothesis NOT rejected: SDM NOT significantly different from SEM.")
        
        model_sac <- sacsarlm(model_base, data = data, listw = lw_spatial)
        sac_vs_sem <- spatialreg::LR.Sarlm(model_sac, model_sem)
        if(sac_vs_sem$p.value < 0.05) {
          print("... and SAC is more likely than SEM.")
          sugg <- "SAC"
        } else {
          print("... and SAC is NOT more likely than SEM.")
          sugg <- "SEM" # I later check against SDEM
        }
      }
      
      model_sar <- lagsarlm(model_base, data = data, listw = lw_spatial, Durbin = FALSE)
      sdm_vs_sar <- spatialreg::LR.Sarlm(model_sdm, model_sar)
      if(sdm_vs_sar$p.value < 0.05) { 
        print("SDM more likely than SAR.")
        # if(sugg == "") sugg <- "SDM"
      } else {
        print("SDM NOT more likely than SAR.")
        if(sugg == "") sugg <- "SAR"
      }
      
      # cannot compare SAC and SDM like that, since one is not the constrained version of the other
      model_sac <- sacsarlm(model_base, data = data, listw = lw_spatial)
      sac_vs_sdm <- spatialreg::LR.Sarlm(model_sac, model_sdm)
      if(sac_vs_sdm$p.value < 0.05) { # significant difference
        if(logLik(model_sac) > logLik(model_sdm)) {
          print("Significant difference between SAC and SDM: SAC is more likely.")
        } else {
          print("Significant difference between SAC and SDM: SDM is more likely.")
        }
      } else { # no significant difference
        if(logLik(model_sac) > logLik(model_sdm)) {
          print("SDM NOT significantly different from SAC, but SAC is more likely.")
        } else {
          print("SDM NOT significantly different from SAC, but SDM is more likely.")
        }
      }
      
      model_sdem <- spatialreg::errorsarlm(model_base, data = data, listw = lw_spatial, etype = "emixed")
      summary(model_sdem)
      sdem_vs_sem <- spatialreg::LR.Sarlm(model_sdem, model_sem)
      if(sdem_vs_sem$p.value < 0.05) { print("SDEM more likely than SEM."); sugg <- ifelse(sugg == "SEM", "SDEM", sugg) } else {"SDEM NOT more likely than SEM."}
      
      if(sugg == "") {
        sugg <- "SDM"
      }
    } else {
      # try SLX
      model_slx <- lmSLX(model_base, data = data, listw = lw_spatial, Durbin = TRUE)
      summary(model_slx)
      
      if(AIC(model_slx) < AIC(ols_base)) { # AIC tells us that SLX is better suited
        # now Elhorst tells us to try SDM again and check for theta = 0
        model_sdm <- lagsarlm(model_base, data = data, listw = lw_spatial, Durbin = TRUE)
        
        sdm_vs_slx <- spatialreg::LR.Sarlm(model_sdm, model_slx)
        
        if(sdm_vs_slx$p.value < 0.05) {
          print("SDM is preferred to SLX.")
          sugg <- "SDM"
        } else {
          print("SLX is the suggested model.")
          sugg <- "SLX"
        }
      } else {
        print("OLS is the suggested model.")
        sugg <- "OLS"
      }
    }
    
    elhorst_suggestion <- elhorst_suggestion %>% add_row(
      variable = dep_var,
      weights_matrix = lw_string,
      suggestion = sugg
    )
  }
}
, file = "code/08c_model_recommendations_Elhorst.txt")

table(elhorst_suggestion$suggestion) # Elhorst 2010 mainly suggests SDM
