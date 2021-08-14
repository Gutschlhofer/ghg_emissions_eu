# Moran's I test ---------------------------------------------------------------

# save plots under
plot_path <- "output/plots/ghg_select/"
theme_set(theme_minimal())

## 1. Create proper W matrix

data_coords <- st_coordinates(st_centroid(data$geometry))

### queen
lw_queen <- poly2nb(data, queen = TRUE)

### knn
k <- round(0.163404391231319 * nrow(data)) # use amount of neighbours determined by gwr_sel
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

dep_variables2 <- dep_variables
# dep_variables2 <- ghg_aggregate_over_sector
for(dep_var in dep_variables2) {
  # try is here because sometimes we have no regression result or not enough rows
  try ( perform_spatial_tests(dep_var, data) )
}
# until incl edgar_CH4_MNM
  
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

dep_variables2 <- dep_variables
# dep_variables2 <- ghg_aggregate_over_sector

capture.output(
for(dep_var in dep_variables2) {
  
  print(dep_var)
  
  if(!file.exists(sprintf("./output/regressions/OLS_%s.rds", dep_var))) next()
  
  ols_base <- readRDS(sprintf("./output/regressions/OLS_%s.rds", dep_var))
  ols_cntr <- readRDS(sprintf("./output/regressions/OLS_%s_cntr.rds", dep_var))
  ols_no_density <- readRDS(sprintf("./output/regressions/OLS_%s_no_density.rds", dep_var))
  
  if(nobs(ols_no_density) < nrow(data)) next()
  
  lw_spatial <- lw_inversedist
  
  # this test indicates that compared to OLS, both SAR and SEM make sense
  # however, looking at RLMerr and RLMlag values, there appears to be spatial dependence
  # even considering a spatial lag, however, considering a SEM, an additional lag
  # in y does not make sense anymore
  
  # with queen contiguity we observe p=0.01262 for RLMlag implying that a lag makes sense
  
  # different models with lw_spatial
  lm.LMtests(ols_cntr, lw_spatial, test = c("all")) %>% make_model_recommendation %>% print # SEM
  # lm.LMtests(ols_maup, lw_spatial, test = c("all")) # 
  lm.LMtests(ols_no_density, lw_spatial, test = c("all")) %>% make_model_recommendation %>% print # -> SEM (even with queen)
  
  # test remaining neighbourhoods
  lm.LMtests(ols_base, lw_queen, test = c("all")) %>% make_model_recommendation %>% print # suggests SAC (p-value = 0.01262)
  lm.LMtests(ols_base, lw_inversedist_all, test = c("all")) %>% make_model_recommendation %>% print # suggests SAC (p-value = 0.03701)
  lm.LMtests(ols_base, lw_knn, test = c("all")) %>% make_model_recommendation %>% print # SEM
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
