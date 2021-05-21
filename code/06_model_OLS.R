# load data
# data <- readRDS("input/data.rds")

# Base model -------------------------------------------------------------------
model_base <- log(edgar) ~ log(pop) + log(density) + log(gdppc) + 
  I(log(gdppc)^2) + gwa_share_BE +
  log(hdd) + log(cdd_fix)
ols_base <- lm(model_base, data)
summary(ols_base)

# check for MAUP
ols_maup <- lm(model_base, data_nuts2)
summary(ols_maup)

# Model with country dummies ---------------------------------------------------
model_cntr <- log(edgar) ~ log(pop) + log(density) + log(gdppc) + 
  I(log(gdppc)^2) + gwa_share_BE + 
  log(hdd) + log(cdd_fix) + cntr_code
ols_cntr <- lm(model_cntr, data)
summary(ols_cntr)

model_base_no_density <- log(edgar) ~ log(pop) + log(gdppc) + 
  I(log(gdppc)^2) + gwa_share_BE +
  log(hdd) + log(cdd_fix)
ols_no_density <- lm(model_base_no_density, data)
summary(ols_no_density)

# OLS TABLE
# Output for Presentation (fontsize = tiny):
################################################################################
output <- stargazer::stargazer(ols_base, ols_cntr, digits=2, # type = "text", 
                               single.row = TRUE, no.space = TRUE,
                               dep.var.labels = "CO2",
                               covariate.labels = c("Population", "Density", "GDP/cap", "GDP/cap, squared",
                                                  "GWA", "HDD", "CDD"),
                               column.sep.width = "3pt", font.size = "tiny",
                               title = "OLS Regression Results",
                               out = "output/tables/OLS.tex") # flip = TRUE ?

# filter out all entries containing country dummies
output <- output[!grepl("cntr",output)]
constant_line_nr <- grep("Constant",output)
dummy_line <- "  Country Dummies & No & Yes \\\\" # 4 backslashes makes 2

# put output together
output <- c(output[1:constant_line_nr],
            dummy_line,
            output[(constant_line_nr+1):length(output)])

# save output
cat(output, file = "./output/tables/OLS_dummyshort.tex", sep = "\n")

# Output for Presentation (fontsize = footnotesize):
################################################################################
output <- stargazer::stargazer(ols_base, ols_cntr, digits=2, # type = "text", 
                               single.row = TRUE, no.space = TRUE,
                               dep.var.labels = "CO2",
                               covariate.labels = c("Population", "Density", "GDP/cap", "GDP/cap, squared",
                                                    "GWA", "HDD", "CDD"),
                               column.sep.width = "3pt", font.size = "footnotesize",
                               title = "OLS Regression Results",
                               out = "output/tables/OLS_footnotesize.tex") # flip = TRUE ?

# filter out all entries containing country dummies
output <- output[!grepl("cntr",output)]
constant_line_nr <- grep("Constant",output)
dummy_line <- "  Country Dummies & No & Yes \\\\" # 4 backslashes makes 2

# put output together
output <- c(output[1:constant_line_nr],
            dummy_line,
            output[(constant_line_nr+1):length(output)])

# save output
cat(output, file = "./output/tables/OLS_dummyshort_paper.tex", sep = "\n")

# Moran I test -----------------------------------------------------------------

# save plots under
plot_path <- "output/plots/"
theme_set(theme_minimal())

## 1. Create proper W matrix

data_coords <- st_coordinates(st_centroid(data$geometry))

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
lw_inversedist_all <- nb2listw(lw_d, glist=idlist, style="W")
m_all <- listw2mat(lw_inversedist_all)

### convert all to lw objects
lw_knn <- lw_knn %>% nb2listw()
lw_d <- lw_d %>% nb2listw()

## 2. Moran's I Test
moran.test(data$edgar, lw_knn)
moran.test(data$edgar, lw_inversedist)
# moran.test(data$edgar, lw_d)
moran.test(data$edgar, lw_inversedist_all)

moran.test(ols_base$residuals, lw_knn)
moran.test(ols_cntr$residuals, lw_knn)

moran.test(ols_base$residuals, lw_inversedist)
moran.test(ols_cntr$residuals, lw_inversedist)

moran.test(ols_base$residuals, lw_inversedist_all)
moran.test(ols_cntr$residuals, lw_inversedist_all)

## 3. Local Moran's I Test

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

plot_localmoran_sig(data$edgar, data, lw = lw_knn, subtitle = "edgar / k-nearest-neighbours")
plot_localmoran_sig(data$edgar, data, lw = lw_inversedist, subtitle = "edgar / KNN-inverse-distance")
plot_localmoran_sig(data$edgar, data, lw = lw_inversedist_all, subtitle = "edgar / inverse-distance")

plot_localmoran_sig(ols_base$residuals, data, lw = lw_knn, subtitle = "OLS residuals / k-nearest-neighbours")
plot_localmoran_sig(ols_cntr$residuals, data, lw = lw_knn, subtitle = "OLS CFE residuals / k-nearest-neighbours")

plot_localmoran_sig(ols_base$residuals, data, lw = lw_inversedist, subtitle = "OLS residuals / inverse distance")
plot_localmoran_sig(ols_cntr$residuals, data, lw = lw_inversedist, subtitle = "OLS CFE residuals / inverse distance")

plot_localmoran_sig(ols_base$residuals, data, lw = lw_inversedist_all, subtitle = "OLS residuals / inverse distance all")
plot_localmoran_sig(ols_cntr$residuals, data, lw = lw_inversedist_all, subtitle = "OLS CFE residuals / inverse distance all")

