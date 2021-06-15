# load data
# data <- readRDS("input/data.rds")

# ensure that all plots have the same theme
theme_set(theme_minimal())

# save plots under
plot_path <- "output/plots/"
plot_template <- paste0("raw_%s",".png")

if(!dir.exists(plot_path)) dir.create(plot_path)

# save plots?
s <- T

# divide into 5 quantiles + corresponding legend
plot_quantiles <- function(variable_name, data, s = F, shape_nuts0 = shape_nuts0,
                           plot_path = plot_path, plot_template = plot_template) {
  library(tidyverse)
  theme_set(theme_minimal())
  
  plot_name <- paste0(variable_name, "_quantiles")
  
  no_classes <- 5
  var_vector <- data %>% pull(all_of(variable_name))
  quantiles <- quantile(var_vector, probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE)
  
  quantiles <- quantiles[!duplicated(quantiles)]
  
  labels <- c()
  for(idx in 1:length(quantiles)){
    labels <- c(labels, paste0(round(quantiles[idx], -2),
                               " – ",
                               round(quantiles[idx + 1], -2)))
  }
  
  # remove the last label because that would be something like "100.000 - NA"
  labels <- labels[1:length(labels)-1]
  
  data$quantiles <- cut(var_vector, 
                        breaks = quantiles, 
                        labels = labels, 
                        include.lowest = T)
  
  p <- ggplot(data = data) +
    ggpattern::geom_sf_pattern(
      data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
      pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
      pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = quantiles), color = "white", size=0.01) +
    viridis::scale_fill_viridis(direction = -1, discrete = TRUE) + 
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='black', fill=NA, size=0.1) + 
    theme(plot.margin=grid::unit(c(0,0,0,0), "cm")) 
  if(s) ggsave(p, path = plot_path, filename = sprintf(plot_template, plot_name), scale=1, width = 5, height = 5)
  
  return(p)
}

## EDGAR data ------------------------------------------------------------------
cl <- makeCluster(parallel::detectCores())
parLapply(cl = cl, dep_variables, plot_quantiles, data = data, s = s, 
          shape_nuts0 = shape_nuts0, plot_path = paste0(plot_path, "all_ghg/"), plot_template = plot_template)
stopCluster(cl)

# lapply(dep_variables, plot_quantiles, data = data, s = s)
# plot_quantiles(data, variable_name = "edgar", plot_name = "Edgar_quantiles", s = s)

## GDP p.c. --------------------------------------------------------------------
no_classes <- 5
quantiles_gdp <- quantile(data$gdppc, 
                      probs = seq(0, 1, length.out = no_classes + 1))

labels <- c()
for(idx in 1:length(quantiles_gdp)){
  labels <- c(labels, paste0(round(quantiles_gdp[idx], 0), 
                             " – ", 
                             round(quantiles_gdp[idx + 1], 0)))
}
labels <- labels[1:length(labels)-1]

data$gdppc_quantiles <- cut(data$gdppc, 
                            breaks = quantiles_gdp, 
                            labels = labels, 
                            include.lowest = T)

gdp <- ggplot(data = data) +
  geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                  pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                  pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  geom_sf(aes(fill = gdppc_quantiles), color = "white", size=0.01) +
  scale_fill_viridis(direction = -1, discrete = TRUE) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm")) 
if(s) ggsave(gdp ,path = plot_path, filename = sprintf(plot_template, "GDPpc_quantiles"), width = 5, height = 5)



## employment shares -----------------------------------------------------------
gva <- ggplot(data = data) + 
  geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                  pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                  pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  geom_sf(aes(fill = gva_share_BE), color = "white", size=0.01) +
  scale_fill_viridis_c(option = "magma", direction = -1, labels = percent) +  
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))
if(s) ggsave(gva, path = plot_path, filename = sprintf(plot_template, "GVA_share"), width = 4, height = 5)

# possibly only relevant for presentation: combined GDP & GVA share 
# GDP_GVA <- plot_grid(gdp, gva, ncol = 2)
# if(s) ggsave(plot = GDP_GVA, path = plot_path, filename = sprintf(plot_template, "GDP_GVA")) 

## CDD -------------------------------------------------------------------------
cdd <- ggplot(data = data) + 
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  geom_sf(aes(fill = cdd), color = "white", size=0.01) +
  scale_fill_viridis_c(direction = -1) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))
if(s) ggsave(cdd, path = plot_path, filename = sprintf(plot_template, "CDD"), width = 4, height = 5) 



## HDD -------------------------------------------------------------------------
hdd <- ggplot(data = data) + 
  geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe', 
                  pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008, 
                  pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  geom_sf(aes(fill = hdd), color = "white", size=0.01) +
  scale_fill_viridis_c(option="magma", direction = -1) +
  theme(legend.title = element_blank()) + 
  geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))
if(s) ggsave(hdd, path = plot_path, filename = sprintf(plot_template, "HDD"), width = 4, height = 5) 

# combined HDD and CDD
HDD_CDD <- plot_grid(hdd, cdd, ncol = 2)
if(s) ggsave(plot = HDD_CDD, path = plot_path, filename = sprintf(plot_template, "HDD_CDD"), width = 10, height = 6) 



## population ------------------------------------------------------------------
data$pop <- data$pop/1000 # in 1000 people

no_classes <- 5
quantiles_pop <- quantile(data$pop, 
                              probs = seq(0, 1, length.out = no_classes + 1))

labels <- c()
for(idx in 1:length(quantiles_pop)){
  labels <- c(labels, paste0(round(quantiles_pop[idx], -1),
                             "k – ",
                      paste0(round(quantiles_pop[idx + 1], -1), 
                             "k")))
}

labels <- labels[1:length(labels)-1]

data$pop_quantiles <- cut(data$pop, 
                              breaks = quantiles_pop, 
                              labels = labels, 
                              include.lowest = T)

pop <- ggplot(data = data) +
  geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                  pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                  pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) +  
  geom_sf(aes(fill = pop_quantiles), color = "white", size=0.01) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = rev(viridis(8)[3:8])) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))

data$pop <- data$pop*1000


## density ---------------------------------------------------------------------

no_classes <- 5
quantiles_density <- quantile(data$density, 
                      probs = seq(0, 1, length.out = no_classes + 1))

labels <- c()
for(idx in 1:length(quantiles_density)){
  labels <- c(labels, paste0(round(quantiles_density[idx], 3),
                             " – ",
                             paste0(round(quantiles_density[idx + 1], 3)
                                    )))
}

labels <- labels[1:length(labels)-1]

data$density_quantiles <- cut(data$density, 
                            breaks = quantiles_density, 
                            labels = labels, 
                            include.lowest = T)

dens <- ggplot(data = data) +
  geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                  pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                  pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) +  
  geom_sf(aes(fill = density_quantiles), color = "white", size=0.01) +
  scale_fill_manual(values = rev(magma(8)[3:8])) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))
if(s) ggsave(dens, path = plot_path, filename = sprintf(plot_template, "Density_quantiles"), width = 4, height = 5)


# pop_dens
pop_dens <- plot_grid(pop, dens, ncol = 2)
if(s) ggsave(pop_dens, path = plot_path, filename = sprintf(plot_template, "Pop_Dens"), width = 10, height = 6)


# Summary table ----------------------------------------------------------------
sum_data <- st_drop_geometry(data)
sum_data <- sum_data %>% dplyr::select(gdppc, density, gva_share_BE, edgar, hdd, cdd_fix)

# st_options(descr.transpose = TRUE)
# 
# summary1 <- as.data.frame(descr(sum_data, 
#                   stats= c("min", "q1", "mean", "med", "q3", "max", "sd"),
#                   order = "p"))
# 
# rownames(summary1) <- c("GDP p.c.", "Population density", "Empl. share in manufact.", "CO² emission levels", "Heating Days Index", "Cooling Days Index")
# summary1 <- round(summary1, 2)
# summary1

# equivalent to table 1 from videras
sum_data_log <- log(sum_data)

summary2 <- rbind(as.data.frame(descr(sum_data,
                                stats = c("mean", "sd"),
                                order = "p")),
                    as.data.frame(descr(sum_data_log,
                                stats = c("mean", "sd"),
                                order = "p")))

colnames(summary2) <- c("GDP p.c.", "Population density", "Empl. share in manufact.", "CO² emission levels", "HDD", "CDD")
rownames(summary2) <- c("Mean (orig. values)", "Std.Dev (orig. values)", "Mean (log values)", "Std.Dev (log values)")
summary2 <- round(summary2, 2)
summary2
