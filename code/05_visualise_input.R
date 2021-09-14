# # uncomment these to run as job
# setwd("..")
# source("code/00_libraries_functions.R", local = TRUE)
# data <- readRDS("input/data.rds")
# dep_variables <- colnames(data)[grepl("edgar", colnames(data))]

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
                           plot_path = plot_path, plot_template = plot_template,
                           legend_title = "") {
  library(tidyverse)
  theme_set(theme_minimal())
  
  plot_name <- paste0(variable_name, "_quantiles")
  
  no_classes <- 5
  var_vector <- data %>% pull(all_of(variable_name))
  quantiles <- quantile(var_vector, probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE)
  
  quantiles <- quantiles[!duplicated(quantiles)]
  
  labels <- c()
  for(idx in 1:length(quantiles)){
    round_digits <- ifelse(quantiles[2] < 10, ifelse(quantiles[2] < 2, 1, 0), -2)
    
    labels <- c(labels, paste0(round(quantiles[idx], round_digits),
                               " – ",
                               round(quantiles[idx + 1], round_digits)))
  }
  
  # remove the last label because that would be something like "100.000 - NA"
  labels <- labels[1:length(labels)-1]
  
  if(length(quantiles) > 1) {
    data$quantiles <- cut(var_vector, 
                          breaks = quantiles, 
                          labels = labels, 
                          include.lowest = T)
  } else {
    data$quantiles <- as.factor(var_vector)
  }
  
  p <- ggplot(data = data) +
    ggpattern::geom_sf_pattern(
      data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
      pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
      pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = quantiles), color = "white", size=0.01) +
    viridis::scale_fill_viridis(direction = -1, discrete = TRUE) + 
    guides(fill = guide_legend(reverse = TRUE, title = legend_title)) +
    # theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='black', fill=NA, size=0.1) + 
    theme(plot.margin=grid::unit(c(0,0,0,0), "cm")) 
  if(s) ggsave(p, path = plot_path, filename = sprintf(plot_template, plot_name), scale=1, width = 5, height = 5)
  
  return(p)
}
# plot a continuous variable
plot_c <- function(variable_name, data, s = F, shape_nuts0 = shape_nuts0,
                   plot_path = plot_path, plot_template = plot_template,
                   direction = -1, pct = TRUE) {
  data <- data %>% dplyr::rename(var = all_of(variable_name))
  my_plot <- ggplot(data = data) + 
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = var), color = "white", size=0.01)
  
  if(pct) { my_plot <- my_plot + scale_fill_viridis_c(option = "magma", direction = -1, labels = percent) 
  } else {  my_plot <- my_plot + scale_fill_viridis_c(option = "magma", direction = -1) }
  
  my_plot <- my_plot +
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1) + 
    theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))
  if(s) ggsave(my_plot, path = plot_path, filename = sprintf(plot_template, variable_name), width = 4, height = 5)
  return(my_plot)
}
# plot a discrete variable
plot_d <- function(variable_name, data, s = F, shape_nuts0 = shape_nuts0,
                   plot_path = plot_path, plot_template = plot_template,
                   direction = -1) {
  data <- data %>% dplyr::rename(var = all_of(variable_name))
  my_plot <- ggplot(data = data) + 
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = var), color = "white", size=0.01) +
    scale_fill_viridis_d(option = "magma", direction = direction) +  
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1) + 
    theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))
  if(s) ggsave(my_plot, path = plot_path, filename = sprintf(plot_template, variable_name), width = 4, height = 5)
}

## EDGAR data ------------------------------------------------------------------
files <- list.files("output/plots/all_ghg")
files <- str_replace(files, "_quantiles.png", "")
files <- str_replace(files, "raw_", "")

dep_variables2 <- dep_variables[!dep_variables %in% files]

# this takes a while so it makes sense to parallelize it
cl <- makeCluster(parallel::detectCores())
parLapply(cl = cl, dep_variables2, plot_quantiles, data = data, s = s, 
          shape_nuts0 = shape_nuts0, plot_path = paste0(plot_path, "all_ghg/"), plot_template = plot_template)
stopCluster(cl)

## relative GHG emissions ------------------------------------------------------
p_area <- plot_quantiles(variable_name = "edgar_per_area", 
                         data = data %>% dplyr::mutate(edgar_per_area = edgar/area), # GHG emissions t CO2-eq per km2
                         s = s, 
                         shape_nuts0 = shape_nuts0, 
                         plot_path = paste0(plot_path), 
                         plot_template = plot_template,
                         legend_title = expression(paste(t~CO[2]-eq~per~km^2)))
p_capita <- plot_quantiles(variable_name = "edgar_per_capita", 
                           data = data %>% dplyr::mutate(edgar_per_capita = edgar/pop), # GHG emissions t CO2-eq per capita
                           s = s, 
                           shape_nuts0 = shape_nuts0, 
                           plot_path = paste0(plot_path), 
                           plot_template = plot_template,
                           legend_title = expression(paste(t~CO[2]-eq~per~capita)))

# combined per capita and per area plot
per_a_c <- plot_grid(p_area, p_capita, ncol = 2)
if(s) ggsave(plot = per_a_c, path = plot_path, filename = sprintf(plot_template, "edgar_per_area_capita"), width = 10, height = 5) 

plot_quantiles(variable_name = "edgar_per_area_and_per_capita", 
               data = data %>% dplyr::mutate(edgar_per_area_and_per_capita = edgar/pop/area*1e3), # GHG emissions kg CO2-eq per capita per km2
               s = s, 
               shape_nuts0 = shape_nuts0, 
               plot_path = paste0(plot_path), 
               plot_template = plot_template,
               legend_title = expression(paste(kg~CO[2]-eq/capita/km^2)))

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
plot_c(variable_name = "gva_share_A", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)
plot_c(variable_name = "gva_share_BE", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)
plot_c(variable_name = "gva_share_F", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)
plot_c(variable_name = "gva_share_GJ", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)

## renewable energy shares -----------------------------------------------------
plot_c(variable_name = "REN", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)
plot_c(variable_name = "REN_ELC", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)
plot_c(variable_name = "REN_HEAT_CL", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)
plot_c(variable_name = "REN_TRA", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)

## population shares -----------------------------------------------------------
plot_c(variable_name = "pop_share_Y_GE65", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)
plot_c(variable_name = "pop_share_Y15_64", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)


# possibly only relevant for presentation: combined GDP & GVA share 
# GDP_GVA <- plot_grid(gdp, gva, ncol = 2)
# if(s) ggsave(plot = GDP_GVA, path = plot_path, filename = sprintf(plot_template, "GDP_GVA")) 

## CDD -------------------------------------------------------------------------
cdd <- plot_c(variable_name = "cdd", data = data, s = s, shape_nuts0 = shape_nuts0, 
              plot_path = plot_path, plot_template = plot_template, 
              direction = -1, pct = FALSE)

## HDD -------------------------------------------------------------------------
hdd <- plot_c(variable_name = "hdd", data = data, s = s, shape_nuts0 = shape_nuts0, 
              plot_path = plot_path, plot_template = plot_template, 
              direction = -1, pct = FALSE)

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
if(s) ggsave(dens, path = plot_path, filename = sprintf(plot_template, "density_quantiles"), width = 4, height = 5)


# pop_dens
pop_dens <- plot_grid(pop, dens, ncol = 2)
if(s) ggsave(pop_dens, path = plot_path, filename = sprintf(plot_template, "pop_dens"), width = 10, height = 6)

# plot some discrete variables -------------------------------------------------
plot_d(variable_name = "urbn_type", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)
plot_d(variable_name = "coast_type", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = -1)
plot_d(variable_name = "mount_type", data = data, s = s, shape_nuts0 = shape_nuts0, 
       plot_path = plot_path, plot_template = plot_template, direction = 1)

## EDGAR sector disaggregation -------------------------------------------------
# mosaic plot by GHG, sector

sectors_to_show <- 10

scale_x_list <- function (name = waiver(), breaks = ggmosaic:::product_breaks(), minor_breaks = NULL,
                                 labels = ggmosaic:::product_labels(), limits = NULL, expand = waiver(),
                                 oob = scales:::censor, na.value = NA_real_, trans = "identity",
                                 position = "bottom", sec.axis = waiver())
{
  sc <- ggplot2::continuous_scale(c("x", "xmin", "xmax", "xend",
                                    "xintercept", "xmin_final", "xmax_final", "xlower",
                                    "xmiddle", "xupper"), "position_c", identity, name = name,
                                  breaks = breaks, minor_breaks = minor_breaks, labels = labels,
                                  limits = limits, expand = expand, oob = oob, na.value = na.value,
                                  trans = trans, guide = "none", position = position,
                                  super = ScaleContinuousProduct)
  if (!ggplot2:::is.waive(sec.axis)) {
    if (ggplot2:::is.formula(sec.axis))
      sec.axis <- ggplot2::sec_axis(sec.axis)
    is.sec_axis = getFromNamespace("is.sec_axis", "ggplot2")
    if (!ggplot2:::is.sec_axis(sec.axis))
      stop("Secondary axes must be specified using 'sec_axis()'")
    sc$secondary.axis <- sec.axis
  }
  sc
}

plot_mosaic <- function(data, sector_detail = c("sector_name", "category", "category_main")) {
  
  sectors_to_show <- 10
  
  data_m <- data %>% 
    st_drop_geometry() %>% 
    dplyr::select(nuts3_id, cntr_code, year, 
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
    dplyr::rename(sector = all_of(sector_detail))
  
  # group all transport sectors into one
  data_m <- data_m %>% 
    dplyr::mutate(sector = ifelse(grepl("TNR", short), ifelse(sector_detail != "category_main", "Transport", "Energy - Transport"), sector)) %>% 
    dplyr::mutate(sector = ifelse(grepl("TRO", short), ifelse(sector_detail != "category_main", "Transport", "Energy - Transport"), sector)) %>% 
    dplyr::mutate(sector = ifelse(sector == "Energy", "Energy - Industry", sector)) %>% 
    dplyr::mutate(sector = ifelse(sector == "Fuel combustion activities", "Fuel combustion activities - Industry", sector))
  
  data_m <- data_m %>% dplyr::select(sector, ghg, value)
  
  # get most important sectors
  data_s <- data_m %>% 
    dplyr::group_by(sector) %>% 
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(desc(value)) %>% 
    dplyr::slice_head(n = sectors_to_show)
  
  data_m <- data_m %>% 
    dplyr::mutate(sector = ifelse(sector %in% data_s$sector, sector, "n.e.c.")) %>% 
    dplyr::group_by(sector, ghg) %>% 
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(sector = factor(sector, levels = c(unique(data_s$sector), "n.e.c.")),
                  ghg = as.factor(ghg))
  
  breaks_values <- data_m %>%
    dplyr::group_by(ghg) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE) / 1000) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = cumsum(value) )
  
  # change to string for labelling
  breaks_values$ghg <- as.character(breaks_values$ghg)
  
  # if you only want breaks at actual breakpoints and not also for ghg labels inbetween, use this in the plot and set the labels accordingly
  graph_breaks_temp <- c(0,breaks_values$value) # + c(0,-0.1,-0.15,-0.05,-0.05,-0.05,0.05,0)
  
  ## The code below creates breaks in-between the breaks
  #  in order to show the ghg labels there
  graph_breaks <- c()
  graph_labels <- c()
  for(i in 1:(length(graph_breaks_temp)-1)){
    new_break <- (graph_breaks_temp[i+1]+graph_breaks_temp[i])/2
    
    # if(i == 1){
    #   graph_breaks <- c(graph_breaks_temp[i]) # otherwise we never add index i
    #   graph_labels <- c("0.0")
    # }
    
    # add to the breaks
    graph_breaks <- c(graph_breaks,
                      new_break)#,
    # graph_breaks_temp[i+1])
    # add to the labels
    graph_labels <- c(graph_labels,
                      breaks_values$ghg[i])#,
    # as.character(round(graph_breaks_temp[i+1],1)))
  }
  
  mosaic <- data_m %>%
    ggplot() +
    geom_mosaic(aes(weight = value, x = product(sector, ghg), fill = sector), na.rm=T, divider=ddecker(), offset = 0.005) +
    theme(legend.position="right") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_list(position = "top", 
                 sec.axis = ggplot2::sec_axis(~.*sum(data_m$value/1000), 
                                              breaks = graph_breaks, 
                                              labels = c(
                                                CH4=expression(paste(CH[4])),
                                                CO2f=expression(paste(CO[2]*f)),
                                                CO2o=expression(paste(CO[2]*o)),
                                                N2O=expression(N[2]*O)))) +
    # facet_grid(group~.) +
    labs(x = "", y = "") +
    guides(fill=guide_legend(
      title = switch(sector_detail,
                     "sector_name" = "Sector",
                     "category" = "Category",
                     "category_main" = "Main category"), reverse = TRUE)) +
    viridis::scale_fill_viridis(discrete = TRUE)
  
  # # one can also add text inside the rectangles
  # mosaic +
  #   geom_text(data = ggplot_build(mosaic)$data[[1]] %>% mutate(.wt = round(.wt,0)), aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=replace(.wt, .wt < 300, "")))
  
  if(s) ggsave(mosaic, path = plot_path, filename = sprintf(plot_template, paste0("ghg_mosaic_", sector_detail)), width = ifelse(sector_detail == "category", 12, 8), height = 5)
  
}

data_tmp <- data %>% dplyr::select(-all_of(dep_variables_agg))

plot_mosaic(data_tmp, sector_detail = "sector_name")
plot_mosaic(data_tmp, sector_detail = "category")
plot_mosaic(data_tmp, sector_detail = "category_main")
# plot_mosaic(data_panel %>% dplyr::select(-all_of(dep_variables_agg)), sector_detail = "category_main_panel")

# plot panel -------------------------------------------------------------------
year_filter <- 2002:2018
data_panel <- readRDS("input/data_panel.rds")
data_panel <- data_panel %>% 
  dplyr::filter(year %in% year_filter)

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

plot_stack(data_panel, "sector_name")
plot_stack(data_panel, "category")
plot_stack(data_panel, "category_main")
