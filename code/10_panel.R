# setup ------------------------------------------------------------------------
year_filter <- 2002:2018

# get data ---------------------------------------------------------------------
# data_panel <- readRDS("input/data_panel.rds")
data_panel <- data_panel %>% 
  dplyr::filter(year %in% year_filter)

summary(data_panel)

gg_miss_var(data_panel, show_pct = TRUE)
# notes:
# gva:share has much better coverage than emp_share

# Visualise panel --------------------------------------------------------------
# ensure that all plots have the same theme
theme_set(theme_minimal())

# save plots under
plot_path <- "output/plots/"
plot_template <- paste0("raw_%s",".png")


plot_stack <- function(data, sector_detail = c("sector_name", "category", "category_main")) {
  
  sectors_to_show <- 5
  
  data_m <- data %>% 
    st_drop_geometry() %>% 
    dplyr::select(nuts3_id, cntr_code, year, 
                  starts_with("edgar_"),
                  -starts_with("edgar_GHG"),
                  # small caps is only totals
                  -starts_with("edgar_co2", ignore.case = FALSE),
                  -edgar_n2o, -edgar_ch4
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
    dplyr::select(-sector) %>% 
    dplyr::rename(sector = all_of(sector_detail))
  
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
  
  # we only do this once
  if(sector_detail == "sector_name") {
    
    plot <- data_m %>%  
      dplyr::group_by(ghg, year) %>% 
      dplyr::summarise(value = sum(value)) %>% 
      ggplot(aes(x=year, y=value, fill=ghg)) +
      geom_area() +
      scale_fill_viridis_d()
    
    if(s) ggsave(plot, path = plot_path, filename = sprintf(plot_template, paste0("ghg_panel_stack")), width = 8, height = 5)
    
  }
  
  plot <- data_m %>% 
    dplyr::filter(value != 0) %>% 
    dplyr::group_by(sector, year) %>% 
    dplyr::summarise(value = sum(value)) %>% 
    ggplot(aes(x=year, y=value, fill=sector)) +
    geom_area() +
    scale_fill_viridis_d()
  
  if(s) ggsave(plot, path = plot_path, filename = sprintf(plot_template, paste0("sector_panel_stack_", sector_detail)), width = ifelse(sector_detail == "category", 12, 8), height = 5)
}

plot_stack(data_panel, "sector_name")
plot_stack(data_panel, "category")
plot_stack(data_panel, "category_main")


missing_gdp <- data_panel %>% 
  dplyr::filter(is.na(gdp))

ggplot(data = missing_gdp %>% 
         dplyr::group_by(nuts3_id) %>% 
         dplyr::summarise(count = length(unique(year)))) + 
  geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                  pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                  pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  geom_sf(aes(fill = count), color = "white", size=0.01) +
  scale_fill_viridis_c(option = "magma", direction = -1) +  
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))

missing_gdp %>% pull(cntr_code) %>% unique()

# perform some calculations
data_panel <- data_panel %>% 
  dplyr::mutate(
    density = (pop/area)/1000, # pop per m2, density in 1000people/km2
    heating_or_cooling = hdd + cdd,
    cdd_fix = cdd+1, # move our scale by 1 to be able to log it
    cdd_log = ifelse(cdd == 0, 0, log(cdd))
  )

# exclude so we have gdp, population and GVA share BE
exclude <- c("NO","CH", "TR", "RS", "IE", "UK", "LI", "MT")
data_panel <- data_panel[!data_panel$cntr_code%in%exclude,]
# exclude so we have CDD,HDD
exclude <- c("AL","MK","ME")
data_panel <- data_panel[!data_panel$cntr_code%in%exclude,]

# test <- data_panel %>% filter(gva_share_F %>% is.na) # it's Sweden
data_panel <- data_panel %>% dplyr::filter(cntr_code != "SE")

# this filters out regions in france that also don't have GDP
data_panel <- data_panel %>% dplyr::filter(!is.na(gva_share_BE))

summary(data_panel)

not_all_years <- data_panel %>%
  st_drop_geometry %>% 
  dplyr::group_by(nuts3_id) %>% 
  dplyr::summarise(n_row = length(unique(year))) %>% 
  dplyr::arrange(n_row) %>% 
  dplyr::filter(n_row != max(n_row)) %>% 
  dplyr::pull(nuts3_id)

data_panel <- data_panel %>% 
  dplyr::filter(!(nuts3_id %in% not_all_years))

data_one_year <- data_panel %>% dplyr::filter(year == 2016)
no_neigh <- c(505:506,521,539,564:565,597)
data_one_year <- data_one_year[-no_neigh,]
data_panel <- data_panel %>% dplyr::filter(nuts3_id %in% data_one_year$nuts3_id)

# create W matrix for 
lw_queen <- poly2nb(data_one_year, queen = TRUE) %>% 
  nb2listw()
lw_spatial <- lw_queen

data_panel$year <- as.factor(data_panel$year)
data_panel$nuts3_id <- as.factor(data_panel$nuts3_id)

panel_sac <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial,
                        model = "random",
                        # effect = "twoway",
                        lag = TRUE, spatial.error = "kkp")

panel_sac
summary(panel_sac)

panel_sem <- splm::spml(model_base, data_panel, index = c("nuts3_id", "year"), listw = lw_spatial,
                        lag = FALSE, spatial.error = "kkp")

panel_sem
summary(panel_sem)

