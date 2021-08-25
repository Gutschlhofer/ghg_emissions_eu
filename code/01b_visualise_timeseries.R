# some preliminary visualisations to check consistency between aggregate data (data_edgar) and sectoral data (data_edgar_all)
# ensure that all plots have the same theme
theme_set(theme_minimal())

# before running this, run the data_edgar code in 04_combine_data.R
data_aggregates <- data_edgar %>%
  dplyr::select(year, edgar_n2o, edgar_co2, edgar_co2_short, edgar_ch4) %>%
  # st_drop_geometry %>%
  pivot_longer(cols = starts_with("edgar")) %>%
  dplyr::mutate(name = as.factor(name),
                year = as.numeric(year)) %>%
  dplyr::group_by(name, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::rename(GHG = name)
data_sectors <- data_edgar %>%
  dplyr::select(year, starts_with("edgar_")) %>%
  dplyr::select(-edgar_n2o, -edgar_co2, -edgar_co2_short, -edgar_ch4, -edgar_co2_total) %>%
  dplyr::select(-starts_with("edgar_GHG")) %>%
  # st_drop_geometry %>%
  pivot_longer(cols = starts_with("edgar")) %>%
  dplyr::mutate(name = get_ghg_name_from_indicator(name)) %>%
  dplyr::mutate(name = as.factor(name),
                year = as.numeric(year)) %>%
  dplyr::group_by(name, year) %>%
  dplyr::summarise(value = sum(value, na.rm=T)) %>%
  dplyr::rename(GHG = name)
plot_aggregates <- data_aggregates %>%
  # filter(ghg %in% c("edgar_co2")) %>% 
  ggplot(aes(x=year, y=value, fill=GHG)) +
  geom_area() +
  scale_fill_viridis_d()
plot_aggregates
ggsave(plot_aggregates, path = "output/plots/", filename = "raw_ghg_panel_stack_aggregates.png", scale=1, width = 8, height = 5)

plot_sectors <- data_sectors %>%
  dplyr::mutate(value = value / 1e9) %>% # transform to billion tonnes co2 equiv
  ggplot(aes(x=year, y=value, fill=GHG)) +
  geom_area() +
  # add subscripts to legend
  scale_fill_viridis_d(labels=c(CO2_l=expression(paste(CO[2]*l)),
                                CO2_s=expression(paste(CO[2]*s)),
                                CH4=expression(paste(CH[4])),
                                N2O=expression(~N[2]*O))) +
  ylab(bquote('billion tonnes ' ~CO[2]* '-eq/year')) +
  xlab("") +
  theme(legend.text.align = 0)

plot_sectors
ggsave(plot_sectors, path = "output/plots/", filename = "raw_ghg_panel_stack.png", scale=1, width = 8, height = 5)
