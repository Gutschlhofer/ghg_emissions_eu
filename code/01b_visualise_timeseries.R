# some preliminary visualisations to check consistency between aggregate data (data_edgar) and sectoral data (data_edgar_all)
# before running this, run the data_edgar code in 04_combine_data.R
data_aggregates <- data_edgar %>%
  dplyr::select(year, edgar_n2o, edgar_co2, edgar_co2_short, edgar_ch4) %>%
  # st_drop_geometry %>%
  pivot_longer(cols = starts_with("edgar")) %>%
  dplyr::mutate(name = as.factor(name),
                year = as.numeric(year)) %>%
  dplyr::group_by(name, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::rename(ghg = name)
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
  dplyr::rename(ghg = name)
plot_aggregates <- data_aggregates %>%
  # filter(ghg %in% c("edgar_co2")) %>% 
  ggplot(aes(x=year, y=value, fill=ghg)) +
  geom_area() +
  scale_fill_viridis_d(); plot_aggregates
ggsave(plot_aggregates, path = "output/plots/", filename = "raw_ghg_panel_stack_aggregates.png", scale=1, width = 5, height = 5)
plot_sectors <- data_sectors %>%
  # filter(ghg %in% c("CO2_l")) %>%
  ggplot(aes(x=year, y=value, fill=ghg)) +
  geom_area() +
  scale_fill_viridis_d(); plot_sectors
ggsave(plot_sectors, path = "output/plots/", filename = "raw_ghg_panel_stack.png", scale=1, width = 5, height = 5)