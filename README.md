# Spatial econometric STIRPAT model of EU GHG emissions

[![DOI](https://zenodo.org/badge/369532050.svg)](https://zenodo.org/badge/latestdoi/369532050)

This repository accompanies my master's thesis, where I estimate spatial STIRPAT (Stochastic Impacts by Regression on Population, Affluence, and Technology) panel models to examine elasticities of GHG emissions in the EU. The purpose is to explore different levels of aggregation of GHG emissions (across sectors and/or across GHGs). I use the European Commission Joint Research Centre's Emissions Database for Global Atmospheric Research (EDGARv6.0) for CO<sub>2</sub> (from fossil as well as organic sources), CH<sub>4</sub>, and N<sub>2</sub>O emissions disaggregated by IPCC sectors. All other data is retrieved from Eurostat. Data availability limits the analysis to 1092 NUTS 3 regions in 23 EU countries and to the time period 2007-2018.

The repository builds on work colleagues and I did in a Spatial Economics course in our Economics Master at WU Vienna, where we applied Geographically Weighted Regressions (GWR) on 2016 carbon dioxide emissions for EU NUTS 3 regions [github.com/Gutschlhofer/carbon_emissions_EU](https://github.com/Gutschlhofer/carbon_emissions_EU).

## Data

All data needed is automatically downloaded via scripts in the [code](code) folder.

## How to

1. Make sure to have enough storage space (at least 90 GB) available as EDGAR data can take up to 85 GB.
2. Preparation: `install.packages("pacman")` and `remotes::install_github("coolbutuseless/ggpattern")`
3. Run the code in [code/main.R](code/main.R) ([code/00_libraries_functions.R](code/00_libraries_functions.R) automatically installs and loads all necessary packages via `pacman`)
4. (Optional) Have a look at the [code](code) folder, there are additional scripts that are not run by the main.R script.

## Issues

In case you face any problems, feel free to open an issue: [ghg_emissions_eu/issues](https://github.com/Gutschlhofer/ghg_emissions_eu/issues).