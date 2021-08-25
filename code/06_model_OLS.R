# load data
# data <- readRDS("input/data.rds")

run_ols <- function(dep_var, data, data_nuts2, ghg_aggregate_over_sector) {
  library(tidyverse)
  library(sf)
  
  # Base model -------------------------------------------------------------------
  dep_variable <- sprintf("log(%s)", dep_var) #"log(edgar)"
  
  base_variables <- c(
    "log(pop)",
    "log(pop_share_Y15_64)",
    # "log(pop_share_Y20_34)",
    # "log(pop_share_Y35_49)",
    # "log(pop_share_Y50_64)",
    "log(pop_share_Y_GE65)",
    "log(density)",
    "log_gdppc",
    "I(log_gdppc^2)",
    "log(gva_share_A)",
    "log(gva_share_BE)",
    "log(gva_share_F)",
    "log(gva_share_GJ)",
    "log(hdd)",
    "log(cdd_fix)",
    "log(REN)",
    "urbn_type",
    "coast_type" #,
    # "mount_type"
  )
  
  model_base <- as.formula(paste(dep_variable, "~", paste(base_variables, collapse= "+")))
  
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
  
  if(nrow(data) == 0) return()
  
  ols_base <- lm(model_base, data)
  summary(ols_base)
  
  saveRDS(ols_base, file = sprintf("./output/regressions/OLS_%s.rds", dep_var))
  
  # if(dep_var %in% ghg_aggregate_over_sector) {
  #   # check for MAUP
  #   ols_maup <- lm(as.formula(paste(dep_variable, "~", paste(base_variables[!(base_variables %in% c("urbn_type",
  #                                                                                                   "coast_type",
  #                                                                                                   "mount_type"))], collapse= "+"))), 
  #                  data_nuts2)
  #   output <- summary(ols_maup)
  #   saveRDS(ols_base, file = sprintf("./output/regressions/OLS_%s_maup.rds", dep_var))
  #   cat(paste(output), file = sprintf("./output/tables/OLS_%s_maup.txt", dep_var), sep = "\n")
  # }
  
  # Model with country dummies ---------------------------------------------------
  model_cntr <- as.formula(paste(dep_variable, "~", paste(base_variables[base_variables!="log(REN)"], collapse= "+"), " + cntr_code"))
  ols_cntr <- lm(model_cntr, data)
  summary(ols_cntr)
  saveRDS(ols_cntr, file = sprintf("./output/regressions/OLS_%s_cntr.rds", dep_var))
  
  model_base_no_density <- as.formula(paste(dep_variable, "~", paste(base_variables[base_variables!="log(density)"], collapse= "+")))
  ols_no_density <- lm(model_base_no_density, data)
  summary(ols_no_density)
  saveRDS(ols_no_density, file = sprintf("./output/regressions/OLS_%s_no_density.rds", dep_var))
  
  # OLS TABLE
  # Output for Presentation (fontsize = tiny):
  ################################################################################
  output <- stargazer::stargazer(ols_base, ols_cntr, digits=2, # type = "text", 
                                 single.row = TRUE, no.space = TRUE,
                                 dep.var.labels = dep_var,
                                 # covariate.labels = c("Population", "Density", "GDP/cap", "GDP/cap, squared",
                                 #                      "GVA", "HDD", "CDD"),
                                 column.sep.width = "3pt", font.size = "tiny",
                                 title = sprintf("OLS Regression Results for %s", dep_var),
                                 out = sprintf("output/tables/OLS_%s.tex", dep_var)) # flip = TRUE ?
  
  # filter out all entries containing country dummies
  output <- output[!grepl("cntr",output)]
  constant_line_nr <- grep("Constant",output)
  dummy_line <- "  Country Dummies & No & Yes \\\\" # 4 backslashes makes 2
  
  # put output together
  output <- c(output[1:constant_line_nr],
              dummy_line,
              output[(constant_line_nr+1):length(output)])
  
  # save output
  cat(output, file = sprintf("./output/tables/OLS_%s_dummyshort.tex", dep_var), sep = "\n")
  
  # Output for Presentation (fontsize = footnotesize):
  ################################################################################
  output <- stargazer::stargazer(ols_base, ols_cntr, digits=2, # type = "text",
                                 single.row = TRUE, no.space = TRUE,
                                 dep.var.labels = dep_var,
                                 # covariate.labels = c("Population", "Density", "GDP/cap", "GDP/cap, squared",
                                 #                      "GVA", "HDD", "CDD"),
                                 column.sep.width = "3pt", font.size = "footnotesize",
                                 title = sprintf("OLS Regression Results for %s", dep_var),
                                 out = sprintf("output/tables/OLS_%s_footnotesize.tex", dep_var)) # flip = TRUE ?
  
  # filter out all entries containing country dummies
  output <- output[!grepl("cntr",output)]
  constant_line_nr <- grep("Constant",output)
  dummy_line <- "  Country Dummies & No & Yes \\\\" # 4 backslashes makes 2
  
  # put output together
  output <- c(output[1:constant_line_nr],
              dummy_line,
              output[(constant_line_nr+1):length(output)])
  
  # save output
  cat(output, file = sprintf("./output/tables/OLS_%s_dummyshort_paper.tex", dep_var), sep = "\n")
  
}

# dep_variables2 <- dep_variables[(6+19):109]
# dep_variables2 <- dep_variables[grepl("edgar_co2", tolower(dep_variables))]
dep_variables2 <- dep_variables
# dep_variables2 <- ghg_aggregate_over_sector
# dep_variables2 <- "edgar_CO2_long_ENE"
# dep_variables2 <- "edgar_co2"

# lapply(dep_variables2, run_ols, data = data, data_nuts2 = data_nuts2, ghg_aggregate_over_sector = ghg_aggregate_over_sector)
# 
# for(dep_var in dep_variables2) {
#   run_ols(dep_var, data, data_nuts2, ghg_aggregate_over_sector)
# }

cl <- makeCluster(parallel::detectCores())
parLapply(cl = cl, dep_variables2, run_ols, data = data, data_nuts2 = data_nuts2, ghg_aggregate_over_sector = ghg_aggregate_over_sector)
stopCluster(cl)

readRDS("output/regressions/OLS_edgar_co2.rds")->test
summary(test)

readRDS("output/regressions/OLS_edgar_CO2_long_NMM.rds")->test
summary(test)

get_coef_from_file <- function(file_rds, all_coef_names) {
  ols <- readRDS(file_rds)
  coefs <- ols %>% coef
  
  # in case we are missing one or more coefs, add them as NAs here
  coefs[all_coef_names[!all_coef_names %in% names(coefs)]] <- NA
  
  nobs <- nobs(ols)
  coefs["nobs"] <- nobs
  coefs[c(all_coef_names,"nobs")]
  return(coefs)
}

file_rds <- list.files("output/regressions", pattern = "OLS_edgar*", full.names = T)
files_regressions_no_density <- list.files("output/regressions", pattern = "_no_density.rds", full.names = T)
all_coef_names <- readRDS(file_rds[1]) %>% coef %>% names

regressions_no_density <- lapply(files_regressions_no_density, get_coef_from_file, all_coef_names = all_coef_names)
regressions_all <- lapply(file_rds, get_coef_from_file, all_coef_names = all_coef_names)

coef_names <- regressions_no_density[[1]] %>% names

short_names <- list.files("output/regressions", pattern = "_no_density.rds", full.names = F) %>% 
  # str_replace("OLS_edgar_", "") %>% 
  str_replace(".rds", "")

df <- data.frame(matrix(unlist(regressions_no_density), nrow=length(regressions_no_density[[1]]), byrow=FALSE))

rownames(df) <- coef_names
colnames(df) <- short_names

df <- t(df)

df_tmp <- df
df <- df[df[,"nobs"] == 1092,]

summary(df[,"log_gdppc"])
summary(df[,"log(pop)"])
summary(df[,"nobs"])

summary(df)

# ------------------
# extract data for GHG agg sectors

file_rds <- list.files("output/regressions", pattern = "OLS_edgar_agg_GHG*", full.names = T)
files_regressions_base <- file_rds[!(file_rds %>% grepl("_no_density.rds", .) | 
                                       file_rds %>% grepl("_cntr.rds", .))]

files_regressions_no_density <- file_rds[file_rds %>% grepl("_no_density.rds", .)]
all_coef_names <- readRDS(file_rds[1]) %>% coef %>% names

regressions_no_density <- lapply(files_regressions_no_density, get_coef_from_file, all_coef_names = all_coef_names)
regressions_all <- lapply(file_rds, get_coef_from_file, all_coef_names = all_coef_names)
regressions_base <- lapply(files_regressions_base, get_coef_from_file, all_coef_names = all_coef_names)

coef_names <- regressions_no_density[[1]] %>% names

short_names <- files_regressions_no_density %>% 
  str_replace("output/regressions/OLS_edgar_agg_GHG_", "") %>%
  str_replace("_no_density", "") %>% 
  str_replace(".rds", "")

df <- data.frame(matrix(unlist(regressions_no_density), nrow=length(regressions_no_density[[1]]), byrow=FALSE))
rownames(df) <- coef_names
colnames(df) <- short_names

df <- t(df)
df

# ------------------
# extract data for agg sectors

file_rds <- list.files("output/regressions", pattern = "OLS_edgar_agg_*", full.names = T)
files_regressions_base <- file_rds[!(file_rds %>% grepl("_no_density.rds", .) | 
                                       file_rds %>% grepl("_cntr.rds", .))]

files_regressions_no_density <- file_rds[file_rds %>% grepl("_no_density.rds", .)]
all_coef_names <- readRDS(file_rds[1]) %>% coef %>% names

regressions_no_density <- lapply(files_regressions_no_density, get_coef_from_file, all_coef_names = all_coef_names)
regressions_all <- lapply(file_rds, get_coef_from_file, all_coef_names = all_coef_names)
regressions_base <- lapply(files_regressions_base, get_coef_from_file, all_coef_names = all_coef_names)

coef_names <- regressions_no_density[[1]] %>% names

short_names <- files_regressions_no_density %>% 
  str_replace("output/regressions/OLS_edgar_agg_", "") %>%
  str_replace("_no_density", "") %>% 
  str_replace(".rds", "")

df <- data.frame(matrix(unlist(regressions_no_density), nrow=length(regressions_no_density[[1]]), byrow=FALSE))
rownames(df) <- coef_names
colnames(df) <- short_names

df <- t(df)
summary(df)

# readRDS(files_regressions_base[1])->test; summary(test)
# 
# readRDS("output/regressions/OLS_edgar.rds")->test; summary(test)


