# 1. Prepare data for GWR ------------------------------------------------------

s <- TRUE # save plots?

# choose one of the methods
# either set adapt manually (no gwr.sel is run or via optimisation (NULL))
run_gwr <- function(data,
                    method = c("bisq","gauss","tricube"),
                    adapt = NULL,
                    file_name_add = "",
                    model = model_base,
                    cv = FALSE # gwr.sel method = cv (cv = TRUE) or aic (cv = FALSE)
                    ) {
  
  data_coords <- st_coordinates(st_centroid(data$geometry))
  
  # make spatial data frame
  spdf <- SpatialPointsDataFrame(data_coords, 
                                 st_drop_geometry(data), 
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  # 2. bandwidth function, run GWR -----------------------------------------------
  bw_fct <- method

  if(bw_fct == "bisq"){
    # find best neighbour specification
    # select bandwidth, here Videras (2014) uses >>gwr.bisquare<<
    if(is.null(adapt)) {
      bw <- gwr.sel(model, data = spdf,# coords = data_coords, 
                    gweight = gwr.bisquare, longlat = TRUE, verbose = FALSE, adapt = TRUE
                    ,method = ifelse(cv, "cv", "aic"))
    } else {
      bw <- adapt
    }
    
    gwr <- gwr(model, data = spdf,
               adapt = bw, gweight = gwr.bisquare,
               longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
  } else if(bw_fct == "gauss"){
    if(is.null(adapt)) {
      bw <- gwr.sel(model, data = spdf,# coords = data_coords, 
                    gweight = gwr.Gauss, longlat = TRUE, verbose = FALSE, adapt = TRUE
                    ,method = ifelse(cv, "cv", "aic"))
    } else {
      bw <- adapt
    }
    
    gwr <- gwr(model, data = spdf,
               adapt = bw, gweight = gwr.Gauss,
               longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
  } else { # tricube
    if(is.null(adapt)) {
      bw <- gwr.sel(model, data = spdf,# coords = data_coords, 
                    gweight = gwr.tricube, longlat = TRUE, verbose = FALSE, adapt = TRUE
                    ,method = ifelse(cv, "cv", "aic"))
    } else {
      bw <- adapt
    }
    
    gwr <- gwr(model, data = spdf,
               adapt = bw, gweight = gwr.tricube,
               longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
  }
  
  print(paste("adapt Bandwidth:",bw))
  
  # 3. Calculate p-values for estimators -----------------------------------------
  
  gwr_output <- gwr$SDF %>% as.data.frame()
  
  vars <- names(gwr_output)[substr(names(gwr_output), nchar(names(gwr_output))-2, nchar(names(gwr_output))) == "_se"]
  
  p_vec <- c()
  sig_vec <- c()
  
  for(v in vars){
    se <- v
    x <- substr(v, 1, nchar(v)-3)
    p <- paste0(x, "_p")
    p_vec <- c(p_vec, p)
    
    n <- round(gwr$adapt * nrow(data))
    k <- 8 # 7 x + 1 intercept
    
    t.stat<-(gwr_output[,x]-0)/gwr_output[,se] # calculate t-statistic (substract H0 beta)
    pval.t<-2*pt(-abs(t.stat), df=(n-k))       # calculate p-value (assume normal residuals)
    ## error: actually n-k df
    # print(pval.t)
    gwr_output[,p] <- pval.t
    
    gwr_output[,paste0(p,"_sig")] <- ifelse(gwr_output[,x] > 0 & pval.t < 0.05, "pos, p < 0.05", 
                                            ifelse(gwr_output[,x] < 0 & pval.t < 0.05, "neg, p < 0.05",
                                                   "n.sig"))
  }
  
  sig_vec <- paste0(p_vec, "_sig")
  
  coef <- st_as_sf(gwr$SDF)
  summary(coef)
  
  coef$id <- data$nuts3_id
  st_crs(coef) <- st_crs(data)
  
  data_coef <- cbind(data,st_drop_geometry(coef),gwr_output[,all_of(c(p_vec, sig_vec))])
  
  turning_point <- exp(-data_coef$log_gdppc/(2*data_coef$I.log_gdppc.2.))
  
  cat(summary(turning_point), file = paste0("output/tables/turning_point",file_name_add,".txt"))
  
  # save the files
  file_name_gwr <- paste0("input/gwr",file_name_add, ".rds")
  file_name <- paste0("input/data_coef",file_name_add, ".rds")
  saveRDS(gwr, file_name_gwr)
  saveRDS(data_coef, file_name)
  # 
  # # 4. Visualise -----------------------------------------------------------------
  # 
  # plot_path <- "output/plots/"
  # plot_template <- paste0("gwr_%s",file_name_add,".png")
  # 
  # theme_set(theme_minimal())
  # 
  # # log.pop
  # ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = log.pop.), color = "white", size=0.01) + 
  #   scale_fill_viridis_c(option = "magma", direction = -1) +  
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_Pop"), width = 4, height = 5)
  # 
  # # log.gdppc.
  # gwr_gdp <- ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = log_gdppc), color = "white", size=0.01) + 
  #   scale_fill_viridis_c(option = "magma", direction = -1) +  
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_GDPpc"), width = 4, height = 5)
  # 
  # # I.log.gdppc..2.
  # gwr_gdp2 <- ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = I.log_gdppc.2.), color = "white", size=0.01) + 
  #   scale_fill_viridis_c(option = "magma", direction = -1) +  
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_GDPpc2"), width = 4, height = 5)
  # 
  # GDP_GDP2 <- plot_grid(gwr_gdp, gwr_gdp2, ncol = 2)
  # if(s) ggsave(plot = GDP_GDP2, path = plot_path, filename = sprintf(plot_template, "log_GDPpc_GDPpc2"), width = 10, height = 6) 
  # 
  # # log.density.
  # if(!is.null(data_coef$log.density.)){
  #   ggplot(data = data_coef) +
  #     geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                     pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                     pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #     geom_sf(aes(fill = log.density.), color = "white", size=0.01) + 
  #     scale_fill_viridis_c(option = "magma", direction = -1) +  
  #     theme(legend.title = element_blank()) +
  #     geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  #   if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_Density"), width = 4, height = 5)
  # }
  # 
  # # gva_share_BE
  # ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = log.gva_share_BE.), color = "white", size=0.01) + 
  #   scale_fill_viridis_c(option = "magma", direction = -1) +  
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "GVA_share_BE"), width = 4, height = 5)
  # 
  # # log.hdd.
  # ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = log.hdd.), color = "white", size=0.01) + 
  #   scale_fill_viridis_c(option = "magma", direction = -1) +  
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_HDD"), width = 4, height = 5)
  # 
  # # log.cdd_fix.
  # ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = log.cdd_fix.), color = "white", size=0.01) + 
  #   scale_fill_viridis_c(option = "magma", direction = -1) +  
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_CDD_fix"), width = 4, height = 5)
  # 
  # # localR2
  # ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = localR2), color = "white", size=0.01) + 
  #   scale_fill_viridis_c(option = "magma", direction = -1, labels = scales::percent_format(accuracy = 1)) +  
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "local_R2"), width = 4, height = 5)
  # 
  # 
  # # significance plots -----------------------------------------------------------
  # plot_template <- paste0("gwr_%s",file_name_add,"_signif.png")
  # 
  # # log.pop._p_sig
  # ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = log.pop._p_sig), color = "white", size=0.01) + 
  #   scale_fill_manual(values = rev(magma(4)[2:4])) +
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_Pop"), width = 4, height = 5)
  # 
  # # log.density._p_sig
  # if(!is.null(data_coef$log.density._p_sig)){
  #   ggplot(data = data_coef) +
  #     geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                     pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                     pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #     geom_sf(aes(fill = log.density._p_sig), color = "white", size=0.01) + 
  #     scale_fill_manual(values = rev(magma(4)[2:4])) +
  #     theme(legend.title = element_blank()) +
  #     geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  #   if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_Density"), width = 4, height = 5)
  # }
  #   
  # # log.gdppc._p_sig
  # ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = log_gdppc_p_sig), color = "white", size=0.01) + 
  #   scale_fill_manual(values = rev(magma(4)[2:4])) +
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_GDPpc"), width = 4, height = 5)
  # 
  # # I.log.gdppc..2._p_sig
  # ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = I.log_gdppc.2._p_sig), color = "white", size=0.01) + 
  #   scale_fill_manual(values = rev(magma(4)[2:4])) +
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_GDPpc2"), width = 4, height = 5)
  # 
  # # log.gva_share_BE._p_sig
  # ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = log.gva_share_BE._p_sig), color = "white", size=0.01) + 
  #   scale_fill_manual(values = rev(magma(4)[2:4])) +
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "GVA_share"), width = 4, height = 5)
  # 
  # # log.hdd._p_sig
  # ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = log.hdd._p_sig), color = "white", size=0.01) + 
  #   scale_fill_manual(values = rev(magma(4)[2:4])) +
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_HDD"), width = 4, height = 5)
  # 
  # # log.cdd_fix._p_sig
  # ggplot(data = data_coef) +
  #   geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
  #                   pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
  #                   pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
  #   geom_sf(aes(fill = log.cdd_fix._p_sig), color = "white", size=0.01) + 
  #   scale_fill_manual(values = rev(magma(4)[2:4])) +
  #   theme(legend.title = element_blank()) +
  #   geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  # if(s) ggsave(path = plot_path, filename = sprintf(plot_template, "log_CDD_fix"), width = 4, height = 5)
  # 
  # 
  # # 5. Tests ---------------------------------------------------------------------
  # 
  # ### knn
  # k <- round(bw * nrow(data)) # use amount of neighbours determined by gwr_sel
  # lw_knn <- knearneigh(data_coords, k=k) %>% 
  #   knn2nb()
  # 
  # ### inverse distance (based on knn)
  # dlist <- nbdists(lw_knn, data_coords, longlat = TRUE)
  # idlist <- lapply(dlist, function(x) 1/x)
  # lw_inversedist <- nb2listw(lw_knn, glist=idlist, style="W")
  # m <- listw2mat(lw_inversedist)
  # 
  # ### inverse distance (for all)
  # lw_d <- dnearneigh(data_coords, 0, Inf, longlat = TRUE)
  # dlist <- nbdists(lw_d, data_coords, longlat = TRUE)
  # idlist <- lapply(dlist, function(x) 1/x)
  # lw_inversedist_all <- nb2listw(lw_d, glist=idlist, style="W")
  # m_all <- listw2mat(lw_inversedist_all)
  # 
  # ### convert all to lw objects
  # lw_knn <- lw_knn %>% nb2listw()
  # lw_d <- lw_d %>% nb2listw()
  # 
  # moran_knn <- gwr.morantest(gwr, lw = lw_knn)
  # 
  # moran_id <- gwr.morantest(gwr, lw = lw_inversedist)
  # 
  # moran_id_all <- gwr.morantest(gwr, lw = lw_inversedist_all)
  # 
  # print(gwr)
  # print(moran_knn)
  # print(moran_id)
  # print(moran_id_all)
}

## After running through it the first time, you can put in the adapt value with adapt= to save resources
run_gwr(data, method = "bisq", adapt = NULL)
run_gwr(data, method = "bisq", adapt = .1, file_name_add = "_lessneighbours")
run_gwr(data, method = "bisq", adapt = .3, file_name_add = "_moreneighbours")
run_gwr(data, method = "bisq", adapt = NULL, file_name_add = "_cv", cv = TRUE)

# Gauss
run_gwr(data, method = "gauss", adapt = NULL, file_name_add = "_gauss")
run_gwr(data, method = "gauss", adapt = 0.2, file_name_add = "_gauss_20pct")
run_gwr(data, method = "gauss", adapt = NULL, file_name_add = "_gauss_cv", cv = TRUE)

# treat outliers
run_gwr(data_fix_outlier, method = "bisq", adapt = NULL, file_name_add = "_fixoutlier")
run_gwr(data_filter_outlier, method = "bisq", adapt = NULL, file_name_add = "_filteroutlier")

# try without density
run_gwr(data, method = "bisq", model = model_base_no_density, adapt = NULL, file_name_add = "_nodensity")
run_gwr(data_fix_outlier, method = "bisq", model = model_base_no_density, adapt = NULL, file_name_add = "_nodensity_fixoutlier")

# # try NUTS2 (MAUP)
# run_gwr(data_nuts2, method = "bisq", adapt = NULL, file_name_add = "_nuts2")
