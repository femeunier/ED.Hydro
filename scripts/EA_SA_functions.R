################################################################################
# Perform UA and SA

prep_VCD <- function(wf_id, var){
  base_path <- sprintf("/fs/data2/output/PEcAn_%i", wf_id)
  original_xml <- file.path(base_path, "pecan.CONFIGS.xml")
  new_xml <- sprintf("/fs/data2/output/PEcAn_%i/pecan.CONFIGS.NPP.xml", wf_id)
  file.copy(original_xml, new_xml, overwrite = FALSE)
  tx  <- readLines(new_xml)
  
  for(i in seq_along(var)){
    new_file <- paste0("/fs/data2/output/PEcAn_",wf_id,"/pecan.CONFIGS.",var[i],".xml")
    if(!file.exists(new_file)){
      tx2  <- gsub(pattern = "NPP", replace = var[i], x = tx)
      writeLines(tx2, con=new_file)
      settings <- read.settings(new_file)
      runModule.get.results(settings)
      runModule.run.ensemble.analysis(settings, TRUE)    
      runModule.run.sensitivity.analysis(settings)
    }
  }
}

################################################################################
# Prepare VCD data and make plots

plot_VCD <- function(model, var, plot, PDF){
  # namedcolors <- brewer.pal(3, "Set1")
  namedcolors <- c(brewer.pal(3, "Set1")[1:2], "grey40", "black")
  # namedcolors <- c("#c23c81", "#238a8d", "grey40", "black")
  names(namedcolors) <- c("ORIG", "HYDRO", "No_Diff", "Shared")
  
  plot_data <- list()
  PD <- list()
  
  for(i in seq_along(var)){
    
    plot_data[[var[i]]] <- list()
    
    for(j in seq_along(model$wf_id)){
      conf_file <- paste0("/fs/data2/output/PEcAn_",model$wf_id[j],"/pecan.CONFIGS.",var[i],".xml")
      settings <- read.settings(conf_file)
      SA_file <- sprintf("/fs/data2/output/PEcAn_%s/sensitivity.results.%s.%s.%s.%s.Rdata",
                         settings$workflow$id,
                         settings$sensitivity.analysis$ensemble.id,
                         settings$sensitivity.analysis$variable, 
                         settings$sensitivity.analysis$start.year, 
                         settings$sensitivity.analysis$end.year)
      load(SA_file)
      
      pft <- names(sensitivity.results) # Assuming there is only one
      
      plot.inputs <- sensitivity.results[[pft]]$variance.decomposition.output
      
      traits <-  names(plot.inputs$variances)
      units <- as.character(trait.lookup(traits)$units)
      trait.labels <- as.character(trait.lookup(traits)$figid)
      
      plot_data[[var[i]]][[settings$workflow$id]] <- 
        data.frame(
          trait.labels = ifelse(!is.na(trait.labels),
                                trait.labels,
                                traits),
          units = ifelse(!is.na(units), units, ""),
          coef.vars = plot.inputs$coef.vars * 100,
          elasticities = plot.inputs$elasticities,
          variances = plot.inputs$variances,
          wf_id = model$wf_id[j],
          model.type = model$model.type[j]
        )
    }
    plot.data <- do.call(rbind.data.frame, plot_data[[var[i]]])
    plot.data <- plot.data %>% plyr::arrange(variances) %>% 
      mutate(cv_color = model.type, el_color = model.type, 
             pv_color = model.type, label_color = model.type) 
    
    PD[[var[i]]] <- plot.data
    PD[[var[i]]]$var <- var[i]
    PD[[var[i]]]$met.type <- unique(model$met.type)
    
    if(plot){
      
      plot.data <- plot.data %>% mutate(new.labels = case_when(
        trait.labels == "leaf_psi_tlp" ~ "TLP",
        trait.labels == "leaf_water_sat" ~ "theta_s,l",
        trait.labels == "wood_water_sat"~ "theta_s,w",
        trait.labels == "wood_psi50" ~ "p50",
        trait.labels == "wood_Kmax" ~ "Kmax",
        trait.labels == "wood_Kexp" ~ "Kexp",
        trait.labels == "Aboveground Fraction of Structural Biomass" ~ "AGB Allocation",
        trait.labels == "leaf NIR reflectance" ~ "Leaf NIR reflectance", 
        trait.labels == "leaf NIR transmittance" ~ "Leaf NIR transmittance", 
        trait.labels == "leaf VIS reflectance" ~ "Leaf VIS reflectance", 
        trait.labels == "leaf VIS transmittance" ~ "Leaf VIS transmittance",
        TRUE ~ as.character(trait.labels)
      ))
      
      keep.traits <- data.frame(
        new.labels = c(
          "Water Conductance", 
          "Kmax",
          "Kexp",
          "theta_s,l",
          "theta_s,w",
          "p50",
          "TLP",
          
          "Specific Leaf Area",
          "Vcmax",
          
          "AGB Allocation",
          "Fine Root Allocation",
          
          "Leaf orientation",
          "Leaf NIR reflectance", 
          "Leaf NIR transmittance", 
          "Leaf VIS reflectance", 
          "Leaf VIS transmittance",
          
          "Growth Respiration",
          "Veg. Resp. Q10",
          
          "Leaf Turnover Rate",
          "Root Turnover Rate"
        ),
        
        trait.type = c(
          rep("Hydraulic",7), 
          rep("Photo.",2),
          rep("Allocation",2),
          rep("Radiation",5), 
          rep("Resp.",2),
          rep("Turnover",2)
          
        )
      )
      
      plot.data <- left_join(keep.traits, plot.data, by = "new.labels")
      plot.data$new.labels <- factor(plot.data$new.labels, levels = rev(unique(plot.data$new.labels)))
      plot.data <- plot.data[order(plot.data$new.labels),]
      
      points.df = data.frame(new.labels = unique(plot.data$new.labels), 
                             points = seq_along(unique(plot.data$new.labels)))
      
      plot.data <- left_join(plot.data, points.df)
      plot.data <- plot.data %>% group_by(points) %>% arrange(model.type) # %>% select(one_of("points", "model.type")) %>% head() 
      
      plot.data$coef.vars <- log(abs(plot.data$coef.vars))
      
      base.plot <- ggplot(plot.data) + 
        coord_flip() + scale_color_brewer(palette="Set1") + 
        theme_classic() + theme(
          axis.text.y = element_blank(), axis.ticks = element_blank(), 
          axis.line = element_blank(), axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank())
      
      cv.plot <-  base.plot + ggtitle("Coefficient of Variance (log)") + geom_hline(yintercept = 0) + 
        geom_pointrange(aes(x = points, y = coef.vars, ymin = 0, ymax = coef.vars, color = cv_color),
                        alpha = .7, size = 1.25) + 
        scale_y_continuous(labels = function(x) sprintf("%.2e", x)) + 
        xlim(1, max(plot.data$points)) + 
        theme(legend.position="none") + 
        scale_color_manual("cv_color", values = namedcolors)
      
      el.plot <- base.plot + ggtitle("Elasticity") +
        geom_hline(yintercept = 0) +
        geom_pointrange(aes(x = points, y = elasticities, ymin = 0, ymax = elasticities, color = el_color),
                        alpha = .7, size = 1.25) + 
        xlim(1, max(plot.data$points)) + 
        theme(legend.position="none") + 
        scale_color_manual("el_color", values = namedcolors) 
      
      pv.plot <- base.plot + ggtitle("Variance") +
        geom_hline(yintercept = 0) +
        geom_pointrange(aes(x = points, variances, ymin = 0, ymax = variances, color = pv_color),
                        alpha = .7, size = 1.25) + 
        xlim(1, max(plot.data$points)) + 
        scale_color_manual("pv_color", values = namedcolors) + 
        theme(legend.justification=c(1,0), legend.position=c(1,0))
      
      trait.plot <- base.plot + ggtitle("Parameter")  +
        geom_text(aes(y = 1, x = points, label = new.labels, hjust = 1, color = label_color)) +
        scale_y_continuous(breaks = c(0, 0), limits = c(0, 1)) + 
        xlim(0, max(plot.data$points)) + 
        theme(axis.text.x = element_blank()) + 
        scale_color_manual("label_color", values = namedcolors) + 
        theme(legend.position="none")
      
      title1=textGrob(var[i], gp=gpar(fontsize=24, fontface="bold"))
      
      if(PDF){
        fpath <- "/fs/data3/ecowdery/ED_Tropics/figures"
        fname <- paste(c("VDC",unique(model$wf_id),var[i],unique(model$met.type),"png"), collapse = ".")
        # pdf(file.path(fpath,fname), width = 11, height = 8)
        # dev.off()
        ggsave(filename = file.path(fpath,fname), 
               plot = grid.arrange(trait.plot, cv.plot, el.plot, pv.plot, ncol = 4, top = title1), 
               width = 9.7, height = 7, units = "in")
      }else{
        grid.arrange(trait.plot, cv.plot, el.plot, pv.plot, ncol = 4, top = title1)
      }
    }
  }
  PD <- do.call(rbind.data.frame, PD) %>% 
    select(one_of("model.type", "var", "variances", "trait.labels", "wf_id", "met.type"))
  return(PD)
}


################################################################################
addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}
################################################################################



var_heatmap <- function(PD.ALL, var_vector, full_range, color_range, plot.width, plot.height){
  
  PD.ALL <- PD.ALL %>% mutate(new.labels = case_when(
    trait.labels == "leaf_psi_tlp" ~ "TLP",
    trait.labels == "leaf_water_sat" ~ "theta_s,l",
    trait.labels == "wood_water_sat"~ "theta_s,w",
    trait.labels == "wood_psi50" ~ "p50",
    trait.labels == "wood_Kmax" ~ "Kmax",
    trait.labels == "wood_Kexp" ~ "Kexp",
    trait.labels == "Aboveground Fraction of Structural Biomass" ~ "AGB Allocation",
    trait.labels == "leaf NIR reflectance" ~ "Leaf NIR reflectance", 
    trait.labels == "leaf NIR transmittance" ~ "Leaf NIR transmittance", 
    trait.labels == "leaf VIS reflectance" ~ "Leaf VIS reflectance", 
    trait.labels == "leaf VIS transmittance" ~ "Leaf VIS transmittance",
    TRUE ~ as.character(trait.labels)
  ))
  
  
  # Pick the variables
  PD.ALL <- PD.ALL %>% filter(.,var %in% var_vector)
  PD.ALL$var <- as.factor(PD.ALL$var) %>% factor(levels = var_vector)
  
  # Setup grouping of parameters
  
  traits_hydro <- c("Water Conductance", 
                    "Kmax",
                    "Kexp",
                    "theta_s,l",
                    "theta_s,w",
                    "p50",
                    "TLP")
  traits_photo <- c("Specific Leaf Area",
                    "Vcmax")
  traits_alloc <- c("AGB Allocation",
                    "Fine Root Allocation")
  traits_radtn <- c("Leaf orientation",
                    "Leaf NIR reflectance", 
                    "Leaf NIR transmittance", 
                    "Leaf VIS reflectance", 
                    "Leaf VIS transmittance") 
  traits_respr <- c("Growth Respiration",
                    "Veg. Resp. Q10")
  traits_trnov <- c("Leaf Turnover Rate",
                    "Root Turnover Rate")
  
  
  
  keep.traits <- data.frame(
    new.labels = c(
      traits_hydro,
      traits_photo,
      traits_alloc,
      traits_radtn,
      traits_respr,
      traits_trnov
    ),
    
    trait.type = c(
      rep("Hydraulic",7), 
      rep("Photo.",2),
      rep("Allocation",2),
      rep("Radiation",5), 
      rep("Resp.",2),
      rep("Turnover",2)
      
    )
  )
  
  PD.sub <- left_join(keep.traits, PD.ALL, by = "new.labels")
  PD.sub$new.labels <- factor(PD.sub$new.labels, levels = rev(unique(PD.sub$new.labels)))
  PD.sub$model.type <- factor(PD.sub$model.type , levels = c("ORIG", "HYDRO"))
  PD.sub$trait.labels <- factor( PD.sub$trait.type, levels = c(
    "Hydraulics", "Photo.", "Allocation", "Radiation", "Resp.", "Turnover", "Total_hydro", "Total"
  ))
  
  totals <- list()
  
  total_var <- PD.sub %>% dplyr::group_by(model.type, var) %>% dplyr::summarise(total = sum(variances))
  total_var$model.type <- factor(total_var$model.type , levels = c("ORIG", "HYDRO"))
  total_var$trait.type <- "Total"
  totals[["Total"]] <- total_var
  
  total_var
  
  for(t in c("traits_hydro", "traits_photo", "traits_alloc", "traits_radtn", "traits_respr", "traits_trnov")){
    n <- paste0("Total_", strsplit(t, "_") %>% unlist %>% .[2])
    tt <- eval(parse(text = t))
    dat <- PD.sub %>% filter(new.labels %in% tt) %>%
      dplyr::group_by(model.type, var) %>% dplyr::summarise(total = sum(variances))
    dat$model.type <- factor(dat$model.type , levels = c("ORIG", "HYDRO"))
    dat$trait.type <- n
    totals[[n]] <- dat
  }
  tv <- do.call(rbind, totals)
  
  
  trait_colors <- c("red", "blue", "gray70", "gray75", "gray80", "gray85")
  names(trait_colors) <- c( "Total_alloc", "Total_hydro", "Total_photo", 
                            "Total_radtn", "Total_respr", "Total_trnov")
  
  # PD.sub <- rbind.fill(PD.sub, totals[["Total_hydro"]])
  # PD.sub <- rbind.fill(PD.sub, totals[["Total"]])

  ####################
  # Time to plot!
  
  # rescale_vec <- c(min(full_range),1e-20,1e-15,1e-10,1e-11,7e-6,max(full_range))
  # n = 7
  # n <- length(colorvec)
  # rescale_vec <- exp(seq(log(min(PD.ALL$variances)), log(max(PD.ALL$variances)), length.out = n))
  
  # color_range <- c(2.027377e-32, 3.882505e-10, 7.435148e-8, 1.423860e-07 , 2.726747e-6, 5.221826e-2, 1.000000e+00)
  
  rescale_vec <- color_range
  
  base <- ggplot(PD.sub)  + 
    theme_classic() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 16),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 12), 
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.key.height = unit(5, "line"), legend.key.width = unit(2, "line")) 
  # base <- ggplot(PD.sub) +
  #   viridis::scale_fill_viridis(direction = -1, guide = "colorbar",
  #                               limits=c(min(full_range),max(full_range))) +
  #   theme(legend.key.height = unit(7, "line"))
  
  heatmap <- base +
    geom_tile(aes(y= new.labels, x = model.type, fill = variances)) + 
    viridis::scale_fill_viridis(values = rescale(rescale_vec), direction = -1, 
                                limits=c(min(full_range),max(full_range)), na.value="white", 
                                guide = "colorbar", name = "Normailized \nVariance") + 
    # geom_text(aes(y= new.labels, x = model.type, label = sprintf("%.1e",variances))) +
    # ylab("Parameters") +
    # xlab("Version of ED") +  
    # ggtitle("Variance") + 
      facet_grid(factor(trait.type, levels = c(
        "Hydraulic", "Photo.", "Allocation", "Radiation", "Resp.", "Turnover", "Total_hydro", "Total"
      )) ~ var, space = "free", scales = "free")  
    
    # geom_pointrange(aes(x = model.type, y = total, ymin=0, ymax=total),shape ="_", size = 4)
    
  
  gt <- heatmap
  # Below is what I would use if I wanted to add the barplot under the facets
  # gt <- ggplot_gtable(ggplot_build(heatmap)) 
  # # gtable_show_layout(gt)
  # gt$heights[20] = 3*gt$heights[20]
  # gt$heights[22] = 3*gt$heights[22]
  # grid.draw(gt) 
  
  fpath <- "/fs/data3/ecowdery/ED_Tropics/figures/"
  fname <- paste("heatmap", paste(unique(na.omit(PD.sub$wf_id)), collapse = "."), unique(na.omit(PD.sub$met.type)), "png", sep= "." )
  ggsave(file.path(fpath,fname), gt, width = plot.width, height = plot.height)
  
  base2 <- ggplot() + 
    theme_classic() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 16),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 12), 
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)) 
  
  b1 <- base2 + 
    geom_bar(data = tv %>% filter(trait.type == "Total_hydro"), 
             aes(x = model.type, y = total, fill = trait.type), stat = "identity") + 
    scale_fill_manual("trait.type", values = trait_colors) + 
    facet_grid(~ var)
  
  b2 <- base2 +
    geom_bar(data = tv %>% filter(!trait.type == "Total"), 
             aes(x = model.type, y = total, fill = trait.type), stat = "identity") + 
    scale_fill_manual("trait.type", values = trait_colors) + 
    facet_grid(~ var)
  
  gt2 <- grid.arrange(b1,b2, ncol = 1)
  
  fpath <- "/fs/data3/ecowdery/ED_Tropics/figures/"
  fname <- paste("barplot", paste(unique(na.omit(PD.sub$wf_id)), collapse = "."), unique(na.omit(PD.sub$met.type)), "png", sep= "." )
  ggsave(file.path(fpath,fname), gt2, width = plot.width, height = plot.height)
  
  return(gt)
  # hist_all <- ggplot(total_var) + geom_pointrange(data = total_var, aes(x = model.type, y = total, ymin=0, ymax=total),shape ="_", size = 4) + 
  #   # geom_bar(aes(x = model.type, y = total), stat = "identity") +
  #   facet_grid(~var, scale = "free") + scale_y_continuous(trans = "log") + theme(strip.text = element_blank())
  # 
  # p <- grid.arrange(heatmap + theme(axis.text.x = element_blank()), hist_all, 
  #                   layout_matrix = rbind(
  #                     c(1, 1, 1, 1, 1),
  #                     c(1, 1, 1, 1, 1),
  #                     c(1, 1, 1, 1, 1),
  #                     c(1, 1, 1, 1, 1),
  #                     c(1, 1, 1, 1, 1),
  #                     c(1, 1, 1, 1, 1),
  #                     c(2, 2, 2, 2, NA)))
  # 
  
  # fpath <- "/fs/data3/ecowdery/ED_Tropics/figures/"
  # fname <- paste("heatmap", paste(unique(PD.sub$wf_id), collapse = "."), unique(PD.sub$met.type), "pdf", sep= "." )
  # pdf(file.path(fpath,fname), width = 11, height = 12)
  # grid.arrange(heatmap + theme(axis.text.x = element_blank()), hist_all, 
  #              layout_matrix = rbind(
  #                c(1, 1, 1, 1, 1),
  #                c(1, 1, 1, 1, 1),
  #                c(1, 1, 1, 1, 1),
  #                c(1, 1, 1, 1, 1),
  #                c(1, 1, 1, 1, 1),
  #                c(1, 1, 1, 1, 1),
  #                c(2, 2, 2, 2, NA)))
  # dev.off()
  
}

#  regional veg model with emergent ecological properties and explicit biogeophysical cycles


# Setup construct plot

# Attempt at rainbow
# colorvec <- c('#dafbff', '#abdda4',  '#ffffbf', '#fdae61', '#d7191c')
# PANK (https://www.invisionapp.com/inside-design/finding-the-right-color-palettes-for-data-visualizations/)
colorvec <- paste0("#", c("f5aea1", "ea628a", "a83890", "a83890", "3d1058"))
# BLUE (https://www.invisionapp.com/inside-design/finding-the-right-color-palettes-for-data-visualizations/)
colorvec <- paste0("#", c("b6decb","b6decb", "3683b8", "244794", "151c65"))
colorvec <- c(paste0("#", c("dcecc8","75c6d0", "3893c1", "26539b", "11154c")))
# GREEN (http://tristen.ca/hcl-picker/#/hlc/5/0.79/263039/D4F68F)
colorvec <- rev(unlist(strsplit("#263039,#365F62,#4F927F,#83C68B,#D4F68F", ",")))


# n = 7
# n <- length(colorvec)
# rescale_vec <- exp(seq(log(min(PD.ALL$variances)), log(max(PD.ALL$variances)), length.out = n))
# base <- ggplot(PD.sub) + 
#   scale_fill_gradientn(colors = colorvec, 
#                        values = rescale(rescale_vec),
#                        guide = "colorbar", limits=c(0,max(PD.ALL$variances))) + 
#   theme(legend.key.height = unit(6, "line")) 


# ggsave("/fs/data3/ecowdery/ED_Tropics/figures/heatmap_water.png", 
#        plot = last_plot(), width = 11, height = 12, units = "in")




# NPP_hist <- ggplot(total_var %>% filter(var == "NPP")) + 
#   geom_bar(aes(x = model.type, y = total), stat = "identity")
# 
# GPP_hist <- ggplot(total_var %>% filter(var == "GPP")) + 
#   geom_bar(aes(x = model.type, y = total), stat = "identity")
# 
# TVEg_hist <- ggplot(total_var %>% filter(var == "TVeg")) + 
#   geom_bar(aes(x = model.type, y = total), stat = "identity")
# 
# SoilMoist_hist <- ggplot(total_var %>% filter(var == "SoilMoist")) + 
#   geom_bar(aes(x = model.type, y = total), stat = "identity")

# grid.arrange(NPP_hist, GPP_hist, TVEg_hist, SoilMoist_hist, ncol = 4)



# ggsave("/fs/data3/ecowdery/ED_Tropics/figures/heatmap_dought_bar.png", 
#        plot = p, width = 11, height = 12, units = "in")












################################################################################
# Experimenting with heatmaps, makes a cv, pv and var heat map

# base <- ggplot(plot.data) +
#   scale_fill_distiller(palette = "Spectral", direction = -1) +
#   theme_bw() + 
#   scale_x_discrete(labels = c("ORIG", "HYDRO"), breaks = as.factor(model$wf_id))
# 
# cv_heat <- base +
#   geom_tile(aes(y= trait.labels, x = as.factor(wf_id), fill = coef.vars)) + 
#   ylab("Parameters") +
#   xlab("Workflow ID") +  
#   ggtitle("CV %") 
# 
# pv_heat <- base + 
#   geom_tile(aes(y= trait.labels, x = as.factor(wf_id), fill = variances)) + 
#   ylab("Parameters") +
#   xlab("Workflow ID") +  
#   ggtitle("Partial Variance")
# 
# el_heat <- base + 
#   geom_tile(aes(y= trait.labels, x = as.factor(wf_id), fill = elasticities)) + 
#   ylab("Parameters") +
#   xlab("Workflow ID") +  
#   ggtitle("Elasticity")

# trait.plot <- base + ggtitle("Parameter")  +
#   geom_text(aes(y = 1, x = points, label = trait.labels, hjust = 1, color = label_color)) +
#   scale_y_continuous(breaks = c(0, 0), limits = c(0, 1)) + 
#   xlim(0, max(plot.data$points)) + 
#   theme(axis.text.x = element_blank()) + 
#   scale_color_manual("label_color", values = namedcolors) + 
#   theme(legend.position="none")

# grid.arrange(cv_heat, el_heat, pv_heat, nrow = 1)