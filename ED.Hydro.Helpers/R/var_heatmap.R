#' Variance Heatmap
#'
#' @param PD.ALL
#' @param var_vector
#' @param full_range
#' @param color_range
#' @param plot.width
#' @param plot.height
#' @param keep.traits
#' @export

var_heatmap <- function(PD.ALL, var_vector, full_range, keep.traits, color_range,
                        barplot_facet = TRUE, title = "",
                        fpath = "/fs/data3/ecowdery/ED.Hydro/figures/",
                        plot.width = 12, plot.height = 13.5){

  # Rework any of the labels, in case they are too long for the plot

  PD.ALL <- PD.ALL %>% mutate(new.labels = case_when(
    trait.labels == "Aboveground Fraction of Structural Biomass" ~ "Frac Biomass Aboveground",
    trait.labels == "leaf NIR reflectance" ~ "Leaf NIR reflectance",
    trait.labels == "leaf NIR transmittance" ~ "Leaf NIR transmittance",
    trait.labels == "leaf VIS reflectance" ~ "Leaf VIS reflectance",
    trait.labels == "leaf VIS transmittance" ~ "Leaf VIS transmittance",
    trait.labels == "Rooting depth allometry slope" ~ "Root depth allom. slope",
    trait.labels == "Rooting depth allometry intercept" ~ "Root depth allom. int.",
    TRUE ~ as.character(trait.labels)
  ))

  # A lot of factor wrangling, here we "refactor" a lot so that factors will
  # show up in the order that we want them to in the plot
  # For example Hydro, Photo, and so on
  # Also introducing new factors "Total_hydro" and "Total"

  PD.ALL <- PD.ALL %>% filter(.,var %in% var_vector)
  PD.ALL$var <- as.factor(PD.ALL$var) %>% factor(levels = var_vector)
  PD.sub <- left_join(keep.traits, PD.ALL, by = "new.labels")
  PD.sub$new.labels <- factor(PD.sub$new.labels, levels = rev(unique(PD.sub$new.labels)))
  PD.sub$model.type <- factor(PD.sub$model.type , levels = c("ORIG", "HYDRO"))
  PD.sub$trait.labels <- factor( PD.sub$trait.type, levels = c(
    "Hydraulics", "Photo.", "Allocation", "Radiation", "Resp.", "Total_hydro", "Total"
  ))
  totals <- list() # Sanity check

  total_var <- PD.sub %>% dplyr::group_by(model.type, var) %>% dplyr::summarise(total = sum(variances))
  total_var$model.type <- factor(total_var$model.type , levels = c("ORIG", "HYDRO"))
  total_var$trait.type <- "Total"
  totals[["Total"]] <- total_var

  total_var

  # Calculate the total variance in each column.
  # If barplot_facet = TRUE, this will be displayed in the
  # "Total" facet at the bottom of the plot.

  # Calculating totals of variances for the other more detailed bar plot,
  # may want to revisit this one
  tv <- PD.sub %>% group_by(model.type, trait.type, var) %>% dplyr::summarize(total = sum(variances))

  if(barplot_facet){
    PD.sub2 <- rbind.fill(PD.sub, totals[["Total"]])
    PD.sub2$new.labels <- addLevel(PD.sub2$new.labels, "")
    PD.sub2[which(PD.sub2$trait.type == "Total"),"new.labels"] <- ""
  }

  #########################################################
  # Time to plot!

  rescale_vec <- color_range

  themes <- theme_classic() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=18, face="bold"),
          axis.title = element_blank(),
          axis.text = element_text(size = 14),
          strip.text.x = element_text(size = 16),
          strip.text.y = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 12),
          legend.key.height = unit(5, "line"),
          legend.key.width = unit(2, "line"))

  heatmap <- ggplot(PD.sub) + themes +
    geom_tile(data = PD.sub, aes(y= new.labels, x = model.type, fill = variances)) +
    viridis::scale_fill_viridis(values = rescale(rescale_vec), direction = -1,
                                limits=c(min(full_range),max(full_range)), na.value="white",
                                guide = "colorbar", name = "Normailized \nVariance") +
    ggtitle(title) +
    facet_grid(factor(trait.type, levels = c(
      "Hydraulics", "Photo.", "Allocation", "Radiation", "Resp."
    )) ~ var, space = "free", scales = "free")
  # Other things I have added to the plot in the past ...
  # geom_text(aes(y= new.labels, x = model.type, label = sprintf("%.1e",variances))) +
  # ylab("Parameters") +
  # xlab("Version of ED") +

  gt <- ggplot_gtable(ggplot_build(heatmap))

  fname <- paste("heatmap", paste(unique(na.omit(PD.sub$wf_id)), collapse = "."),
                 unique(na.omit(PD.sub$met.type)), "png", sep= "." )
  ggsave(file.path(fpath,fname), gt, width = plot.width, height = plot.height)

  if(barplot_facet){

    heatmap2 <- ggplot(PD.sub2) + themes +
      geom_tile(data = PD.sub, aes(y= new.labels, x = model.type, fill = variances)) +
      viridis::scale_fill_viridis(values = rescale(rescale_vec), direction = -1,
                                  limits=c(min(full_range),max(full_range)), na.value="white",
                                  guide = "colorbar", name = "Normailized \nVariance") +
      ggtitle(title) +
      facet_grid(factor(trait.type, levels = c(
        "Hydraulics", "Photo. + S.C.", "Alloc. + Allom.", "Radiation", "Resp. + Turnovr", "Total"
      )) ~ var, space = "free", scales = "free") +
      geom_bar(aes(x = model.type, y = total), stat = "identity")

    gt2 <- ggplot_gtable(ggplot_build(heatmap2))


    # gtable_height(gt)
    # gtable_show_layout(gt)
    a = 18
    gt2$heights[a] = 3*gt2$heights[a]
    grid.draw(gt2)

    fname <- paste("heatmap", paste(unique(PD.sub$wf_id), collapse = "."),
                   unique(na.omit(PD.sub$met.type)),"bar", "png", sep= "." )
    ggsave(file.path(fpath,fname), gt2, width = plot.width, height = plot.height)
  }

  #########################################
  # OH HEY BONUS PLOT

  trait_colors <- c(Set1_color2hex("red"), Set1_color2hex("blue"),
                    Set1_color2hex("yellow"), Set1_color2hex("green"),
                    Set1_color2hex("purple"))
  names(trait_colors) <- c( "Allocation", "Hydraulics", "Photo.",
                            "Radiation", "Resp.")

  base2 <- ggplot() +
    theme_classic() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=18, face="bold"),
          axis.title = element_blank(),
          axis.text = element_text(size = 16),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 12),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12))

  b1 <- base2 + ggtitle(title) +
    geom_bar(data = tv %>% filter(trait.type == "Hydraulics"),
             aes(x = model.type, y = total, fill = trait.type), stat = "identity") +
    scale_fill_manual("trait.type", values = trait_colors) +
    facet_grid(~ var)

  b2 <- base2 +
    geom_bar(data = tv,
             aes(x = model.type, y = total, fill = trait.type), stat = "identity") +
    scale_fill_manual("trait.type", values = trait_colors) +
    facet_grid(~ var)

  gt3 <- arrangeGrob(b1,b2, ncol = 1)

  fname <- paste("barplot", paste(unique(na.omit(PD.sub$wf_id)), collapse = "."),
                 unique(na.omit(PD.sub$met.type)), "png", sep= "." )
  ggsave(file.path(fpath,fname), gt3, width = plot.width, height = plot.height)
}


# # return(gt)
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
# p



#  regional veg model with emergent ecological properties and explicit biogeophysical cycles


# Setup construct plot

# Attempt at rainbow
# colorvec <- c('#dafbff', '#abdda4',  '#ffffbf', '#fdae61', '#d7191c')
# PANK (https://www.invisionapp.com/inside-design/finding-the-right-color-palettes-for-data-visualizations/)
# colorvec <- paste0("#", c("f5aea1", "ea628a", "a83890", "a83890", "3d1058"))
# BLUE (https://www.invisionapp.com/inside-design/finding-the-right-color-palettes-for-data-visualizations/)
# colorvec <- paste0("#", c("b6decb","b6decb", "3683b8", "244794", "151c65"))
# colorvec <- c(paste0("#", c("dcecc8","75c6d0", "3893c1", "26539b", "11154c")))
# GREEN (http://tristen.ca/hcl-picker/#/hlc/5/0.79/263039/D4F68F)
# colorvec <- rev(unlist(strsplit("#263039,#365F62,#4F927F,#83C68B,#D4F68F", ",")))


# n = 7
# n <- length(colorvec)
# rescale_vec <- exp(seq(log(min(PD.ALL$variances)), log(max(PD.ALL$variances)), length.out = n))
# base <- ggplot(PD.sub) +
#   scale_fill_gradientn(colors = colorvec,
#                        values = rescale(rescale_vec),
#                        guide = "colorbar", limits=c(0,max(PD.ALL$variances))) +
#   theme(legend.key.height = unit(6, "line"))


# ggsave("/fs/data3/ecowdery/ED.Hydro/figures/heatmap_water.png",
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



# ggsave("/fs/data3/ecowdery/ED.Hydro/figures/heatmap_dought_bar.png",
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
