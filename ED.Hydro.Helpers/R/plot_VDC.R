#' Prepare VDC data and make plots
#'
#' @param model
#' @param var
#' @param plot
#' @param PDF
#' @export

plot_VDC <- function(model, var, keep.traits, plot, PDF,
                     fpath = "/fs/data3/ecowdery/ED.Hydro/figures"){

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
        trait.labels == "Aboveground Fraction of Structural Biomass" ~ "Frac Biomass Aboveground",
        trait.labels == "leaf NIR reflectance" ~ "Leaf NIR reflectance",
        trait.labels == "leaf NIR transmittance" ~ "Leaf NIR transmittance",
        trait.labels == "leaf VIS reflectance" ~ "Leaf VIS reflectance",
        trait.labels == "leaf VIS transmittance" ~ "Leaf VIS transmittance",
        trait.labels == "Rooting depth allometry slope" ~ "Root depth allom. slope",
        trait.labels == "Rooting depth allometry intercept" ~ "Root depth allom. int.",
        TRUE ~ as.character(trait.labels)
      ))

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

      cv.plot <-  base.plot + ggtitle("Coefficient of Variance (log(abs(x)))") +
        geom_hline(yintercept = 0) +
        geom_pointrange(aes(x = points, y = coef.vars, ymin = 0,
                            ymax = coef.vars, color = cv_color),
                        alpha = .7, size = 1.25) +
        scale_y_continuous(labels = function(x) sprintf("%.2e", x)) +
        xlim(1, max(plot.data$points)) +
        theme(legend.position="none") +
        scale_color_manual("cv_color", values = namedcolors)

      el.plot <- base.plot + ggtitle("Elasticity") +
        geom_hline(yintercept = 0) +
        geom_pointrange(aes(x = points, y = elasticities, ymin = 0,
                            ymax = elasticities, color = el_color),
                        alpha = .7, size = 1.25) +
        xlim(1, max(plot.data$points)) +
        theme(legend.position="none") +
        scale_color_manual("el_color", values = namedcolors)

      pv.plot <- base.plot + ggtitle("Variance") +
        geom_hline(yintercept = 0) +
        geom_pointrange(aes(x = points, variances, ymin = 0,
                            ymax = variances, color = pv_color),
                        alpha = .7, size = 1.25) +
        xlim(1, max(plot.data$points)) +
        scale_color_manual("pv_color", values = namedcolors) +
        theme(legend.justification=c(1,0), legend.position=c(1,0))

      trait.plot <- base.plot + ggtitle("Parameter")  +
        geom_text(aes(y = 1, x = points, label = new.labels, hjust = 1,
                      color = label_color)) +
        scale_y_continuous(breaks = c(0, 0), limits = c(0, 1)) +
        xlim(0, max(plot.data$points)) +
        theme(axis.text.x = element_blank()) +
        scale_color_manual("label_color", values = namedcolors) +
        theme(legend.position="none")

      title1=textGrob(var[i], gp=gpar(fontsize=24, fontface="bold"))

      if(PDF){
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
