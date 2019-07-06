#' Prepare VDC data and make plots
#'
#' @param model
#' @param var
#' @param plot
#' @param PDF
#' @export

plot_VDC <- function(model, var, keep.traits, plot, PDF,
                     fpath = "/fs/data3/ecowdery/ED.Hydro/figures",path_to_config = NA){

  if(is.na(path_to_config)){ # assume we're on test-pecan and auto generate the file path
    path_to_config <- sprintf("/fs/data2/output/PEcAn_%i", wf_id)
  }

  normalize <- function(x){
    return((x/max(x)))
  }

  # namedcolors <- brewer.pal(3, "Set1")
  namedcolors <- c(brewer.pal(3, "Set1")[1:2], "grey40", "black")
  # namedcolors <- c("#c23c81", "#238a8d", "grey40", "black")
  #names(namedcolors) <- c("ORIG", "HYDRO", "No_Diff", "Shared")

  plot_data <- list()
  PD <- list()

  for(i in seq_along(var)){

    plot_data[[var[i]]] <- list()

    for(j in seq_along(model$wf_id)){
      conf_file <- paste0(path_to_config,model$wf_id[j],"/pecan.CONFIGS.",var[i],".xml")
      settings <- PEcAn.settings::read.settings(conf_file)
      SA_file <- paste0(path_to_config,
                        sprintf("%s/sensitivity.results.%s.%s.%s.%s.Rdata",
                                model$wf_id[j],
                                settings$sensitivity.analysis$ensemble.id,
                                settings$sensitivity.analysis$variable,
                                settings$sensitivity.analysis$start.year,
                                settings$sensitivity.analysis$end.year))
      load(SA_file)

      pft <- names(sensitivity.results) # Assuming there is only one

      plot.inputs <- sensitivity.results[[pft]]$variance.decomposition.output

      traits <-  names(plot.inputs$variances)
      units <- as.character(trait.lookup(traits)$units)
      trait.labels <- as.character(trait.lookup(traits)$figid)

      plot_data[[var[i]]][[as.character(model$wf_id[j])]] <-
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

      plot.data$model.type <- as.character(plot.data$model.type)

      plot.data$coef.vars <- ((plot.data$coef.vars))
      plot.data$std <- sqrt(plot.data$variances)

      ord <- sort(plot.data$variances[plot.data$model.type == plot.data$model.type[length(plot.data$model.type)]],index.return=TRUE,decreasing = FALSE)
      plot.data$points[sort(rep(seq(0,1)*length(ord$x),length(ord$x)))+rep(ord$ix,length(unique(plot.data$model.type)))]=
        rep(seq_along(unique(plot.data$new.labels)),length(unique(plot.data$model.type)))


      #plot.data$points <- plot.data$points[order(plot.data$variances[plot.data$model.type == plot.data$model.type[length(plot.data$model.type)]])]



      df <- plot.data %>%
        group_by(model.type) %>% mutate_each(funs(normalize),c(std,variances))

      plot.data <- plot.data %>%  mutate(varNor = df$std, varNor2 = df$variances)


      base.plot <- ggplot(plot.data) +
        coord_flip() + scale_color_brewer(palette="Set1") +
        theme_classic() + theme(
          axis.text.y = element_blank(), axis.ticks = element_blank(),
          axis.line = element_blank(), axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())

      cv.plot <-  base.plot + ggtitle("Coefficient of Variance") +
        geom_hline(yintercept = 0) +
        geom_pointrange(aes(x = points, y = coef.vars, ymin = 0,
                            ymax = coef.vars, color = cv_color),
                        alpha = .7, size = 1.25) +
        scale_y_continuous(labels = function(x) sprintf("%.2e", x)) +
        xlim(1, max(plot.data$points)) + ylim(0, 1.25*max(plot.data$coef.vars)) +
        theme(legend.position="none") +
        scale_color_manual("cv_color", values = namedcolors)

      el.plot <- base.plot + ggtitle("Elasticity") +
        geom_hline(yintercept = 0) +
        geom_pointrange(aes(x = points, y = elasticities, ymin = 0,
                            ymax = elasticities, color = el_color),
                        alpha = .7, size = 1.25) +
        xlim(1, max(plot.data$points)) + ylim(ifelse(min(plot.data$elasticities)>0,0.75,1.25)*min(plot.data$elasticities), 1.25*max(plot.data$elasticities)) +
        theme(legend.position="none") +
        scale_color_manual("el_color", values = namedcolors)

      trait.plot <- base.plot + ggtitle("Parameter")  +
        geom_text(aes(y = 1, x = points, label = new.labels, hjust = 1,
                      colour = "black")) +
        scale_y_continuous(breaks = c(0, 0), limits = c(0, 1)) +
        xlim(0, max(plot.data$points)) +
        theme(axis.text.x = element_blank()) +
        scale_color_manual("label_color", values = "black") +
        theme(legend.position="none")

      pv.plot <- base.plot + ggtitle("Standard deviation") +
        geom_hline(yintercept = 0) +
        geom_pointrange(aes(x = points, std, ymin = 0,
                            ymax = std, color = pv_color),
                        alpha = .7, size = 1.25) +
        xlim(1, max(plot.data$points)) + ylim(0, 1.25*max(plot.data$std)) +
        scale_color_manual("pv_color", values = namedcolors) +
        theme(legend.position="none") + scale_y_continuous(labels = scientific)

      zoom.plot <- base.plot + ggtitle("Normalized standard deviation") +
        geom_hline(yintercept = 0) +
        geom_pointrange(aes(x = points, y = varNor, ymin = 0,
                            ymax = varNor, color = el_color),
                        alpha = .7, size = 1.25) +
        xlim(1, max(plot.data$points)) + ylim(0, 1.25) +
        theme(legend.justification=c(1,0), legend.position=c(1,0)) +
        scale_color_manual("el_color", values = namedcolors) +
        theme(legend.title = element_blank())


      title1=textGrob(var[i], gp=gpar(fontsize=24, fontface="bold"))

      if(PDF){
        fname <- paste(c("VDC",unique(model$wf_id),var[i],unique(model$met.type),"png"), collapse = ".")
        # pdf(file.path(fpath,fname), width = 11, height = 8)
        # dev.off()
        ggsave(filename = file.path(fpath,fname),
               plot = grid.arrange(trait.plot, cv.plot, el.plot, pv.plot,zoom.plot, ncol = 5, top = title1),
               width = 18, height = 8, units = "in")
      }else{
        grid.arrange(trait.plot, cv.plot, el.plot,pv.plot,zoom.plot, ncol = 5, top = title1)
      }
    }
  }
  PD <- do.call(rbind.data.frame, PD) %>%
    dplyr::select(one_of("model.type", "var", "variances", "trait.labels", "wf_id", "met.type"))
  return(list(PD,plot.data))
}
