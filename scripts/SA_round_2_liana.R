rm(list=ls())

library(PEcAn.all)
library(ED.Hydro.Helpers)
library(grid)
library(gtable)
library(magrittr)
library(RColorBrewer)
library(scales)
library(plyr)
library(tidyverse)
library(ggforce)
library(dplyr)

keep.traits <- which_keep_traits(Betsy = FALSE)


################################################################################
# Set up and run ensemble and sensitivity analysis for all variables

wf_ids <- c(99000000012,99000000014)
fpath = paste0("/home/carya/Figures/",paste(wf_ids, collapse = "_"))
dir.create(fpath)

var <- c("NPP_pft")

for(wf_id in wf_ids){
  prep_VDC(wf_id = wf_id, var = var,
           path_to_config = paste0("/home/carya/output/PEcAn_", wf_id ),
           REDO = TRUE
  )
}

################################################################################
# Create variance decomposition plots

plot = TRUE
PDF = TRUE

model <- data.frame(
  wf_id =  wf_ids,
  model.type = c("Posterior","Prior"),
  met.type = c("water", "water"),
  stringsAsFactors = FALSE
)
model <- model %>% mutate(title = sprintf("%s (%.0f)", model.type, wf_id))

PD_model <- plot_VDC(model, var, keep.traits, plot= TRUE, PDF=TRUE, fpath,
                     path_to_config="/home/carya/output/PEcAn_")


#
# plot.data <- PD_model[[2]]
# namedcolors <- c(brewer.pal(3, "Set1")[1:2], "grey40", "black")
#
# plot.data$model.type <- as.character(plot.data$model.type)
# df <- plot.data %>%
#   group_by(model.type) %>% mutate_each(funs(normalize),variances)
# plot.data <- plot.data %>%  mutate(varNor = df$variances)
#
#
# base.plot <- ggplot(plot.data) +
#   coord_flip() + scale_color_brewer(palette="Set1") +
#   theme_classic() + theme(
#     axis.text.y = element_blank(), axis.ticks = element_blank(),
#     axis.line = element_blank(), axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank())
#
# cv.plot <-  base.plot + ggtitle("Coefficient of Variance") +
#   geom_hline(yintercept = 0) +
#   geom_pointrange(aes(x = points, y = coef.vars, ymin = 0,
#                       ymax = coef.vars, color = cv_color),
#                   alpha = .7, size = 1.25) +
#   scale_y_continuous(labels = function(x) sprintf("%.2e", x)) +
#   xlim(1, max(plot.data$points)) +
#   theme(legend.position="none") +
#   scale_color_manual("cv_color", values = namedcolors)
#
# el.plot <- base.plot + ggtitle("Elasticity") +
#   geom_hline(yintercept = 0) +
#   geom_pointrange(aes(x = points, y = elasticities, ymin = 0,
#                       ymax = elasticities, color = el_color),
#                   alpha = .7, size = 1.25) +
#   xlim(1, max(plot.data$points)) +
#   theme(legend.position="none") +
#   scale_color_manual("el_color", values = namedcolors)
#
# pv.plot <- base.plot + ggtitle("Variance") +
#   geom_hline(yintercept = 0) +
#   geom_pointrange(aes(x = points, variances, ymin = 0,
#                       ymax = variances, color = pv_color),
#                   alpha = .7, size = 1.25) +
#   xlim(1, max(plot.data$points)) +
#   scale_color_manual("pv_color", values = namedcolors) +
#   theme(legend.position="none")
#
# trait.plot <- base.plot + ggtitle("Parameter")  +
#   geom_text(aes(y = 1, x = points, label = new.labels, hjust = 1,
#                 color = label_color)) +
#   scale_y_continuous(breaks = c(0, 0), limits = c(0, 1)) +
#   xlim(0, max(plot.data$points)) +
#   theme(axis.text.x = element_blank()) +
#   scale_color_manual("label_color", values = namedcolors) +
#   theme(legend.position="none")
#
# pv.plot <- base.plot + ggtitle("Variance") +
#   geom_hline(yintercept = 0) +
#   geom_pointrange(aes(x = points, variances, ymin = 0,
#                       ymax = variances, color = pv_color),
#                   alpha = .7, size = 1.25) +
#   xlim(1, max(plot.data$points)) +
#   scale_color_manual("pv_color", values = namedcolors) +
#   theme(legend.position="none")
#
# zoom.plot <- base.plot + ggtitle("Normalized variance") +
#   geom_hline(yintercept = 0) +
#   geom_pointrange(aes(x = points, y = varNor, ymin = 0,
#                       ymax = varNor, color = el_color),
#                   alpha = .7, size = 1.25) +
#   xlim(1, max(plot.data$points)) +
#   theme(legend.position="none") +
#   scale_color_manual("el_color", values = namedcolors)
#
#
#
# title1=textGrob(var[length(var)], gp=gpar(fontsize=24, fontface="bold"))
#
# fname <- paste(c("VDC",unique(model$wf_id),var[length(var)],unique(model$met.type),"png"), collapse = ".")
# ggsave(filename = file.path(fpath,fname),
#        plot = grid.arrange(trait.plot, cv.plot, el.plot, pv.plot,zoom.plot, ncol = 5, top = title1),
#        width = 9.7, height = 7, units = "in")


#
# for(v in var){
#   max_var <- max(PD_model %>% filter(var == v) %>% pull(variances))
#   new_model <- PD_model %>% filter(var == v) %>% pull(variances)/max_var
#   PD_model <- PD_model %>% mutate(variances=replace(variances, var==v, new_model))
# }
#
# color_range <- c(2.027377e-32, 1.418114e-07, 1.527431e-05, 3.576483e-04, 0.0019560172, 0.025047194, 1)
#
#
# full_range <- range(PD_model$variances)
# var_vector <- c("GPP", "TVeg", "NPP", "SoilMoist")
#
# PD.ALL = PD_model
#
# plot.width = 12
# plot.height = 13.5
# barplot_facet = TRUE
# title = sprintf("%s: %s", paste(model$title, collapse = " & "), toupper(unique(model$met.type)))
#
# var_heatmap(PD.ALL = PD_model, var_vector, full_range, keep.traits,
#             color_range, barplot_facet = TRUE, title, fpath,
#             plot.width = 12, plot.height = 13.5)
#
