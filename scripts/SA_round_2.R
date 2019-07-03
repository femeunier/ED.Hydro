library(PEcAn.all)
library(ED.Hydro.Helpers)
library(grid)
library(gtable)
library(magrittr)
library(RColorBrewer)
library(scales)
library(plyr)
library(tidyverse)

keep.traits <- which_keep_traits()


################################################################################
# Set up and run ensemble and sensitivity analysis for all variables

wf_ids <- c(1000010449,1000010428)
wf_ids <- c(1000010046,1000010047) # The runs I used for the dept talk


# New Hydro that runs 1000010481
wf_ids <- c(1000010428,1000010481)

# 2005 runs that work and are the example for the final 2012 runs
wf_ids <- c(1000010491,1000010490)

# 2012 runs
fpath = "/fs/data3/ecowdery/ED.Hydro/figures/6_21_meeting"
wf_ids <- c(1000010493,1000010492)

wf_ids <- c(1000010495,1000010494)

# 2012 adding in more parameters
wf_ids <- c(1000010515,1000010514)
fpath = paste0("/fs/data3/ecowdery/ED.Hydro/figures/",paste(wf_ids, collapse = "_"))
dir.create(fpath)

var <- c("GPP", "NPP", "TVeg", "SoilMoist")

for(wf_id in wf_ids){
  prep_VCD(wf_id, var)
}


################################################################################
# Create variance decomposition plots


plot = TRUE
PDF = TRUE

model <- data.frame(
  wf_id =  wf_ids,
  model.type = c("ORIG", "HYDRO"),
  met.type = c("water", "water"),
  stringsAsFactors = FALSE
)
model <- model %>% mutate(title = sprintf("%s (%.0f)", model.type, wf_id))

PD_model <- plot_VCD(model, var, keep.traits, plot= TRUE, PDF=TRUE, fpath)

for(v in var){
  max_var <- max(PD_model %>% filter(var == v) %>% pull(variances))
  new_model <- PD_model %>% filter(var == v) %>% pull(variances)/max_var
  PD_model <- PD_model %>% mutate(variances=replace(variances, var==v, new_model))
}

color_range <- c(2.027377e-32, 1.418114e-07, 1.527431e-05, 3.576483e-04, 0.0019560172, 0.025047194, 1)


full_range <- range(PD_model$variances)
var_vector <- c("GPP", "TVeg", "NPP", "SoilMoist")

PD.ALL = PD_model

plot.width = 12
plot.height = 13.5
barplot_facet = TRUE
title = sprintf("%s: %s", paste(model$title, collapse = " & "), toupper(unique(model$met.type)))

var_heatmap(PD.ALL = PD_model, var_vector, full_range, keep.traits,
            color_range, barplot_facet = TRUE, title, fpath,
            plot.width = 12, plot.height = 13.5)

