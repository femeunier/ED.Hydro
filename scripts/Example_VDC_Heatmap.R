library(ED.Hydro.Helpers)
library(PEcAn.utils)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(plyr)
library(scales)

# Example uses of ED.Hydro functions!

# Pick workflow ids
# ED Hydro : 1000010515
# ED Orig:   1000010514

wf_ids <- c(1000010515,1000010514)

# Pick output variables

var <- c("GPP", "NPP", "TVeg", "SoilMoist")

# Make sure the VDC has been run for every variable
# Note: every time you change which priors are associated with a PFT (ie which traits it looks for)
# Or you change what trait names are called in the trait mapping table or
# units are changed etc, it's probably a good idea to REDO the VDC.
# You can always BACKUP the previous results of the VDC to see how they changed!
# You can't do that from the function, but since you know the path to the VDC output anyway, I recommend doing that.
# At least with the plots in the next step.

for(wf_id in wf_ids){
  prep_VDC(wf_id = wf_id, var = var,
           path_to_config = paste0("/fs/data2/output/PEcAn_", wf_id ),
           REDO = FALSE
  )
}

# Create variance decomposition plots

plot = TRUE
PDF = TRUE
if(PDF){ # IF you're printing to PDF, pick where you're printing to!
  fpath = paste0("/fs/data3/ecowdery/ED.Hydro/figures/",paste(wf_ids, collapse = "_"))
  dir.create(fpath)
}

# Met "type" is just for keeping track of watered vs. drought and
# should be auto generated, but for now you need to keep track of this by hand
model <- data.frame(
  wf_id =  wf_ids,
  model.type = c("ORIG", "HYDRO"),
  met.type = c("water", "water"),
  stringsAsFactors = FALSE
)
model <- model %>% mutate(title = sprintf("%s (%.0f)", model.type, wf_id))

keep.traits <- which_keep_traits()

PD_model <- plot_VDC(model, var, keep.traits, plot= plot, PDF=PDF, fpath)

# Creat heatmaps

# Most of this is all just to figure out the range of variance values which then
# determines the color values

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
            color_range, barplot_facet, title, fpath,
            plot.width, plot.height)
