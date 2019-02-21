library(PEcAn.all)
library(grid)
library(gtable)
library(magrittr)
library(RColorBrewer)
library(scales)
library(plyr)
source("/fs/data3/ecowdery/ED_Tropics/scripts/EA_SA_functions.R")
source("/fs/data3/ecowdery/ED_Tropics/scripts/new_aes.R")
################################################################################
# Set up and run ensemble and sensitivity analysis for all variables

# wf_ids <- c(1000010046, 1000010047, 1000010049, 1000010050)
wf_ids <- c(1000010047,1000010230)
var <- c("GPP", "NPP", "TVeg", "SoilMoist")

for(wf_id in wf_ids){
  prep_VCD(wf_id, var)
}

################################################################################
# Create variance decomposition plots

plot = TRUE
PDF = TRUE

model_water <- data.frame(
  wf_id = c(1000010046, 1000010047),
  model.type = c("ORIG", "HYDRO"), 
  met.type = c("water", "water"), 
  stringsAsFactors = FALSE
)

model_drought <- data.frame(
  wf_id = c(1000010049, 1000010050),
  model.type = c("ORIG", "HYDRO"), 
  met.type = c("drought", "drought"), 
  stringsAsFactors = FALSE
)

compare_fr <- data.frame(
  wf_id = c(1000010047, 1000010230),
  model.type = c("ORIG", "HYDRO"), 
  met.type = c("water", "water"), 
  stringsAsFactors = FALSE
)

PD_water <- plot_VCD(model_water, var, plot, PDF)
PD_drought <- plot_VCD(model_drought, var, plot, PDF)

PD_compare_fr <- plot_VCD(compare_fr, var, plot, PDF)

for(v in var){
  max_var <- max(c(PD_water %>% filter(var == v) %>% pull(variances),
                 PD_drought %>% filter(var == v) %>% pull(variances)))
  
  new_water <- PD_water %>% filter(var == v) %>% pull(variances)/max_var
  new_drought <- PD_drought %>% filter(var == v) %>% pull(variances)/max_var
  
  hist(c(new_water, new_drought))
  
  PD_water <- PD_water %>% mutate(variances=replace(variances, var==v, new_water))
  PD_drought <- PD_drought %>% mutate(variances=replace(variances, var==v, new_drought))
}

for(v in var){
  max_var <- max(PD_compare_fr %>% filter(var == v) %>% pull(variances))
  new_compare_fr <- PD_compare_fr %>% filter(var == v) %>% pull(variances)/max_var
  PD_compare_fr <- PD_compare_fr %>% mutate(variances=replace(variances, var==v, new_compare_fr))
}

################################################################################
################################################################################
# Make the final heatmaps of variance

full_range_old <- range(PD_water$variances, PD_drought$variances)
max_var <- full_range_old[2]

color_range <- c(2.027377e-32, 1.418114e-07, 1.527431e-05, 3.576483e-04, 0.0019560172, 0.025047194, 1)


full_range <- range(PD_water$variances, PD_drought$variances)
var_vector <- c("GPP", "TVeg", "NPP", "SoilMoist")

var_heatmap(PD_water, var_vector, full_range, color_range, plot.width = 12, plot.height = 13.5)
var_heatmap(PD_drought, var_vector, full_range, color_range, plot.width = 12, plot.height = 13.5)


ggsave(file.path(fpath,fname), gt, width = 12, height = 13.5)


# par(mfrow= c(1,1))
# ggplot(PD.ALL, aes(x = variances)) + geom_histogram()
# ggplot(PD.ALL, aes(x = variances)) + geom_histogram() + scale_x_log10()

full_range_old <- range(PD_compare_fr$variances)
max_var <- full_range_old[2]

color_range <- c(2.027377e-32, 1.418114e-07, 1.527431e-05, 3.576483e-04, 0.0019560172, 0.025047194, 1)


full_range <- range(PD_compare_fr$variances)
var_vector <- c("GPP", "TVeg", "NPP", "SoilMoist")

var_heatmap(PD_compare_fr, var_vector, full_range, color_range, plot.width = 12, plot.height = 13.5)

ggsave(file.path(fpath,fname), gt, width = 12, height = 13.5)
