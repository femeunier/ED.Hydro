bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")
source("/fs/data3/ecowdery/ED_Tropics/parameters/prior_functions.R")

sanity_plot <- function(v, extra_text = "", data = NA, default=NA, xlim = NULL){
  var_id <- tbl(bety, "variables") %>% filter(name == v) %>% pull(id)
  fit <- tbl(bety, "priors") %>% filter(variable_id == var_id) %>% collect()
  plot(density(rdistn(fit)), main = paste(v, extra_text), xlim = xlim)
  abline(v = get_ED_default(PFT3_defaults_history, v), col = "blue", lwd = 2)
  if(!is.na(data)){
    abline(v = data, col = "green", lwd = 2)
  }
  if(!is.na(default)){
    abline(v = default, col = "blue", lwd = 2, lty = 2)
  }
}

par(mfrow = c(4,4))

sanity_plot("leaf_psi_osmotic", "(pinot)")
sanity_plot("wood_psi_osmotic", "(pinot)")
sanity_plot("leaf_elastic_mod", "(epsil)")
sanity_plot("wood_elastic_mod", "(epsil)")
sanity_plot("leaf_psi_tlp", "(pitlp)")
sanity_plot("wood_psi_tlp", "(pitlp)")
sanity_plot("leaf_water_sat", "(thetas)")
sanity_plot("wood_water_sat", "(thetas)")
sanity_plot("leaf_water_cap")
sanity_plot("wood_water_cap")
sanity_plot("leaf_psi_min")
sanity_plot("wood_psi_min")
sanity_plot("wood_Kmax", "(Kmax)")
sanity_plot("wood_psi50", "(psi50)", default = -312.4401153)
sanity_plot("wood_Kexp", "(avuln)", default = 1.798829586)

####################################

par(mfrow = c(2,2))


sanity_plot("leaf_psi_tlp", data = -236.3722076416)
x <- trait_data %>% filter(variable_id == v_df$variables_ids[v_df$variables == "leaf_psi_tlp"]) %>% pull(mean)
y <- rep(0, length(x))
points(x,y, pch = "|", col = "magenta") 

sanity_plot("wood_Kmax", "(Kmax)",  data = 0.0495331958, xlim = c(0,10))
x <- trait_data %>% filter(variable_id == v_df$variables_ids[v_df$variables == "wood_Kmax"]) %>% pull(mean)
y <- rep(0, length(x))
points(x,y, pch = "|", col = "magenta") 
lines(density(x), col = "magenta")

sanity_plot("wood_psi50", "(psi50)", data = -213.7043914795, default = -312.4401153)
x <- trait_data %>% filter(variable_id == v_df$variables_ids[v_df$variables == "wood_psi50"]) %>% pull(mean)
y <- rep(0, length(x))
points(x,y, pch = "|", col = "magenta")
lines(density(x), col = "magenta")

sanity_plot("wood_Kexp", "(avuln)", data = 2.2011377811, default = 1.798829586, xlim = c(0,20))
x <- trait_data %>% filter(variable_id == v_df$variables_ids[v_df$variables == "wood_Kexp"]) %>% pull(mean)
y <- rep(0, length(x))
points(x,y, pch = "|", col = "magenta") 
lines(density(x), col = "magenta")

