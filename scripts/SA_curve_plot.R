model_water <- data.frame(
  wf_id = c(1000010046, 1000010047),
  model.type = c("ORIG", "HYDRO"), 
  met.type = c("water", "water"), 
  stringsAsFactors = FALSE
)

l <- list()
for(j in seq_along(model_water$wf_id)){
  conf_file <- paste0("/fs/data2/output/PEcAn_",model_water$wf_id[j],"/pecan.CONFIGS.","SoilMoist",".xml")
  settings <- read.settings(conf_file)
  SA_file <- sprintf("/fs/data2/output/PEcAn_%s/sensitivity.results.%s.%s.%s.%s.Rdata",
                     settings$workflow$id,
                     settings$sensitivity.analysis$ensemble.id,
                     settings$sensitivity.analysis$variable, 
                     settings$sensitivity.analysis$start.year, 
                     settings$sensitivity.analysis$end.year)
  load(SA_file)
  samps <- sensitivity.results$ED_tropical$sensitivity.output$sa.samples$fineroot2leaf
  samps <- sensitivity.results$ED_tropical_hydro$sensitivity.output$sa.samples$fineroot2leaf
  l[[j]] <- data.frame(samps = samps,
             model = rep(model_water$model.type[j], length(samps)))
}

dat <- do.call(rbind.data.frame, l) %>% as.data.frame()


load("/fs/data2/output/PEcAn_1000010046/sensitivity.samples.1000020406.Rdata")

dat$trait <- sa.samples$ED_tropical$fineroot2leaf

ggplot(dat) + geom_line(aes(x = trait, y = samps)) 

j = 1
conf_file <- paste0("/fs/data2/output/PEcAn_",model_water$wf_id[j],"/pecan.CONFIGS.","SoilMoist",".xml")
settings <- read.settings(conf_file)
SA_file <- sprintf("/fs/data2/output/PEcAn_%s/sensitivity.results.%s.%s.%s.%s.Rdata",
                   settings$workflow$id,
                   settings$sensitivity.analysis$ensemble.id,
                   settings$sensitivity.analysis$variable, 
                   settings$sensitivity.analysis$start.year, 
                   settings$sensitivity.analysis$end.year)
load(SA_file)

sa.sample1 <- sensitivity.results$ED_tropical$sensitivity.output$sa.samples$fineroot2leaf
sa.spline1 <- sensitivity.results$ED_tropical$sensitivity.output$sa.splines$fineroot2leaf

plot(sensitivity.results$ED_tropical$sensitivity.output$sa.splines$fineroot2leaf, 
     xlab = "Fine Root Allocation", ylab = "Soil Moisture", lwd = 2)

j = 2 
conf_file <- paste0("/fs/data2/output/PEcAn_",model_water$wf_id[j],"/pecan.CONFIGS.","SoilMoist",".xml")
settings <- read.settings(conf_file)
SA_file <- sprintf("/fs/data2/output/PEcAn_%s/sensitivity.results.%s.%s.%s.%s.Rdata",
                   settings$workflow$id,
                   settings$sensitivity.analysis$ensemble.id,
                   settings$sensitivity.analysis$variable, 
                   settings$sensitivity.analysis$start.year, 
                   settings$sensitivity.analysis$end.year)
load(SA_file)

sa.sample2 <- sensitivity.results$ED_tropical$sensitivity.output$sa.samples$fineroot2leaf
sa.spline2 <- sensitivity.results$ED_tropical$sensitivity.output$sa.splines$fineroot2leaf
sa.spline2(sa.sample)

df <- data.frame(xval = c(sa.sample1, sa.sample2), yval = c(sa.spline1(sa.sample), sa.spline2(sa.sample)) )
df$model <- c(rep("ORIG",5), rep("HYDRO",5))
df$model <- factor(df$model, levels = c("ORIG", "HYDRO"))
plot(df$x, df$y)

ggplot(df) + geom_smooth(aes(x = xval, y = yval), size = 1, color = "black") + geom_point(aes(x = xval, y = yval), size = 3) + facet_grid( ~ model)  + theme_bw() + xlab("Fine Root Allocation (%)") + ylab("Soil Moisture (kg m-2)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12)) 

ggsave("/fs/data3/ecowdery/ED_Tropics/figures/SoilMoist.SA.png", last_plot(), height = 3, width = 8, units = "in")

  trait <- "fineroot2leaf"

  plot_sensitivity(sa.sample1, sa.spline1, trait, y.range = c(.25, .3), median.i = 4, 
                   prior.sa.sample = NULL, prior.sa.spline = NULL, 
                   fontsize = list(title = 12, axis = 8), 
                   linesize = 1, dotsize = 2)
  
  plot_sensitivity(sa.sample2, sa.spline2, trait, y.range = c(.25, .3), median.i = 4, 
                   prior.sa.sample = NULL, prior.sa.spline = NULL, 
                   fontsize = list(title = 12, axis = 8), 
                   linesize = 1, dotsize = 2)
  