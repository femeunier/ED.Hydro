remove(trait.data)
remove(SLA)
load("/fs/data2/output//PEcAn_1000010020/pft/broadleaf_evergreen_tropical_tree/trait.data.Rdata")
SLA <- trait.data$SLA


plot(density(trait.data$SLA$mean))

mean(trait.data$SLA$mean)

give.n <- function(x){
  return(data.frame(y = min(x), label = paste0("\n",paste0("n = ",length(x)))))
}

p1 <- ggplot(SLA) + geom_density(aes(x = mean)) + coord_flip()  + scale_y_reverse() + 
  geom_vline(aes(xintercept = mean(mean)), size = 1, color = "gray")

p2 <- ggplot(SLA, aes(x = as.factor(site_id), y = mean)) +
  geom_hline(aes(yintercept = mean(mean)), size = 1, color = "gray")+
  # geom_hline(aes(yintercept = 20), size = 1) +
  # geom_violin(aes(x = as.factor(site_id), y = mean), draw_quantiles = TRUE, size = 2) + 
  geom_boxplot() + 
  geom_jitter(aes(x = as.factor(site_id), y = mean, color = as.factor(citation_id)), width = .1, size = 4, alpha = .5) + 
  stat_summary(fun.data = give.n, geom = "text", size = 6)



lay <- rbind(c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2),
             c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2))
grid.arrange(p1,p2, layout_matrix = lay)


p_sites <- c(368, 499, 550, 641,
             1230, 1232, 1234, 1235,
             1259)

problem_SLA <- SLA %>% filter(site_id %in% p_sites)

ggplot(problem_SLA) +
  geom_hline(aes(yintercept = mean(mean)), size = 1)+
  geom_hline(aes(yintercept = 20), size = 1) +
  geom_boxplot(aes(x = as.factor(site_id), y = mean)) + 
  geom_point(aes(x = as.factor(site_id), y = mean, color = as.factor(citation_id))) +
  ggtitle("All problem SLA")


problem_SLA[,c("site_id", "specie_id")] %>% group_by(site_id) %>% summarize(n_species = length(unique(specie_id)))

problem_SLA[,c("site_id", "specie_id")] %>% group_by(specie_id) %>% summarize(n_species = length(unique(site_id)))

length(problem_SLA$specie_id %in% c(831, 43019))

problem_SLA_no_cassava <- problem_SLA %>% filter(!specie_id == 831)
# For now remove cassava
ggplot(problem_SLA_no_cassava) +
  geom_hline(aes(yintercept = mean(mean)), size = 1)+
  geom_hline(aes(yintercept = 20), size = 1) +
  geom_boxplot(aes(x = as.factor(site_id), y = mean)) + 
  geom_point(aes(x = as.factor(site_id), y = mean))

problem_SLA_cassava <- problem_SLA %>% filter(specie_id %in% c(831, 43019))
# For now remove cassava
ggplot(problem_SLA_cassava) +
  geom_hline(aes(yintercept = mean(mean, na.rm=TRUE)), size = 1)+
  geom_hline(aes(yintercept = 20), size = 1) +
  geom_boxplot(aes(x = as.factor(site_id), y = mean)) + 
  geom_point(aes(x = as.factor(site_id), y = mean))

#################

problem_SLA %>% filter(site_id == 641) %>% 
  dplyr::select(one_of(c("id", "citation_id","site_id", "vname", "treatment_id", "mean_unconverted", "mean", "Notes"))) 

udunits2::ud.convert(0.0178, "kg m-2", "cm^2 g-1")


1/udunits2::ud.convert(44.2, "cm2 g-1", "kg m-2")
1/udunits2::ud.convert(39.1, "cm2 g-1", "kg m-2")
1/udunits2::ud.convert(18.5, "cm2 g-1", "kg m-2")

values <- c(17.8,18.5,39.1,44.2)

for(v in values){
  LMA1 <- udunits2::ud.convert(v, "cm2 g-1", "kg m-2")
  LMA2 <- udunits2::ud.convert(v, "g cm-2", "kg m-2")
  LMA3 <- udunits2::ud.convert(v, "g m-2", "kg m-2")
  units <- c("cm2 g-1", "g cm-2", "g m-2")
  LMA <- c(LMA1, LMA2, LMA3)
  SLA = 1/LMA
  LAI = SLA/.48
  
  print(data.frame(units = units, LMA = LMA, SLA = SLA , LAI = LAI))
}


LMA <- udunits2::ud.convert(2.69, "mg cm-2", "kg m-2")
SLA = 1/LMA
SLA1
SLA2 = SLA1/.48
LAI = SLA * Bleaf

Bleaf = LAI/SLA2

SLA1 = 11          ; SLA1
SLA2 = SLA1/.48    ; SLA2
LAI = SLA2 * Bleaf ; LAI
  
# Site 499 
problem_SLA %>% filter(site_id == 499) %>% 
  dplyr::select(one_of(c("id", "citation_id","site_id", "vname", "treatment_id", "mean_unconverted", "mean", "Notes"))) 


# Site 550 
problem_SLA %>% filter(site_id == 550) %>% 
  dplyr::select(one_of(c("id", "citation_id","site_id", "vname", "treatment_id", "mean_unconverted", "mean", "Notes"))) 


# This works
udunits2::ud.convert(0.038, "kg m-2", "mg cm-2") # figure 4
udunits2::ud.convert(0.027, "kg m-2", "mg cm-2") # figure 5


udunits2::ud.convert(0.027, "kg m-2", "cm2 g-1") # figure 6
udunits2::ud.convert(0.038, "kg m-2", "cm2 g-1") # figure 7

# Site 1230 
problem_SLA %>% filter(site_id == 1230) %>% 
  dplyr::select(one_of(c("id", "citation_id","site_id", "vname", "treatment_id", "mean_unconverted", "mean", "Notes"))) 

# Site 1230 
problem_SLA %>% filter(site_id == 1235) %>% 
  dplyr::select(one_of(c("id", "citation_id","site_id", "vname", "treatment_id", "mean_unconverted", "mean", "Notes"))) 
