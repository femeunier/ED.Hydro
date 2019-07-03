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

################################################################################
################################################################################


library(ED.Hydro.Helpers)
library(gridExtra)
library(grid)
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php") # the path to my config.php

pftid <- 1000000131
pft_priors <- tbl(bety,"pfts_priors") %>% filter(pft_id == pftid) %>% pull(prior_id)
PFT_species <- tbl(bety, "pfts") %>% dplyr::rename(pft_id = id) %>% filter(pft_id == pftid) %>%
  inner_join(., tbl(bety, "pfts_species"), by = "pft_id") %>%
  inner_join(., tbl(bety, "species") %>% dplyr::rename(specie_id = id), by = "specie_id") %>%
  dplyr::select(one_of("pft_id", "name", "specie_id", "genus", "species", "scientificname")) %>%
  collect()
PFT_species_id <- PFT_species %>% pull(specie_id)

SLA_id <- tbl(bety, "variables") %>% filter(name == "SLA") %>% pull(id)
SLA_fit <- tbl(bety, "priors") %>% filter(variable_id == SLA_id) %>% filter(id %in% pft_priors) %>% collect()
SLA_prior <- data.frame(x = rdistn(SLA_fit, n = 100000))
DEFAULT.LEAF.C <- 0.48
SLA_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "SLA") * DEFAULT.LEAF.C

SLA_trait_data <- tbl(bety, "traits") %>% filter(variable_id == SLA_id, specie_id %in% PFT_species_id) %>% collect()
SLA_trait_data$user_id <- factor(SLA_trait_data$user_id, levels = c(1000000003, setdiff(unique(SLA_trait_data$user_id), 1000000003)))

# Begin plotting

p <- ggplot(SLA_trait_data) + theme_classic()

named_colors <- c("#e41a1c", "#984ea3", "#377eb8")
names(named_colors) <- c("Full data dist", "Prior dist", "User 1-3 dist")
p1 <- p +
  geom_density(aes(x = mean, fill = "Full data dist"), alpha = .5) +
  coord_flip()  +
  scale_y_reverse() +
  xlab("SLA") +
  geom_density(data = SLA_prior, aes(x = x, fill = "Prior dist"), alpha = .5) +
  geom_density(data = SLA_trait_data %>% filter(user_id == 1000000003), aes(x = mean, fill = "User 1-3 dist"), alpha = .5) +
  theme(legend.position = c(0.45, 0.9),
        legend.background = element_blank(),
        legend.title = element_blank()) +
  scale_fill_manual(values = named_colors) +
  geom_vline(aes(xintercept = SLA_default), size = 1)


p2 <- p + geom_boxplot(aes(x = as.factor(site_id), y = mean)) +
  geom_jitter(aes(x = as.factor(site_id), y = mean, color = as.factor(citation_id)), width = .05, size = 3, alpha = .4) +
  geom_hline(aes(yintercept = SLA_default), size = 1) +
  xlab("Site id") + ylab("SLA") + labs(color = "Citation id")

lay <- rbind(c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2),
             c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2))

###############

SLA_1 <- arrangeGrob(p1,p2, layout_matrix = lay, top = textGrob("SLA prior in purple, data distribution in gray", gp=gpar(fontsize=20)))

plot(SLA_1)

ggsave(file.path("/fs/data3/ecowdery/ED.Hydro/SLA1.png"), SLA_1, width = 12, height = 6)

SLA_2 <- p2 + theme(axis.text.x = element_text(angle = 90))

ggsave(file.path("/fs/data3/ecowdery/ED.Hydro/SLA2.png"), SLA_2, width = 12, height = 6)

SLA_3 <- ggplot(SLA_trait_data %>% filter(site_id %in% c(1000000105,1000005005))) +
  geom_boxplot(aes(x = as.factor(site_id), y = mean)) +
  geom_jitter(aes(x = as.factor(site_id), y = mean, color = as.factor(citation_id)), width = .05, size = 3, alpha = .4) +
  # theme(axis.text.x = element_text(angle = 90)) +
  xlab("Site id") + ylab("SLA") + labs(color = "Citation id")

grid.arrange(p1, SLA_3, nrow = 1)

ggsave(file.path("/fs/data3/ecowdery/ED.Hydro/SLA3.png"), SLA_3, width = 12, height = 6)

p2 + xlim("1000005005")


##########
p2 <- p + geom_boxplot(aes(x = user_id, y = mean)) +
  geom_jitter(aes(x = user_id, y = mean, color = as.factor(citation_id)), width = .05, size = 3, alpha = .4) +
  # theme(axis.text.x = element_text(angle = 90)) +
  geom_hline(aes(yintercept = SLA_default), size = 1) +
  xlab("User id") + ylab("SLA") + labs(color = "Citation id")

lay <- rbind(c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2),
             c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2))

SLA_4 <- arrangeGrob(p1,p2, layout_matrix = lay, top = textGrob("SLA m2 kg-1", gp=gpar(fontsize=20)))

plot(SLA_4)

ggsave(file.path("/fs/data3/ecowdery/ED.Hydro/SLA4.png"), SLA_4, width = 12, height = 6, )

