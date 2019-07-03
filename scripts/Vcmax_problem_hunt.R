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

Vcmax_id <- tbl(bety, "variables") %>% filter(name == "Vcmax") %>% pull(id)
Vcmax_fit <- tbl(bety, "priors") %>% filter(variable_id == Vcmax_id) %>% filter(id %in% pft_priors) %>% collect()
Vcmax_prior <- data.frame(x = rdistn(Vcmax_fit, n = 100000))


Vcmax_trait_data <- tbl(bety, "traits") %>% filter(variable_id == Vcmax_id, specie_id %in% PFT_species_id) %>% collect()




p <- ggplot(Vcmax_trait_data)

p1 <- p +
  geom_density(aes(x = mean), fill = "gray") +
  coord_flip()  +
  scale_y_reverse() +
  xlab("Vcmax") +
  # geom_vline(aes(xintercept = mean(mean)), size = 1) +
  geom_density(data = Vcmax_prior, aes(x = x), fill = "purple", alpha = .3)

p2 <- p + geom_boxplot(aes(x = as.factor(site_id), y = mean)) +
  geom_jitter(aes(x = as.factor(site_id), y = mean, color = as.factor(citation_id)), width = .05, size = 3, alpha = .4) +
  # theme(axis.text.x = element_text(angle = 90)) +
  xlab("Site id") + ylab("Vcmax") + labs(color = "Citation id")

lay <- rbind(c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2),
             c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2))

test <- arrangeGrob(p1,p2, layout_matrix = lay, top = textGrob("Vcmax prior in purple, data distribution in gray", gp=gpar(fontsize=20)))

ggsave(file.path("/fs/data3/ecowdery/ED.Hydro/Vcmax1.png"), test, width = 12, height = 6)

Vcmax_2 <- p + geom_boxplot(aes(x = as.factor(site_id), y = mean)) +
  geom_jitter(aes(x = as.factor(site_id), y = mean, color = as.factor(citation_id)), width = .05, size = 3, alpha = .4) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Site id") + ylab("Vcmax") + labs(color = "Citation id")

ggsave(file.path("/fs/data3/ecowdery/ED.Hydro/Vcmax2.png"), Vcmax_2, width = 12, height = 6)

p2 + xlim("1000005005")
