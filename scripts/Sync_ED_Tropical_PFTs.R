# Make sure that all species in ED_Tropical match that of ED_Tropical_Hydro

pft_hydro <- 1000000131
pft_norm <- 1000000132

sp_hydro <- tbl(bety, "pfts") %>% dplyr::rename(pft_id = id) %>% filter(pft_id == pft_hydro) %>%
  inner_join(., tbl(bety, "pfts_species"), by = "pft_id") %>%
  inner_join(., tbl(bety, "species") %>% dplyr::rename(specie_id = id), by = "specie_id") %>%
  dplyr::select(one_of("pft_id", "name", "specie_id", "genus", "species", "scientificname")) %>%
  collect()

sp_norm <- tbl(bety, "pfts") %>% dplyr::rename(pft_id = id) %>% filter(pft_id == pft_norm) %>%
  inner_join(., tbl(bety, "pfts_species"), by = "pft_id") %>%
  inner_join(., tbl(bety, "species") %>% dplyr::rename(specie_id = id), by = "specie_id") %>%
  dplyr::select(one_of("pft_id", "name", "specie_id", "genus", "species", "scientificname")) %>%
  collect()

all(sp_norm$specie_id %in% sp_hydro$specie_id)
all(sp_hydro$specie_id %in% sp_norm$specie_id)

sp_new <- setdiff(sp_hydro$specie_id, sp_norm$specie_id)

for(i in seq_along(sp_new)){
  insert.query <- sprintf("INSERT INTO pfts_species (pft_id, specie_id, created_at, updated_at) VALUES(1000000132, %.0f, NOW(), NOW());", sp_new[i])
  db.query(insert.query, bety$con)
}

# Next do some sort of check that the priors between the two models are matching

priors_hydro <- tbl(bety, "pfts_priors") %>%
  filter(pft_id == pft_hydro) %>%
  select(one_of("pft_id", "prior_id")) %>%
  left_join(tbl(bety,"priors") %>%
              rename(prior_id = id) %>%
              select(-one_of("notes", "created_at", "updated_at")) %>%
              left_join(tbl(bety, "variables") %>%
                          rename (variable_id = id))) %>%
  select(one_of("name", "variable_id", "prior_id")) %>%
  rename(variable_name = name, prior_id_hydro = prior_id) %>%
  collect()

priors_norm <- tbl(bety, "pfts_priors") %>%
  filter(pft_id == pft_norm) %>%
  select(one_of("pft_id", "prior_id")) %>%
  left_join(tbl(bety,"priors") %>%
              rename(prior_id = id) %>%
              select(-one_of("notes", "created_at", "updated_at")) %>%
              left_join(tbl(bety, "variables") %>%
                          rename (variable_id = id))) %>%
  select(one_of("name", "variable_id", "prior_id")) %>%
  rename(variable_name = name, prior_id_norm = prior_id) %>%
  collect()

View(priors_norm)

priors_all <- full_join(priors_hydro, priors_norm)
priors_all <- priors_all %>% mutate(agree = (prior_id_hydro == prior_id_norm))
View(priors_all)
