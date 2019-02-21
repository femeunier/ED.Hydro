bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

pftid <- 1000000132

tropical_species <- tbl(bety, "pfts") %>% dplyr::rename(pft_id = id) %>% filter(pft_id == pftid) %>%
  inner_join(., tbl(bety, "pfts_species"), by = "pft_id") %>% 
  inner_join(., tbl(bety, "species") %>% dplyr::rename(specie_id = id), by = "specie_id") %>% 
  dplyr::select(one_of("pft_id", "name", "specie_id", "genus", "species", "scientificname")) %>% 
  collect()

variables <- c(
  "leaf_elastic_mod", "wood_elastic_mod",
  "leaf_psi_osmotic", "wood_psi_osmotic",
  "rwc_tlp_wood", 
  "leaf_water_sat", "wood_water_sat",
  "leaf_water_cap", "wood_water_cap",
  "leaf_psi_min", "wood_psi_min",
  "wood_Kmax",
  "wood_psi50",
  "wood_Kexp"
)

variables_ids <- lapply(variables, function(v) tbl(bety, "variables") %>% filter(name == v) %>% pull(id)) %>% unlist()

v_df <- data.frame(variables, variables_ids)

species_ids <- unique(tropical_species$specie_id)

class(variables_ids[[1]])
class(species_ids[1])

trait_data <- tbl(bety, "traits") %>% filter(specie_id %in% species_ids) %>% 
  filter(variable_id %in% variables_ids) %>% collect()






