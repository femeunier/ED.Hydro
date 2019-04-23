# Deal with cases

library(taxize)
library(knitr)
library(ED.Hydro.Helpers)

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")
pftid <- 1000000131

sp = read.csv(
  file = "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species_final.csv",
  stringsAsFactors = FALSE)
sp <- sp %>% select(-X)
sp <- sp[order(sp$case),]

unique(sp$case)

case_accept_set <- c(7,8,12)

sp <- sp %>%
  mutate(use = case_when(case %in% case_accept_set ~ "accept",
                         TRUE ~ "submit")) %>%
  mutate(bety_name = case_when(case %in% case_accept_set ~ accept_name,
                               TRUE ~ submit_name)) %>%
  mutate(bety_id = case_when(case %in% case_accept_set ~ accept_bety_id,
                             TRUE ~ submit_bety_id)) %>%
  mutate(in_PFT = case_when(case %in% case_accept_set ~ accept_in_PFT,
                            TRUE ~ submit_in_PFT))

final <- sp %>% select(one_of("submit_name", "bety_name", "bety_id", "in_PFT", "case", "uri")) %>%
  mutate(bety_name = str_to_sentence(bety_name)) %>%
  mutate(scientificname = bety_name) %>%
  separate(bety_name, c("genus", "species"), sep = " ", extra = "drop")

################################################################################
# CAUTION THESE INSET QUERIES DO NOT HAVE CHECKS RIGHT NOW

insert_species_idx <- which(is.na(final$bety_id))

for(i in insert_species_idx){
0101
  insert.species.query <- sprintf("INSERT INTO species (genus, species, scientificname, notes, created_at, updated_at) VALUES('%s', '%s', '%s', '%s', NOW(), NOW()) RETURNING id;",
                   final$genus[i], final$species[i], final$scientificname[i], final$uri[i])
  species_id <- db.query(insert.species.query, bety$con)

  final$bety_id[i] <- tbl(bety, "species") %>% filter(scientificname == final$scientificname[i]) %>% collect() %>% pull(id)

  insert.pft.query <- sprintf("INSERT INTO pfts_species (pft_id, specie_id, created_at, updated_at) VALUES(%.0f, %.0f, NOW(), NOW()) RETURNING id;",
                             pftid, species_id$id)
  db.query(insert.pft.query, bety$con)

  d <- tbl(bety, "pfts_species") %>% filter(pft_id == pftid) %>% filter(specie_id == species_id$id) %>% collect()
  final$in_PFT[i] <- dim(d)[1]==1
}

insert_pft_idx <- which(!final$in_PFT)

for(j in insert_pft_idx){
  insert.pft.query <- sprintf("INSERT INTO pfts_species (pft_id, specie_id, created_at, updated_at) VALUES(%.0f, %.0f, NOW(), NOW()) RETURNING id;",
                              pftid, final$bety_id[j])
  db.query(insert.pft.query, bety$con)

  d <- tbl(bety, "pfts_species") %>% filter(pft_id == pftid) %>% filter(specie_id == final$bety_id[j]) %>% collect()
  final$in_PFT[j] <- dim(d)[1]==1
}

write.csv(final, file = "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species_final.csv")
