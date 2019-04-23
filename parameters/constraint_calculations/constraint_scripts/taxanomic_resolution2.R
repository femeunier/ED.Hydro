# Deal with cases

library(taxize)
library(knitr)
library(ED.Hydro.Helpers)

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

pftid <- 1000000131
PFT_species <- tbl(bety, "pfts") %>% dplyr::rename(pft_id = id) %>% filter(pft_id == pftid) %>%
  inner_join(., tbl(bety, "pfts_species"), by = "pft_id") %>%
  inner_join(., tbl(bety, "species") %>% dplyr::rename(specie_id = id), by = "specie_id") %>%
  dplyr::select(one_of("pft_id", "name", "specie_id", "genus", "species", "scientificname")) %>%
  collect()

sp = read.csv(
  file = "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species.csv",
  stringsAsFactors = FALSE) %>% select(-one_of("X", "X.1"))
sp$submit_name <- str_to_sentence(sp$submit_name)
sp <- sp[order(sp$case),]

m <- sp %>%
  select(one_of("submit_name", "accept_name", "case")) %>%
  select(-one_of("sourceid", "authority", "score"))
m$matched_name2 <- NA
m$current_name_string <- NA
m$betsy <- NA

for(j in seq_along(m$submit_name)){
  r <- gnr_resolve(names = m$submit_name[j], with_context = TRUE, canonical = TRUE, fields = "all")

  if("current_name_string" %in% names(r)){
    m$current_name_string[j] <- r %>% pull(current_name_string) %>% unique() %>% paste(., collapse = ", ")
  }

  m$matched_name2[j] <- r %>% pull(matched_name2) %>% unique() %>% paste(., collapse = ", ")
  cat("*")
}

# I'm changing the case of three of names because I don't agree with the auto resolving


# Now translating back to the sp matrix
# I should automate this but blugh

id <- which(sp$submit_name == "Simarouba glauca")
sp[id,]
sp[id, "accept_name"] = sp[id, "submit_name"]
sp[id, "accept_bety_id"] = sp[id, "submit_bety_id"]
sp[id, "accept_in_PFT"] = sp[id, "submit_in_PFT"]
sp[id, "case"] = 2
sp[id, "uri"] =

id <- which(sp$submit_name == "Aegiphila lhotzkiana")
sp[id,]
sp[id, "accept_name"] = sp[id, "submit_name"]
sp[id, "accept_bety_id"] = sp[id, "submit_bety_id"]
sp[id, "accept_in_PFT"] = sp[id, "submit_in_PFT"]
sp[id, "case"] = 2

id <- which(sp$submit_name == "Lonchocarpus muehlbergianus")
sp[id,]
sp[id, "accept_name"] = sp[id, "submit_name"]
sp[id, "accept_bety_id"] = sp[id, "submit_bety_id"]
sp[id, "accept_in_PFT"] = sp[id, "submit_in_PFT"]
sp[id, "case"] = 11

# Now to think about case 13 which will essentially become case 11

names <- c()

for(q in which(sp$case == 13)){
  sp[q, "accept_name"] = sp[q, "submit_name"]
  sp[q, "accept_bety_id"] = sp[q, "submit_bety_id"]
  sp[q, "accept_in_PFT"] = sp[q, "submit_in_PFT"]
  sp[q, "case"] = 11
}

id <- which(sp$submit_name == "Simarouba glauca")
sp[id,]
sp[id, "accept_name"] = sp[id, "submit_name"]
sp[id, "accept_bety_id"] = sp[id, "submit_bety_id"]
sp[id, "accept_in_PFT"] = sp[id, "submit_in_PFT"]
sp[id, "case"] = 2
sp[id, "uri"] =










write.csv(sp, file = "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species_2.csv")
