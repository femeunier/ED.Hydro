# Prepare the FRED data

#------------------------------------------------------------------------------#
# Setup

# Loading my package also loads any necessary pecan packages that are needed
library(ED.Hydro.Helpers)

# Other optional settings
options(digits = 10) # I set digits to 10 so it's easier to read bety ids
options(geonamesUsername = "ecowdery") # this can be anything, "pecan" probably works or just use mine
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php") # the path to my config.php

datapath <- "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/FRED"
datafile <- "FRED2_20180518.csv"

FRED_in <- read_csv(file.path(datapath,datafile))
FRED_in %>% dim()
FRED_in %>% distinct() %>% dim()

FRED_main <- FRED_in[-(1:4),]
names(FRED_main) <- str_replace_all(names(FRED_main), "([[:punct:]])|\\s+","_")

# I've set this up so that you can just copy/paste in column names from the documentation
# without having to worry about spaces or punctuation
# The regex ([[:punct:]])|\\s+ then replaces all problematic characters with "_"

# Meta data such as sitation and site
meta <- c(
  "Data source_DOI",
  "Data set_DOI",
  "Notes_Site ID"
) %>% str_replace_all("([[:punct:]])|\\s+","_")

# Larger identifiers such as taxonomy, PFT, growth form
ident <- c(
  "Plant taxonomy_Genus",
  "Accepted genus_TPL",
  "Plant taxonomy_Species",
  "Accepted species_TPL",
  "Climate_PFT_Biome equivalent_Poulter",
  "Plant growth form",
  "Latitude_main", # F01185
  "Longitude_Main" # F01186
) %>% str_replace_all("([[:punct:]])|\\s+","_")

traits <- c(
  "Fine root mass/leaf mass ratio"
) %>% str_replace_all("([[:punct:]])|\\s+","_")

keep_cols <- c(traits, meta, ident)
keep_cols %in% names(FRED_main)

FRED_main <- FRED_main %>% dplyr::select(one_of(keep_cols))
FRED_main <- clean_NAs(FRED_main, traits)

dat <- FRED_main %>%
  mutate(lat = as.numeric(Latitude_main), Latitude_main = NULL,
         lon = as.numeric(Longitude_Main), Longitude_Main = NULL) %>%
  rename(doi = Data_source_DOI, biome = Climate_PFT_Biome_equivalent_Poulter)

#------------------------------------------------------------------------------#
# Species

pftid <- 1000000131
PFT_species <- tbl(bety, "pfts") %>% dplyr::rename(pft_id = id) %>% filter(pft_id == pftid) %>%
  inner_join(., tbl(bety, "pfts_species"), by = "pft_id") %>%
  inner_join(., tbl(bety, "species") %>% dplyr::rename(specie_id = id), by = "specie_id") %>%
  dplyr::select(one_of("pft_id", "name", "specie_id", "genus", "species", "scientificname")) %>%
  collect()

dat <- dat %>% mutate(gen = case_when(!is.na(Accepted_genus_TPL) ~ Accepted_genus_TPL,
                                        is.na(Accepted_genus_TPL) ~ Plant_taxonomy_Genus),
                      spec = case_when(!is.na(Accepted_species_TPL) ~ Accepted_species_TPL,
                                         is.na(Accepted_species_TPL) ~ Plant_taxonomy_Species),
                      species = paste(gen, spec)) %>%
  select(-one_of("Accepted_genus_TPL", "Accepted_species_TPL",
                 "Plant_taxonomy_Genus", "Plant_taxonomy_Species", "gen", "spec"))
dat <- dat[-grep("NA NA", dat$species),] # some data doesn't have any species identification

# Just checking baout biomes

unique(dat$biome)

sp_polar <- dat %>% filter(biome == "polar") %>% pull(species) %>% unique()
length(sp_polar); sum(sp_polar %in% PFT_species$scientificname)
sp_boreal <- dat %>% filter(biome == "boreal") %>% pull(species) %>% unique()
length(sp_boreal); sum(sp_boreal %in% PFT_species$scientificname)
sp_temperate <- dat %>% filter(biome == "temperate") %>% pull(species) %>% unique()
length(sp_temperate); sum(sp_temperate %in% PFT_species$scientificname)
sp_tropical <- dat %>% filter(biome == "tropical") %>% pull(species) %>% unique()
length(sp_tropical); sum(sp_tropical %in% PFT_species$scientificname)

intersect(sp_tropical, sp_polar)
intersect(sp_tropical, sp_boreal)
intersect(sp_tropical, sp_temperate)

# So in this case there are no overlapping species whis is great :)
# I'm just going to subset to the tropical species for the moment,
# though I might go back and insert all the data if I feel like it later...

# dat <- dat %>% filter(biome == "tropical")

species_all <- sort(unique(tolower(dat$species)))
sp <- taxanomic_resolution(species_all = species_all,
                           PFT_species = PFT_species,
                           bety = bety)

# In this case, I may not be interested in adding species to the PFT,
# I should ask Mike about it. But I can still see if there are species
# that I can review

ggplot(sp) + geom_bar(aes(x = as.factor(case)))

case_accept_set = c(7,8,12)
sp <- insert_species(sp, case_accept_set, bety)

# This is a special case where we will choose not to include the
# things that we need to review

idx <- which(!sp$reviewed)

sp2 <- sp %>% filter(sp$reviewed)
sp2 <- insert_species(sp2, case_accept_set, insert_PFT = FALSE, bety)

dat <- left_join(dat, sp2 %>% transmute(species = str_to_sentence(submit_name), species_id = bety_id)) %>% select(-species)
dat <- dat %>% filter(is.na(species_id))

ggplot(dat) + geom_density(aes(x = as.numeric(Fine_root_mass_leaf_mass_ratio)))
