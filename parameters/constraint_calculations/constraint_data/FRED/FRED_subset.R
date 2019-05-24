library(PEcAn.all)
library(tidyverse)
library(rcrossref)
library(rgbif)

source("/fs/data3/ecowdery/FRED/project_functions.R")

################################################################################
## Get species

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

pftid <- 1000000132

tropical_species <- tbl(bety, "pfts") %>% dplyr::rename(pft_id = id) %>% filter(pft_id == pftid) %>%
  inner_join(., tbl(bety, "pfts_species"), by = "pft_id") %>%
  inner_join(., tbl(bety, "species") %>% dplyr::rename(specie_id = id), by = "specie_id") %>%
  dplyr::select(one_of("pft_id", "name", "specie_id", "genus", "species", "scientificname")) %>%
  collect()


################################################################################
# Import and do major subsetting of FRED

FRED_in <- read_csv("/fs/data3/ecowdery/FRED/FRED2_20180518.csv")
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

# Traits we will be entering in to the database
# For now I'll focus on one variable, below are more I'm interested in
#
# "Rooting depth", # F00954
# "Rooting depth_Interpolated 50 percent rooting depth", # F00957
# "Rooting depth_Interpolated 95 percent rooting depth", # F00958
# "Belowground biomass per ground area", # F00885
# "Aboveground biomass per ground area", # F01267
# "Fine root mass/leaf mass ratio"       # F00843 no stats
# "Specific root area (SRA)"             # F00718 yes stats

traits <- c(
  "Specific root area (SRA)" #F00718
) %>% str_replace_all("([[:punct:]])|\\s+","_")

stats <- c(
  ID2name(FRED_in, "F00719"),
  ID2name(FRED_in, "F00720"),
  ID2name(FRED_in, "F00721"),
  ID2name(FRED_in, "F00722"),
  ID2name(FRED_in, "F00723")
) %>% str_replace_all("([[:punct:]])|\\s+","_")

keep_cols <- c(traits, stats, meta, ident)
keep_cols %in% names(FRED_main)

FRED_main <- FRED_main %>% dplyr::select(one_of(keep_cols))
FRED_main <- clean_NAs(FRED_main, traits)

if(length(stats) > 0){
  names(FRED_main)[grep("^n_*", names(FRED_main))] <- "n"
  names(FRED_main)[grep("^SE_*", names(FRED_main))] <- "SE"
  names(FRED_main)[grep("^SD_*", names(FRED_main))] <- "SD"
  names(FRED_main)[grep("^Median_*", names(FRED_main))] <- "median"
  names(FRED_main)[grep("^Upper_quartile_*", names(FRED_main))] <- "Q3"
  names(FRED_main)[grep("^Lower_quartile_*", names(FRED_main))] <- "Q1"
  # Fill out the other stats later
  names(FRED_main)[grep("^n_*", names(FRED_main))] <- "n"
  names(FRED_main)[grep("^n_*", names(FRED_main))] <- "n"
  names(FRED_main)[grep("^n_*", names(FRED_main))] <- "n"
  names(FRED_main)[grep("^n_*", names(FRED_main))] <- "n"
  names(FRED_main)[grep("^n_*", names(FRED_main))] <- "n"
  names(FRED_main)[grep("^n_*", names(FRED_main))] <- "n"
  names(FRED_main)[grep("^n_*", names(FRED_main))] <- "n"
  names(FRED_main)[grep("^n_*", names(FRED_main))] <- "n"
}

tbl(bety, "traits") %>% pull(statname) %>% unique()

################################################################################
# Import and do major subsetting of FRED

FRED <- FRED_main
# FRED <- FRED %>% dplyr::select(one_of(str_replace_all(meta, "([[:punct:]])|\\s+","_")))

# First pass subset but not actually complete
FRED <- FRED %>%
  filter(Plant_taxonomy_Genus %in% tropical_species$genus | Accepted_genus_TPL %in% tropical_species$genus) %>%
  filter(Plant_taxonomy_Species %in% tropical_species$species | Accepted_species_TPL %in% tropical_species$species)

FRED <- FRED %>%
  mutate(agreement = case_when(
    Plant_taxonomy_Genus == Accepted_genus_TPL & Plant_taxonomy_Species == Accepted_species_TPL ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(genus = case_when(
    Plant_taxonomy_Genus == Accepted_genus_TPL ~ Plant_taxonomy_Genus,
    TRUE ~ ""
  )) %>%
  mutate(species = case_when(
    Plant_taxonomy_Species == Accepted_species_TPL ~ Plant_taxonomy_Species,
    TRUE ~ ""
  ))

b <- sum(FRED$genus == "" & FRED$species == "")
g <- sum(FRED$genus != "" & FRED$species == "")
s <- sum(FRED$genus == "" & FRED$species != "")
sum(FRED$agreement == FALSE)
b + g + s

(b*4) + (g*2) + (s*2)

good <- FRED %>% filter(agreement == TRUE)
good_join <- good %>% inner_join(tropical_species, by = c("genus", "species"))

problem <- FRED %>% filter(agreement == FALSE)
dim(problem)

fixed <- matrix(0, ncol = ncol(FRED) + 1, nrow = 0) %>% as.data.frame()
colnames(fixed) <- c(names(FRED), "idx")

for(i in 1:nrow(problem)){
  old <- problem[i,] %>% dplyr::select(-one_of(c("genus", "species")))

  genus <- c(old$Plant_taxonomy_Genus,   old$Accepted_genus_TPL)
  species <- c(old$Plant_taxonomy_Species, old$Accepted_species_TPL)
  all_pairs <- crossing(genus, species)
  idx <- rep(i, nrow(all_pairs)) # shows which row they originally came from so we can check that more than one wasn't selected in the join
  new <- cbind(old, all_pairs, idx)
  fixed <- rbind.data.frame(fixed, new)
}

fixed %>% dim

fixed_join <- inner_join(fixed, tropical_species, by = c("genus", "species"))
# View(fixed_join)
which(duplicated(fixed_join$idx)) # Do that check for duplicates.
fixed_join <- distinct(fixed_join, idx, .keep_all = TRUE) %>% select(-idx) # I'm a little scared by this step. Make sure to check it by hand

FRED_JOIN <- rbind.data.frame(good_join, fixed_join)

## Cleanup

FRED_JOIN <- FRED_JOIN %>%
  dplyr::select(-one_of(c("Plant_taxonomy_Genus", "Accepted_genus_TPL",
                          "Plant_taxonomy_Species", "Accepted_species_TPL")))

FRED_JOIN <- clean_NAs(FRED_JOIN, traits)
write.csv(FRED_JOIN, file = "/fs/data3/ecowdery/FRED/FRED_sub.csv")

################################################################################
## Citations

# I'm so confused. rcrossref isn't working in rstudio.
# I think it's a problem with the addin
# Just go run Rscript /fs/data3/ecowdery/FRED/FRED_citations.R in the command line
# file.edit("/fs/data3/ecowdery/FRED/FRED_citations.R")

# source("/fs/data3/ecowdery/FRED/FRED_citations.R")
# system2("Rscript /fs/data3/ecowdery/FRED/FRED_citations.R")


FRED <- FRED_JOIN

unique(FRED$Data_source_DOI)

load("/fs/data3/ecowdery/FRED/FRED_citations.Rds")

FRED_JOIN <- FRED_JOIN %>% mutate(SRA = as.numeric(Specific_root_area__SRA_)/5)
ggplot(FRED_JOIN) + geom_density(aes(x = SRA))
mean(FRED_JOIN$SRA)

dc <- list()

for(i in seq_along(bibs)){

  check_doi <- tbl(bety, "citations") %>% filter(doi == bibs[[i]]$doi) %>% collect()

  if(nrow(check_doi) == 0){
    author <- bibs[[i]]$author %>% clean_chars()
    year <- as.numeric(bibs[[i]]$year)
    title <- bibs[[i]]$title %>% clean_chars()
    journal <- bibs[[i]]$journal %>% clean_chars()
    vol <- as.numeric(bibs[[i]]$volume)
    pg <- bibs[[i]]$pages %>% str_replace(pattern = "--",replacement = "-") %>% str_squish
    url <- bibs[[i]]$url %>% str_squish
    doi <- bibs[[i]]$doi %>% str_squish

    insert.query <- sprintf("INSERT INTO citations (author, year, title, journal, vol, url, doi, user_id, created_at, updated_at) VALUES('%s', %.0f, '%s', '%s', %.0f, '%s', '%s', 1000000003, NOW(), NOW()) RETURNING id;",
                            author, year, title, journal, vol, url, doi)

    citation_id <- db.query(insert.query, bety$con)
    sprintf("Citation %10.0f added to BETY", citation_id)
  }else{
    citation_id <- check_doi$id
    print(sprintf("Citation %10.0f already in BETY", citation_id))
  }
  dc[[i]] = data.frame(doi = bibs[[i]]$doi, citation_id = citation_id, stringsAsFactors = FALSE)
}

doi_citation <- do.call(rbind.data.frame, dc)
options(digits = 10)
doi_citation

FRED$Data_source_DOI

FRED <- inner_join(FRED, doi_citation, by = c("Data_source_DOI" = "doi"))
View(FRED)
################################################################################
## Sites

dat_sites <- FRED %>% select(one_of("Latitude_main", "Longitude_Main", "Data_source_DOI", "citation_id")) %>% distinct()
dat_sites
dim(dat_sites)
site_list <- list()

dat_sites <- FRED_in %>%
  filter(`Data source_DOI` %in% dat_sites$Data_source_DOI) %>%
  select(one_of(
    c("Latitude_main",
      "Longitude_Main",
      "Latitude",
      "Longitude",
      "Latitude_Estimated",
      "Longitude_Estimated",
      "Min_Latitude",
      "Max_Latitude",
      "Min_Longitude",
      "Max_Longitude"
    )
  )) %>% distinct() %>% inner_join(dat_sites)


dat_sites$site_id <- NA

for(i in 1:nrow(dat_sites)){

  lat <- as.numeric(dat_sites[i,"Latitude_main"])
  lon <- as.numeric(dat_sites[i,"Longitude_Main"])
  test1 <- nearby_sites(lat, lon, interval = 0, bety$con)
  if(nrow(test1) == 1){
    dat_sites[i, "site_id"] <- test1$id
  }else{

    print(paste("Nothing conclusive found for", dat_sites[i, "location"]))
    print(paste("Lookup:", dat_sites[i, "doi"]))
    print(nearby_sites(lat, lon, interval = .5, bety$con))

  }
}
glimpse(dat_sites)

FRED <- inner_join(FRED, dat_sites %>% select(one_of("citation_id", "site_id")))

################################################################################
## Treatment

# I'm assuming everything is in situ observation

FRED$treatment <- 2000000012

################################################################################
## Finally put things in BETY

####################
# Specific Root Area

var_id <- 1000000292


in_dat <- FRED
in_dat$variable_id <- var_id

for(i in 1:nrow(FRED)){
  check <- tbl(bety, "traits") %>%
    filter(site_id == in_dat$site_id[i]) %>%
    filter(specie_id == in_dat$specie_id[i]) %>%
    filter(citation_id == in_dat$citation_id[i]) %>%
    filter(treatment_id == in_dat$treatment[i]) %>%
    filter(variable_id == in_dat$variable_id[i]) %>%
    collect()
  check <- check %>% filter(near(mean, in_dat$value[i]))

  if(dim(check)[1] == 0){

    insert.query <- sprintf("INSERT INTO traits (site_id, specie_id, citation_id, treatment_id, variable_id, mean, user_id, access_level, created_at, updated_at) VALUES(%.0f, %.0f, %.0f, %.0f, %.0f, %.10f, 'Imported from BAAD version %s', 1000000003, 4, NOW(), NOW()) RETURNING id;",
                            in_dat$site_id[i], in_dat$specie_id[i],
                            in_dat$citation_id[i], in_dat$treatment[i],
                            in_dat$variable_id[i], in_dat$value[i],
                            baad.data::baad_data_version_current())

    trait_id <- db.query(insert.query, bety$con)
    print(paste("Inserted", trait_id))

  }else{
    trait_id <- check$id
    print(paste("Found", trait_id))
    insert.query2 <- sprintf("UPDATE traits SET notes = 'Imported from BAAD version %s' WHERE id = %.0f RETURNING id;",
                             baad.data::baad_data_version_current(), trait_id)
  }
  trait_id <- db.query(insert.query2, bety$con)
  in_dat$trait_id[i] <- trait_id

}

tbl(bety, "traits") %>% pull(statname) %>% unique()

SRA <- as.numeric(in_dat$Specific_root_area__SRA_)*2 %>% udunits2::ud.convert(.,"cm^2/g", "m^2/kg")

plot(density(SRA))
abline(v = 50)

