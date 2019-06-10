# For the root_beta, if you can observations of both Rooting depth (F00954) and Rooting depth_Extrapolated 50 percent rooting depth (or Rooting depth_Extrapolated 95 percent rooting depth) you can derive it from: root_beta = exp(log(1-alpha)*z_max/z_alpha)

# where alpha is the fraction (0.5 or 0.95), z_max is the rooting_depth (F00954) and z_alpha is the corresponding rooting depth at a certain depth (F00959 for alpha = 0.5 or F00960 for alpha = 0.95)


ID2name <- function(FRED_in, id){
  return(names(FRED_in)[which(FRED_in[3,] == id)])
}
name2ID <- function(FRED_in, name){
  return(FRED_in[3,name])
}

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
datafile <- "FRED2_20180518"

FRED_in <- read_csv(paste0(file.path(datapath,datafile),".csv"))
FRED_in %>% dim()
FRED_in %>% distinct() %>% dim()

library(ggplot2)
ggplot(FRED_in) + geom_density(aes(x = as.numeric(`Specific root area (SRA)`)))


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
  "Climate_Koeppen-Geiger classification",
  "Climate",
  "Plant growth form",
  "Latitude_main", # F01185
  "Longitude_Main" # F01186
) %>% str_replace_all("([[:punct:]])|\\s+","_")

traits <- c(
  "Rooting depth",
  "Rooting depth_Extrapolated 50 percent rooting depth",
  "Rooting depth_Extrapolated 95 percent rooting depth"

) %>% str_replace_all("([[:punct:]])|\\s+","_")

keep_cols <- c(traits, meta, ident)
keep_cols %in% names(FRED_main)

FRED_main <- FRED_main %>% dplyr::select(one_of(keep_cols))
FRED_main <- clean_NAs(FRED_main, traits)

dat_ready <- FRED_main %>%
  mutate(lat = as.numeric(Latitude_main), Latitude_main = NULL,
         lon = as.numeric(Longitude_Main), Longitude_Main = NULL,
         rooting_depth = as.numeric(Rooting_depth),
         rooting_depth_50 = as.numeric(Rooting_depth_Extrapolated_50_percent_rooting_depth),
         rooting_depth_95 = as.numeric(Rooting_depth_Extrapolated_95_percent_rooting_depth)) %>%
  mutate(doi = case_when(!is.na(Data_source_DOI) ~ Data_source_DOI,
                         is.na(Data_source_DOI) ~ Data_set_DOI)) %>%
  filter(Climate_PFT_Biome_equivalent_Poulter %in% c("tropical", "temperate"))


# For the root_beta, if you can observations of both Rooting depth (F00954) and Rooting depth_Extrapolated 50 percent rooting depth (or Rooting depth_Extrapolated 95 percent rooting depth) you can derive it from: root_beta = exp(log(1-alpha)*z_max/z_alpha)

# where alpha is the fraction (0.5 or 0.95), z_max is the rooting_depth (F00954) and z_alpha is the corresponding rooting depth at a certain depth (F00959 for alpha = 0.5 or F00960 for alpha = 0.95)

dat.50 <- dat_ready %>%
  mutate(z_max = rooting_depth,
         alpha = 0.5,
         z_alpha = rooting_depth_50,
         root_beta = exp(log(1-alpha)*z_max/z_alpha)
  ) %>%
  select(one_of("lat","lon", "Climate_Koeppen_Geiger_classification", "Climate_PFT_Biome_equivalent_Poulter",
                "root_beta","Accepted_genus_TPL", "Accepted_species_TPL",
                "Plant_taxonomy_Genus", "Plant_taxonomy_Species", "doi"))

dat.95 <- dat_ready %>%
  mutate(z_max = rooting_depth,
         alpha = 0.95,
         z_alpha = rooting_depth_95,
         root_beta = exp(log(1-alpha)*z_max/z_alpha)
  ) %>%
  select(one_of("lat","lon", "Climate_Koeppen_Geiger_classification", "Climate_PFT_Biome_equivalent_Poulter",
                "root_beta","Accepted_genus_TPL", "Accepted_species_TPL",
                "Plant_taxonomy_Genus", "Plant_taxonomy_Species", "doi"))


test <- dat_ready %>% filter(!is.na(rooting_depth))
sum(!is.na(test$Rooting_depth_Extrapolated_50_percent_rooting_depth))
sum(!is.na(test$Rooting_depth_Extrapolated_95_percent_rooting_depth))




ggplot(dat%>% filter(Climate_PFT_Biome_equivalent_Poulter == "tropical")) +
  geom_density(aes(x = rooting_depth_50, fill = Climate_Koeppen_Geiger_classification), alpha = .3) +
  xlim(0,1) + theme_bw() + theme(legend.position = "bottom")

ggplot(dat%>% filter(Climate_PFT_Biome_equivalent_Poulter == "tropical")) +
  geom_density(aes(x = rooting_depth_95, fill = Climate_Koeppen_Geiger_classification), alpha = .3) +
  xlim(0,1) + theme_bw() + theme(legend.position = "bottom")


ggplot(dat %>% filter(Climate_PFT_Biome_equivalent_Poulter == "tropical")) +
  geom_density(aes(x = rooting_depth, fill = Climate_Koeppen_Geiger_classification), alpha = .3) +
  xlim(0,1) + theme_bw() + theme(legend.position = "bottom")

length(which(dat$Climate_PFT_Biome_equivalent_Poulter == "tropical"))
length(which(dat$Climate_Koeppen_Geiger_classification %in% c("Af", "Am")))
length(which(dat$Climate_Koeppen_Geiger_classification %in% c("As", "Aw")))
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
                      gen = case_when(is.na(gen) ~ "", TRUE ~ gen),
                      spec= case_when(is.na(spec) ~ "", TRUE ~ spec),
                      species = str_trim(paste(gen, spec))) %>%
  select(-one_of("Accepted_genus_TPL", "Accepted_species_TPL",
                 "Plant_taxonomy_Genus", "Plant_taxonomy_Species", "gen", "spec"))
dat <- dat %>% filter(species != "")  # some data doesn't have any species identification


# Felicien and I have decieded to take all the species that are
# Climate_Koeppen-Geiger classification: Af an Am
# and add them to the analysis.

# dat <- dat %>% filter(biome == "tropical")
dat <- dat %>% filter(Climate_Koeppen_Geiger_classification %in% c("Af", "Am"))

ggplot(dat) +
  geom_density(aes(x = fineroot2leaf, fill = Climate_Koeppen_Geiger_classification), alpha = .3) +
  xlim(0,1) + theme_bw() + theme(legend.position = "bottom")

species_all <- sort(unique(tolower(dat$species)))
sp <- taxanomic_resolution(species_all = species_all,
                           PFT_species = PFT_species,
                           bety = bety)

ggplot(sp) + geom_bar(aes(x = as.factor(case)))

case_accept_set = c(7,8,12)
sp <- insert_species(sp, case_accept_set, bety)

idx <- which(!sp$reviewed)
sp$reviewed[idx] <- TRUE

sp <- insert_species(sp, case_accept_set, insert_PFT = TRUE, bety)

all(!is.na(sp$bety_id))
all(sp$in_PFT)

dat <- left_join(dat, sp %>% transmute(species = str_to_sentence(submit_name), species_id = bety_id))
all(!is.na(dat$species_id))

tmp1 <- file.path(datapath, "tmp", paste0(datafile,"_tmp1", ".csv"))
write.csv(dat, file = tmp1)

#------------------------------------------------------------------------------#
# References

tmp1 <- file.path(datapath, "tmp", paste0(datafile,"_tmp1", ".csv"))
dat <- read.csv(file = tmp1, na.strings = NaN,  stringsAsFactors = FALSE) %>% select(-X)

# make a separate dataframe for working on getting refs in to the database
refs <- dat %>%
  mutate(citation_id = as.numeric(NA)) %>%
  select(one_of("doi", "citation_id")) %>%
  distinct()

all_doi <- unique(na.omit(refs$doi))
bibs <- insert_dois(all_doi)

for(i in seq_along(bibs)){
  idx <- which(tolower(refs$doi) == tolower(bibs[[i]]$doi)) #DOI's are case insensitive
  for(j in idx){
    refs$citation_id[j] <- bibs[[i]]$citation_id
  }
}

dat <- left_join(dat,refs) %>% select(-one_of("doi"))

all(!is.na(dat$citation_id))

tmp2 <- file.path(datapath, "tmp", paste0(datafile,"_tmp2", ".csv"))
write.csv(dat, file = tmp2)

#------------------------------------------------------------------------------#
# Sites

tmp2 <- file.path(datapath, "tmp", paste0(datafile,"_tmp2", ".csv"))
dat <- read.csv(file = tmp2, na.strings = NaN,  stringsAsFactors = FALSE) %>%
  select(-one_of("X"))

dat_site_cite <- dat %>%
  mutate(site_id = as.numeric(NA)) %>%
  select(one_of("citation_id", "lat", "lon", "site_id")) %>%
  distinct %>%
  arrange(citation_id)

dat_site_cite <- cite2site(dat_site_cite, interval = .4)
dat_out_na <- dat_site_cite %>% filter(is.na(site_id)) %>%
  select(one_of("citation_id", "lat", "lon")) %>% distinct

dat <- left_join(dat, dat_site_cite, by = c("lat", "lon", "citation_id")) %>%
  select(-one_of("lat", "lon"))

all(!is.na(dat$site_id))

tmp3 <- file.path(datapath, "tmp", paste0(datafile,"_tmp3", ".csv"))
write.csv(dat, file = tmp3)


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Preparation of data


tmp3 <- file.path(datapath, "tmp", paste0(datafile,"_tmp3", ".csv"))
dat <- read.csv(file = tmp3, na.strings = NaN,  stringsAsFactors = FALSE) %>% select(-one_of("X","X.1"))

dat$y <- 0


pft_priors <- tbl(bety,"pfts_priors") %>% filter(pft_id == pftid) %>% pull(prior_id)


fineroot2leaf_id <- tbl(bety, "variables") %>% filter(name == "fineroot2leaf") %>% pull(id)
fineroot2leaf_fit <- tbl(bety, "priors") %>% filter(variable_id == fineroot2leaf_id)  %>% filter(id %in% possible_priors) %>% collect()



fineroot2leaf_prior <- rdistn(fineroot2leaf_fit, n = 100000)
fineroot2leaf_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "q")

max(fineroot2leaf_prior) > max(dat$fineroot2leaf, na.rm = TRUE)


p <- prior_plot(prior = fineroot2leaf_prior,
                q = c(0,.95),
                plot_default = fineroot2leaf_default,
                title = sprintf("(fineroot2leaf): %s", fineroot2leaf_fit$distn),
                type = "prior")

p + geom_density(data = dat, aes(x = fineroot2leaf, fill = "obs"), alpha = .3, color = NA) + geom_point(data = dat, aes(x = fineroot2leaf, y = y))

which(dat$fineroot2leaf < min(fineroot2leaf_prior))
which(dat$fineroot2leaf > max(fineroot2leaf_prior))

var <- "fineroot2leaf"
varid <- fineroot2leaf_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("value" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), variable_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))


plot_dat <- dat %>% mutate(mean = Fine_root_mass_leaf_mass_ratio)


p <- ggplot(plot_dat)

p1 <- p +
  geom_density(aes(x = mean)) +
  coord_flip()  +
  scale_y_reverse() +
  xlab("fineroot2leaf") +
  geom_vline(aes(xintercept = mean(mean)), size = 1, color = "gray")

p2 <- p +
  geom_boxplot(aes(x = as.factor(site_id), y = mean)) +
  geom_jitter(aes(x = as.factor(site_id), y = mean, color = as.factor(citation_id)), width = .05, size = 3, alpha = .4) +
  xlab("Site id") + ylab("fineroot2leaf") +
  theme(legend.title = element_blank())


lay <- rbind(c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2),
             c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2))

grid.arrange(p1,p2, layout_matrix = lay, top = textGrob("New Subset of BAAD database", gp=gpar(fontsize=20)))

