# Prepare the template data

#------------------------------------------------------------------------------#
# Setup

# Loading my package also loads any necessary pecan packages that are needed
library(ED.Hydro.Helpers)

# Other optional settings
options(digits = 10) # I set digits to 10 so it's easier to read bety ids
options(geonamesUsername = "ecowdery") # this can be anything, "pecan" probably works or just use mine
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php") # the path to my config.php

#------------------------------------------------------------------------------#
# Read in the data

datapath <- "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Capacitance_Felicien_2-15-19/"
datafile <- "capacitance_BETY"

dat_in <- read.csv(file.path(datapath, paste0(datafile,".csv")), na.strings = NaN,  stringsAsFactors = FALSE)

dat <- dat_in %>%
  mutate(species = str_trim(str_to_sentence(Species)), Species = NULL) %>%
  rename(lat = Lat, lon = Long)

#------------------------------------------------------------------------------#
# Data I'm removing for various reasons,
# some of these decisions may be wrong and we can come back to them,
# but if I don't feel comfortable adding it in to the database, I won't.


#------------------------------------------------------------------------------#
# References

# make a separate dataframe for working on getting refs in to the database
refs <- dat %>%
  mutate(citation_id = as.numeric(NA), doi = NA) %>%
  select(one_of("Ref", "doi", "citation_id")) %>%
  distinct
# View(refs)

refs[which(refs$Ref == "Hao et al. 2008"),"doi"] <- "10.1007/s00442-007-0918-5"
refs[which(refs$Ref == "Meinzer et al 2008"),"doi"] <- "10.1093/treephys/28.11.1609"
refs[which(refs$Ref == "Werden et al 2018"),"doi"] <- "10.1093/treephys/tpx135"
refs[which(refs$Ref == "Borchert & Pockman 2005"),"doi"] <- "10.1093/treephys/25.4.457"
refs[which(refs$Ref == "Carrasco et al. 2014"),"doi"] <- "10.1093/treephys/tpu087"
refs[which(refs$Ref == "De Guzman et al. 2016 + unpublished data"),"doi"] <- "10.1093/treephys/tpw086"
refs[which(refs$Ref == "Domec et al. 2006a"),"doi"] <- "10.1111/j.1365-3040.2005.01397.x"
refs[which(refs$Ref == "Machado & Tyree 1994"),"doi"] <- "10.1093/treephys/14.3.219"
refs[which(refs$Ref == "Meinzer et al. 2003"),"doi"] <- "10.1046/j.1365-3040.2003.01039.x"
refs[which(refs$Ref == "Meinzer et al. 2008"),"doi"] <- "10.1007/s00442-008-0974-5"
refs[which(refs$Ref == "Scholz et al. 2007"),"doi"] <- "10.1093/treephys/27.4.551"
refs[which(refs$Ref == "Tyree et al. 1991"),"doi"] <- "10.1104/pp.96.4.1105"

refs$doi <- str_trim(refs$doi) # because I always miss something

all_doi <- unique(na.omit(refs$doi))
bibs <- insert_dois(all_doi) # insert_dois() is one of my functions

for(i in seq_along(bibs)){
  idx <- which(tolower(refs$doi) == tolower(bibs[[i]]$doi)) #DOI's are case insensitive
  authors <- bibs[[i]]$author %>% str_split(" ") %>% unlist()
  author <- authors %>% head(min(length(authors), 5)) %>% clean_chars() # clean_chars() is one of my functions
  year <- bibs[[i]]$year
  print(paste0(i," | ",unique(refs[idx, "Ref"])))
  print(bibs[[i]]$key)
  print(str_match(unique(refs[idx, "Ref"]), author))
  print(str_match(unique(refs[idx, "Ref"]), year))
  print("--------------------------------------------")
  refs$citation_id[idx] <- bibs[[i]]$citation_id
}

length(unique(refs$doi))
length(unique(refs$citation_id))

dat <- left_join(dat,refs, by = "Ref") %>% select(-one_of("Ref", "doi"))

tmp1 <- file.path(datapath, "tmp", paste0(datafile,"_tmp1", ".csv"))
write.csv(dat, file = tmp1)

#------------------------------------------------------------------------------#
# Sites

dat <- read.csv(file = tmp1, na.strings = NaN,  stringsAsFactors = FALSE) %>%
  select(-one_of("X","X.1"))

# Resolve a few things by hand before running the automated function

# Data from citation_id = 1000000103 are given the wrong coordinates
dat[which(dat$citation_id == 1000000103), "lat"] <- 10.451983
dat[which(dat$citation_id == 1000000103), "lon"] <- -85.127044

# Some coordinates from citation_id = 1000000137 are hard to distinguish between two sites
dat[which(dat$citation_id == 1000000137 & dat$species == "Ficus insipida"), "lat"] <- 9.280225
dat[which(dat$citation_id == 1000000137 & dat$species == "Ficus insipida"), "lon"] <- -79.975527

# make a separate dataframe to work on getting sites in to the database

dat_site_cite <- dat %>%
  mutate(site_id = as.numeric(NA)) %>%
  select(one_of("citation_id", "lat", "lon", "site_id")) %>% distinct


####
# Run my function cite2site()

# First pass chek for exact matches
dat_site_cite <- cite2site(dat_site_cite, interval = 0)
dat_out_na <- dat_site_cite %>% filter(is.na(site_id)) %>%
  select(one_of("citation_id", "lat", "lon")) %>% distinct

View(dat_out_na)
View(dat_site_cite)

# One final coarse search
dat_site_cite <- cite2site(dat_site_cite, interval = .4)
dat_out_na <- dat_site_cite %>% filter(is.na(site_id)) %>%
  select(one_of("citation_id", "lat", "lon")) %>% distinct

# Merge everything together
dat <- left_join(dat, dat_site_cite, by = c("lat", "lon", "citation_id")) %>%
  select(-one_of("lat", "lon"))

all(!is.na(dat$site_id))

tmp2 <- file.path(datapath, "tmp", paste0(datafile,"_tmp2", ".csv"))
write.csv(dat, file = tmp2)

#------------------------------------------------------------------------------#
# Species

dat <- read.csv(file = tmp2, na.strings = NaN,  stringsAsFactors = FALSE) %>%
  select(-one_of("X","X.1"))

# Fix species names that I know won't work with the taxonomic name resolver
dat[which(dat$species == "Tachigalia versicolor"), "species"] <- "Tachigali versicolor"

###

pftid <- 1000000131
PFT_species <- tbl(bety, "pfts") %>% dplyr::rename(pft_id = id) %>% filter(pft_id == pftid) %>%
  inner_join(., tbl(bety, "pfts_species"), by = "pft_id") %>%
  inner_join(., tbl(bety, "species") %>% dplyr::rename(specie_id = id), by = "specie_id") %>%
  dplyr::select(one_of("pft_id", "name", "specie_id", "genus", "species", "scientificname")) %>%
  collect()
species_all <- sort(unique(tolower(dat$species)))

sp <- taxanomic_resolution(species_all = species_all,
                           PFT_species = PFT_species,
                           bety = bety)


case_accept_set = c(7,8,12)
sp <- insert_species(sp, case_accept_set, bety)

sp %>% filter(reviewed == FALSE) %>% select(one_of("submit_name", "case"))

sp[which(sp$submit_name == "antirrhea trichantha"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "aspidosperma cruenta"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "bursera simarouba"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "chrysophllum cainito"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "hymenaea stignocarpa"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "hymenea courbaril"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "miconia pohliana"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "myrsine ferruginea"), "reviewed"] <- TRUE

sp <- insert_species(sp, case_accept_set, bety)


all(!is.na(sp$bety_id))
all(sp$in_PFT)

dat <- left_join(dat, sp %>% transmute(species = str_to_sentence(submit_name), species_id = bety_id)) %>% select(-species)

tmp3 <- file.path(datapath, "tmp", paste0(datafile,"_tmp3", ".csv"))
write.csv(dat, file = tmp3)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Preparation of data

# Useful conversions (straight from ED)
wdns <- 1.000e3
grav <- 9.80665
MPa2m <- wdns / grav

tmp3 <- file.path(datapath, "tmp", paste0(datafile,"_tmp3", ".csv"))
dat <- read.csv(file = tmp3, na.strings = NaN,  stringsAsFactors = FALSE) %>% select(-one_of("X","X.1"))

dat <- dat %>% mutate(Cft = as.numeric(Cft),
                      SLA = as.numeric(SLA),
                      WD = as.numeric(WD))



## SLA
## Don't need to do any conversions

SLA_id <- tbl(bety, "variables") %>% filter(name == "SLA") %>% pull(id)
SLA_fit <- tbl(bety, "priors") %>% filter(variable_id == SLA_id) %>% filter(id == 142) %>% collect()
SLA_prior <- rdistn(SLA_fit)
SLA_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "SLA")

p <- prior_plot(prior = SLA_prior,
                plot_default = SLA_default,
                title = sprintf("(SLA): %s", SLA_fit$distn),
                type = "prior")

p + geom_density(data = dat, aes(x = SLA, fill = "obs"), alpha = .3, color = NA)

var <- "SLA"
varid <- SLA_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("var" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), var_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

SLA_df <- df

## WD
##

dat <- dat %>% mutate(wood_density = WD)

wood_density_id <- tbl(bety, "variables") %>% filter(name == "wood_density") %>% pull(id)
wood_density_fit <- tbl(bety, "priors") %>% filter(variable_id == wood_density_id) %>% filter(id == 1000000281) %>% collect()

wood_density_prior <- rdistn(wood_density_fit)
wood_density_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "rho")

p <- prior_plot(prior = wood_density_prior,
                plot_default = wood_density_default,
                title = sprintf("(wood_density): %s", wood_density_fit$distn),
                type = "prior")
p + geom_density(data = dat, aes(x = wood_density, fill = "obs"), alpha = .3, color = NA)

mean(dat$WD, na.rm = TRUE)

var <- "wood_density"
varid <- wood_density_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("var" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), var_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

wood_density_df <- df

## Cft to leaf and wood water cap

unique(dat$Organ)

dat <- dat %>% mutate(water_cap =  case_when(
  Organ == "Leaf" ~ "leaf_water_cap",
  Organ == "Xylem" ~ "wood_water_cap"
))

dat <- dat %>% mutate( Cft_conv = case_when(
  Organ == "Leaf" ~ Cft * 1/MPa2m * 1000,
  Organ == "Xylem" ~ Cft * 1/(MPa2m * 1000 * .5)
))

ggplot(dat) + geom_density(aes(x = Cft_conv, col = Organ))

dat <- dat %>%
  mutate(leaf_water_cap = case_when(water_cap == "leaf_water_cap" ~ Cft_conv,
                                    TRUE ~ NA_real_)) %>%
  mutate(wood_water_cap = case_when(water_cap == "wood_water_cap" ~ Cft_conv,
                                    TRUE ~ NA_real_))

ggplot(dat) + geom_density(aes(x = leaf_water_cap)) + geom_density(aes(x = wood_water_cap))


## Leaf Water Capacitance
## Leaf: gH2O g-1 dry weight MPa-1

leaf_water_cap_id <- tbl(bety, "variables") %>% filter(name == "leaf_water_cap") %>% pull(id)
leaf_water_cap_fit <- tbl(bety, "priors") %>% filter(variable_id == leaf_water_cap_id) %>% collect()
leaf_water_cap_prior <- rdistn(leaf_water_cap_fit)
leaf_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "leaf_water_cap")

p <- prior_plot(prior = leaf_water_cap_prior/1000,
                q = c(0,.975),
                plot_default = leaf_water_cap_default,
                title = sprintf("(leaf_water_cap): %s", leaf_water_cap_fit$distn),
                type = "prior")

p + geom_density(data = dat, aes(x = leaf_water_cap/1000, fill = "obs"), alpha = .3, color = NA)

var <- "leaf_water_cap"
varid <- leaf_water_cap_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("var" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), var_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

leaf_water_cap_df <- df

## Wood water capacitance
## Sapwood: kg m-3 MPa-1
## (~1/(1000*WD*MPa2M I guess)

wood_water_cap_id <- tbl(bety, "variables") %>% filter(name == "wood_water_cap") %>% pull(id)
wood_water_cap_fit <- tbl(bety, "priors") %>% filter(variable_id == wood_water_cap_id) %>% collect()
wood_water_cap_prior <- rdistn(wood_water_cap_fit)
wood_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "wood_water_cap")

p <- prior_plot(prior = wood_water_cap_prior/1000,
                plot_default = wood_water_cap_default,
                title = sprintf("(wood_water_cap): %s", wood_water_cap_fit$distn),
                type = "prior")

p + geom_density(data = dat, aes(x = wood_water_cap, fill = "obs"), alpha = .3, color = NA)

var <- "wood_water_cap"
varid <- wood_water_cap_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("var" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), var_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

wood_water_cap_df <- df


