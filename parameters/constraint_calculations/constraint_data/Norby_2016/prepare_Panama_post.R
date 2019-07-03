# Prepare the template data

#------------------------------------------------------------------------------#
# Setup

# Loading my package also loads any necessary pecan packages that are needed
library(ED.Hydro.Helpers)

# Other optional settings
options(digits = 10) # I set digits to 10 so it's easier to read bety ids
options(geonamesUsername = "ecowdery") # this can be anything, "pecan" probably works or just use mine
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php") # the path to my config.php

datapath <- "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Norby_2016/"
datafile <- "Panama_post"

#------------------------------------------------------------------------------#
# Read in the data

dat_in <- read.csv(file.path(datapath, paste0(datafile,".csv")), na.strings = NaN,  stringsAsFactors = FALSE)

dat <- dat_in %>%
  mutate(species = str_trim(str_to_sentence(Species)), Species = NULL)

#------------------------------------------------------------------------------#
# Only use tree data

dat <- dat %>% filter(Growth.form == "tree")

#------------------------------------------------------------------------------#
# References

all_doi <- "10.1111/nph.14319"
bibs <- insert_dois(all_doi) # insert_dois() is one of my functions

dat$citation_id <- bibs[[1]]$citation_id

tmp1 <- file.path(datapath, "tmp", paste0(datafile,"_tmp1", ".csv"))
write.csv(dat, file = tmp1)

#------------------------------------------------------------------------------#
# Sites

tmp1 <- file.path(datapath, "tmp", paste0(datafile,"_tmp1", ".csv"))
dat <- read.csv(file = tmp1, na.strings = NaN,  stringsAsFactors = FALSE) %>%
  select(-one_of("X","X.1","X.2"))

dat <- dat %>% mutate(site_id = case_when(Site.Name == "PNM" ~ 2000000002,
                               Site.Name == "SLZ" ~ 2000000003))

all(!is.na(dat$site_id))
tmp2 <- file.path(datapath, "tmp", paste0(datafile,"_tmp2", ".csv"))
write.csv(dat, file = tmp2)

#------------------------------------------------------------------------------#
# Species

tmp2 <- file.path(datapath, "tmp", paste0(datafile,"_tmp2", ".csv"))
dat <- read.csv(file = tmp2, na.strings = NaN,  stringsAsFactors = FALSE) %>%
  select(-one_of("X","X.1"))

dat <- dat %>% filter(!species == "Unknown")

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

ggplot(sp) + geom_bar(aes(x = case))
case_accept_set = c(7,8,12)
sp <- insert_species(sp, case_accept_set, bety)

sp %>% filter(reviewed == FALSE) %>% select(one_of("submit_name", "case"))


sp <- insert_species(sp = sp, case_accept_set = case_accept_set, insert_PFT = TRUE, bety = bety)


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

which(dat$SLA < min(SLA_prior))
which(dat$SLA > max(SLA_prior))

var <- "SLA"
varid <- SLA_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("value" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), variable_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

SLA_df <- df

## WD
##

dat <- dat %>% mutate(wood_density = WD)

wood_density_id <- tbl(bety, "variables") %>% filter(name == "wood_density") %>% pull(id)
wood_density_fit <- tbl(bety, "priors") %>% filter(variable_id == wood_density_id) %>% filter(id == 1000000422) %>% collect()

wood_density_prior <- rdistn(wood_density_fit)
wood_density_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "rho")

p <- prior_plot(prior = wood_density_prior,
                plot_default = wood_density_default,
                title = sprintf("(wood_density): %s", wood_density_fit$distn),
                type = "prior")
p + geom_density(data = dat, aes(x = wood_density, fill = "obs"), alpha = .3, color = NA)

which(dat$wood_density < min(wood_density_prior))
which(dat$wood_density > max(wood_density_prior))
range(dat$wood_density, na.rm = T)

var <- "wood_density"
varid <- wood_density_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("value" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), variable_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

wood_density_df <- df

## Cft to leaf and wood water cap

unique(dat$Organ)

dat <- dat %>% mutate(water_cap =  case_when(
  Organ == "Leaf" ~ "leaf_water_cap",
  Organ == "Xylem" ~ "wood_water_cap"
))

dat <- dat %>% mutate( Cft_conv = case_when(
  Organ == "Leaf" ~ Cft * (1/MPa2m) * 1000,
  Organ == "Xylem" ~ Cft * (1/MPa2m) * ((wdns/1000)/wood_density)
))

ggplot(dat) + geom_density(aes(x = Cft_conv, col = Organ))

dat <- dat %>%
  mutate(leaf_water_cap = case_when(water_cap == "leaf_water_cap" ~ Cft_conv,
                                    TRUE ~ NA_real_)) %>%
  mutate(wood_water_cap = case_when(water_cap == "wood_water_cap" ~ Cft_conv,
                                    TRUE ~ NA_real_))

ggplot(dat) + geom_density(aes(x = leaf_water_cap)) + geom_density(aes(x = wood_water_cap))


## Leaf Water Capacitance
## Leaf: g H2O g-1 dry weight MPa-1

leaf_water_cap_id <- tbl(bety, "variables") %>% filter(name == "leaf_water_cap") %>% pull(id)
leaf_water_cap_fit <- tbl(bety, "priors") %>% filter(variable_id == leaf_water_cap_id) %>% collect()
leaf_water_cap_prior <- rdistn(leaf_water_cap_fit)
leaf_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "leaf_water_cap")*1000

p <- prior_plot(prior = leaf_water_cap_prior,
                q = c(0,.975),
                plot_default = leaf_water_cap_default,
                title = sprintf("(leaf_water_cap): %s", leaf_water_cap_fit$distn),
                type = "prior")

p + geom_density(data = dat, aes(x = leaf_water_cap, fill = "obs"), alpha = .3, color = NA)

which(dat$leaf_water_cap < min(leaf_water_cap_prior))
which(dat$leaf_water_cap > max(leaf_water_cap_prior))

var <- "leaf_water_cap"
varid <- leaf_water_cap_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("value" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), variable_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

leaf_water_cap_df <- df

## Wood water capacitance
## Sapwood: kg m-3 MPa-1
## (~1/(1000*WD*MPa2M I guess)

wood_water_cap_id <- tbl(bety, "variables") %>% filter(name == "wood_water_cap") %>% pull(id)
wood_water_cap_fit <- tbl(bety, "priors") %>% filter(variable_id == wood_water_cap_id) %>% collect()
wood_water_cap_prior <- rdistn(wood_water_cap_fit)
wood_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "wood_water_cap")*1000

p <- prior_plot(prior = wood_water_cap_prior,
                q = c(0,.99),
                plot_default = wood_water_cap_default,
                title = sprintf("(wood_water_cap): %s", wood_water_cap_fit$distn),
                type = "prior")

p + geom_density(data = dat, aes(x = wood_water_cap, fill = "obs"), alpha = .3, color = NA)

dat$y <- 0

p.dat <- as.data.frame(wood_water_cap_prior)

ggplot() +
  geom_density(data = p.dat, aes(x = wood_water_cap_prior), fill = "gray", alpha = .5, color = NA) +
  geom_point(data = dat, aes(x = wood_water_cap, y = y, color = as.factor(citation_id)), alpha = .8) +
  theme_bw() + xlim(0,24.5)

ggplot() +
  geom_histogram(data = dat, aes(x = wood_water_cap,  y=..density.., fill = as.factor(citation_id)), position = "stack", alpha = .8) +
  geom_density(data = p.dat, aes(x = wood_water_cap_prior), fill = "black", alpha = .5, color = NA) +
  theme_bw() + xlim(0,24.5)

which(dat$wood_water_cap < min(wood_water_cap_prior))
which(dat$wood_water_cap > max(wood_water_cap_prior))

var <- "wood_water_cap"
varid <- wood_water_cap_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("value" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), variable_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

wood_water_cap_df <- df


