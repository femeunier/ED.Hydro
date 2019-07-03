# Prepare the Psitlp data

#------------------------------------------------------------------------------#
# Setup

# Loading my package also loads any necessary pecan packages that are needed
library(ED.Hydro.Helpers)
library(plantecophys)

# Other optional settings
options(digits = 10) # I set digits to 10 so it's easier to read bety ids
options(geonamesUsername = "ecowdery") # this can be anything, "pecan" probably works or just use mine
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php") # the path to my config.php

datapath <- "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Lin_2015//"
datafile <- "tropicaldata"

#------------------------------------------------------------------------------#
# Read in the data

dat_in <- read.csv(file.path(datapath, paste0(datafile,".csv")), na.strings = NaN,  stringsAsFactors = FALSE)

dat <- dat_in %>%
  mutate(species = str_trim(str_to_sentence(Species)), Species = NULL) %>%
  dplyr::rename(lat = latitude, lon = longitude)

# I'm making two major changes early on that may need to be revised if Shawn
# doesn't agree with them

# (1) I'm just removing all the weird "repeat" and "rep" from species names ...

length(unique(dat$species))
dat$species <- str_replace(dat$species , " repeat$", "")
length(unique(dat$species))
dat$species <- str_replace(dat$species, " rep$", "")
length(unique(dat$species))

# (2) I'm only using the data from Daintree because the other data is giving me
# really strange numbers! I need help figurin it out, but for now,
# I'm just adding something to constrain the prior
# ALSO MORE IMPORTANTLY, this data set lets me calculate stomatal slope
# for species instead of grouping by site or some other arbitrary group

unique(dat$Location)

dat <- dat %>% filter(Location == "Daintree rainforest canopy crane_QLD_AU")

#------------------------------------------------------------------------------#
# References

all_doi <- "10.1038/nclimate2550"
bibs <- insert_dois(all_doi) # insert_dois() is one of my functions

dat$citation_id <- bibs[[1]]$citation_id[[1]]

tmp1 <- file.path(datapath, "tmp", paste0(datafile,"_tmp1", ".csv"))
write.csv(dat, file = tmp1)

#------------------------------------------------------------------------------#
# Sites

tmp1 <- file.path(datapath, "tmp", paste0(datafile,"_tmp1", ".csv"))
dat <- read.csv(file = tmp1, na.strings = NaN,  stringsAsFactors = FALSE)

# make a separate dataframe to work on getting sites in to the database

dat_site_cite <- dat %>%
  mutate(site_id = as.numeric(NA)) %>%
  select(one_of("citation_id", "lat", "lon", "site_id", "Location")) %>%
  distinct %>%
  arrange(citation_id)

####
# Run my function cite2site()

# First pass chek for exact matches
dat_site_cite <- cite2site(dat_site_cite, interval = 0)
dat_out_na <- dat_site_cite %>% filter(is.na(site_id)) %>%
  select(one_of("citation_id", "lat", "lon", "Location")) %>% distinct

# dat_out_na %>% mutate("elev" = geonames::GNsrtm3(lat,lon)[[1]]) %>% distinct
# elevation queried from lat lon using R geonames::GNstrm3

# View(dat_out_na)
# View(dat_site_cite)

# One final coarse search
dat_site_cite <- cite2site(dat_site_cite, interval = .4)
dat_out_na <- dat_site_cite %>% filter(is.na(site_id)) %>%
  select(one_of("citation_id", "lat", "lon", "Location")) %>% distinct

# Merge everything together
dat <- left_join(dat, dat_site_cite, by = c("lat", "lon", "citation_id")) %>%
  select(-one_of("lat", "lon"))

all(!is.na(dat$site_id))

tmp2 <- file.path(datapath, "tmp", paste0(datafile,"_tmp2", ".csv"))
write.csv(dat, file = tmp2)

#------------------------------------------------------------------------------#
# Species

tmp2 <- file.path(datapath, "tmp", paste0(datafile,"_tmp2", ".csv"))
dat <- read.csv(file = tmp2, na.strings = NaN,  stringsAsFactors = FALSE) %>%
  select(-one_of("X","X.1"))

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
# sp <- taxanomic_resolution(sp = sp,
#                            PFT_species = PFT_species,
#                            bety = bety)

ggplot(sp) + geom_bar(aes(x = as.factor(case)))

case_accept_set = c(7,8,12)

sp <- insert_species(sp, case_accept_set, bety)

sp <- sp %>% select(one_of("reviewed", "case",
                           "submit_name", "submit_bety_id", "submit_in_PFT",
                           "accept_name", "accept_bety_id", "accept_in_PFT",
                           "proposed_bety_name", "bety_name", "use",
                           "sourceid", "uri", "score"
))

# Make sure to check the "proposed bety name."
# That is what will ultimately be put in to BETY (as opposed to the submit name.)

sp[which(sp$submit_name == "acmena graveolens"), "bety_name"] <- "syzygium graveolens"
sp[which(sp$submit_name == "acmena graveolens"), "reviewed"]  <- TRUE

sp[which(sp$submit_name == "elaeocarpus grandis"), "bety_name"] <- "elaeocarpus angustifolius"
sp[which(sp$submit_name == "elaeocarpus grandis"), "reviewed"]  <- TRUE

sp[which(sp$submit_name == "argyrodendron peralatum"), "bety_name"] <- "argyrodendron peralatum"
sp[which(sp$submit_name == "argyrodendron peralatum"), "reviewed"]  <- TRUE

sp[which(sp$submit_name == "pouteria torta subsp  tuberculata"), "bety_name"] <- "pouteria torta-tuberculata"
sp[which(sp$submit_name == "pouteria torta subsp  tuberculata"), "reviewed"]  <- TRUE

sp <- insert_species(sp, case_accept_set, insert_PFT = TRUE, bety)

all(!is.na(sp$bety_id))
all(sp$in_PFT)

dat <- left_join(dat, sp %>% transmute(species = str_to_sentence(submit_name), species_id = bety_id)) %>% select(-species)
all(!is.na(dat$species_id))

tmp3 <- file.path(datapath, "tmp", paste0(datafile,"_tmp3", ".csv"))
write.csv(dat, file = tmp3)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Preparation of data

pftid <- 1000000131
pft_priors <- tbl(bety,"pfts_priors") %>% filter(pft_id == pftid) %>% pull(prior_id)


# Useful conversions (straight from ED)
wdns <- 1.000e3
grav <- 9.80665
MPa2m <- wdns / grav

tmp3 <- file.path(datapath, "tmp", paste0(datafile,"_tmp3", ".csv"))
dat <- read.csv(file = tmp3, na.strings = NaN,  stringsAsFactors = FALSE) %>%
  select(-one_of("X","X.1")) %>%
  mutate(VpdL = VPD, y = 0)
dat_site <- split(dat,dat$site_id)
dat_fitgroup <- split(dat,dat$fitgroup)
dat_species <- split(dat,dat$species_id)

##############
# Medlyn

fit.BBOpti <- lapply(dat_species, fitBB,
                     gsmodel = "BBOpti",
                     varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL",
                                     Ca = "CO2S", RH = "RH"),
                     fitg0 = FALSE)

coef.BBOpti <- bind_rows(
  lapply(fit.BBOpti, FUN = function(x){
    data.frame(stomatal_slope.g1 = summary(x$fit)$parameters["g1",1],
               SE = summary(x$fit)$parameters["g1",2],
               n = x$n,
               species_id =  unique(x$data$species_id))
  }))

# Compare the fits

coef(fitBB(dat,
           varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL",
                           Ca = "CO2S", RH = "RH"),
           gsmodel = c("BBOpti"),
           fitg0 = FALSE))
coef.BBOpti

# Merge the data

dat.BBOpti <- dat %>% left_join(coef.BBOpti, by = "species_id")

stomatal_slope.g1_id <- tbl(bety, "variables") %>% filter(name == "stomatal_slope.g1") %>% pull(id)
stomatal_slope.g1_fit <- tbl(bety, "priors") %>% filter(variable_id == stomatal_slope.g1_id) %>% filter(distn == "weibull") %>% collect()
stomatal_slope.g1_prior <- data.frame(x = rdistn(stomatal_slope.g1_fit, n = 100000))

ggplot() +
  geom_density(data = stomatal_slope.g1_prior, aes(x = x), fill = "black", alpha = .3) +
  geom_density(data = dat.BBOpti, aes(x = stomatal_slope.g1), fill = "darkblue", alpha = .5, color = NA) +
  geom_point(data = dat.BBOpti, aes(x = stomatal_slope.g1, y = y, color = as.factor(species_id)), shape = "|", size = 10) +
  theme(legend.position="bottom")

# Save the data

var <- "stomatal_slope.g1"
varid <- stomatal_slope.g1_id
df <- dat.BBOpti %>%
  select(one_of(var, "SE", "n", "species_id", "citation_id", "site_id")) %>%
  rename("value" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), variable_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))


##############
# Leuning

fit.BBLeuning <- lapply(dat_species, fitBB,
                        gsmodel = "BBLeuning",
                        varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL",
                                        Ca = "CO2S", RH = "RH"),
                        fitg0 = FALSE)

coef.BBLeuning <- bind_rows(
  lapply(fit.BBLeuning, FUN = function(x){
    data.frame(stomatal_slope = summary(x$fit)$parameters["g1",1],
               SE = summary(x$fit)$parameters["g1",2],
               n = x$n,
               species_id =  unique(x$data$species_id))
  }))

# Compare the fits

coef(fitBB(dat,
           varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL",
                           Ca = "CO2S", RH = "RH"),
           gsmodel = c("BBLeuning"),
           fitg0 = FALSE))
coef.BBLeuning

# Merge the data

dat.BBLeuning <- dat %>% left_join(coef.BBLeuning, by = "species_id")

stomatal_slope_id <- tbl(bety, "variables") %>% filter(name == "stomatal_slope") %>% pull(id)
stomatal_slope_fit <- tbl(bety, "priors") %>% filter(variable_id == stomatal_slope_id) %>%
  filter(id %in% pft_priors) %>% collect()
stomatal_slope_prior <- data.frame(x = rdistn(stomatal_slope_fit, n = 100000))
stomatal_slope_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "stomatal_slope")

ggplot() +
  geom_density(data = stomatal_slope_prior, aes(x = x), fill = "black", alpha = .3) +
  geom_density(data = dat.BBLeuning, aes(x = stomatal_slope), fill = "darkblue", alpha = .5, color = NA) +
  geom_point(data = dat.BBLeuning, aes(x = stomatal_slope, y = y, color = as.factor(species_id)), shape = "|", size = 10) +
  geom_vline(aes(xintercept = stomatal_slope_default)) +
  theme(legend.position="bottom")

# Save the data

var <- "stomatal_slope"
varid <- stomatal_slope_id
df <- dat.BBLeuning %>%
  select(one_of(var, "SE", "n", "species_id", "citation_id", "site_id")) %>%
  rename("value" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), variable_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

##############
# Ball-Berry

fit.BallBerry <- lapply(dat_species, fitBB,
                        gsmodel = "BallBerry",
                        varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL",
                                        Ca = "CO2S", RH = "RH"),
                        fitg0 = FALSE)

coef.BallBerry <- bind_rows(
  lapply(fit.BallBerry, FUN = function(x){
    data.frame(stomatal_slope.BB = summary(x$fit)$parameters["g1",1],
               SE = summary(x$fit)$parameters["g1",2],
               n = x$n,
               species_id =  unique(x$data$species_id))
  }))


# Compare the fits

coef(fitBB(dat,
           varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL",
                           Ca = "CO2S", RH = "RH"),
           gsmodel = c("BallBerry"),
           fitg0 = FALSE))
coef.BallBerry

# Merge the data

dat.BallBerry <- dat %>% left_join(coef.BallBerry, by = "species_id")

stomatal_slope.BB_id <- tbl(bety, "variables") %>% filter(name == "stomatal_slope.BB") %>% pull(id)
stomatal_slope.BB_fit <- tbl(bety, "priors") %>% filter(variable_id == stomatal_slope.BB_id) %>%
  filter(id == 2000000062) %>% collect()
stomatal_slope.BB_prior <- data.frame(x = rdistn(stomatal_slope.BB_fit, n = 100000))

ggplot() +
  geom_density(data = stomatal_slope.BB_prior, aes(x = x), fill = "black", alpha = .3) +
  geom_density(data = dat.BallBerry, aes(x = stomatal_slope.BB), fill = "darkblue", alpha = .5, color = NA) +
  geom_point(data = dat.BallBerry, aes(x = stomatal_slope.BB, y = y, color = as.factor(species_id)), shape = "|", size = 10) +
  theme(legend.position="bottom")

# Save the data

var <- "stomatal_slope.BB"
varid <- stomatal_slope.BB_id
df <- dat.BallBerry %>%
  select(one_of(var, "SE", "n", "species_id", "citation_id", "site_id")) %>%
  rename("value" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), variable_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))
