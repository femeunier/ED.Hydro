# Prepare the conductivity data

#------------------------------------------------------------------------------#
# Setup

# Loading my package also loads any necessary pecan packages that are needed
library(ED.Hydro.Helpers)

# Other optional settings
options(digits = 10) # I set digits to 10 so it's easier to read bety ids
options(geonamesUsername = "ecowdery") # this can be anything, "pecan" probably works or just use mine
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php") # the path to my config.php

# Useful conversions (straight from ED)
wdns <- 1.000e3
grav <- 9.80665
MPa2m <- wdns / grav

#------------------------------------------------------------------------------#
# Read in the data

datapath <- "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Conductivity_Felicien_4-23-19"
datafile <- "conductivity_BETY"

dat_in <- read.csv(file.path(datapath, paste0(datafile,".csv")), na.strings = NaN,  stringsAsFactors = FALSE)

dat <- dat_in %>%
  mutate(species = str_trim(str_to_sentence(Species)), Species = NULL) %>%
  rename(lat = Lat, lon = Long)

#------------------------------------------------------------------------------#
# Data I'm removing for various reasons,
# some of these decisions may be wrong and we can come back to them,
# but if I don't feel comfortable adding it in to the database, I won't.

# Remove eucalyptus
dat <- dat[-grep("Eucalyptus", dat$species),]
dat <- dat[-grep("Corymbia", dat$species),]

# Remove plants that were grown in a greenhouse? Maybe?
# Talk to Mike about this.
dat <- dat[-grep("Vander Willigen et al. 2000", dat$Ref),]
dat <- dat[-grep("Feild et al. 2009 \\+ Feild \\& Isnard", dat$Ref),]

# Remove data that I couldn't figure out where it came from
dat <- dat[-grep("Cochard 2006", dat$Ref),]

# Remove the data that is not published (remember to go back and ask about this later)
dat <- dat[-which(dat$Ref == "Van der Sande & Markesteijn et al. unpublished data"),]

#------------------------------------------------------------------------------#
# Fix data that is confirmed as having been copied over from papers incorrectly

dat[which(dat$Ref == "Choat et al. 2005" & dat$species == "Brachychiton australis"), "ksat"] <- 6.70
dat[which(dat$Ref == "Tyree & Sperry 1989" & dat$species == "Cassipourea guianensis"), "species"] <- "Cassipourea elliptica"

#------------------------------------------------------------------------------#
# References

# make a separate dataframe for working on getting refs in to the database
refs <- dat %>%
  mutate(citation_id = as.numeric(NA), doi = NA) %>%
  select(one_of("Ref", "doi", "citation_id")) %>%
  distinct
# View(refs)

refs[which(refs$Ref == "Brodribb et al. 2002"),"doi"] <- "10.1046/j.1365-3040.2002.00919.x"
refs[which(refs$Ref == "Brodribb et al. 2003"),"doi"] <- "10.1046/j.1365-3040.2003.00975.x"
refs[which(refs$Ref == "Bucci et al. 2004"),"doi"] <- "10.1093/treephys/24.8.891"
refs[which(refs$Ref == "Bucci et al. 2006"),"doi"] <- "10.1111/j.1365-3040.2006.01591.x"
refs[which(refs$Ref == "Bucci et al. 2008"),"doi"] <- "10.1590/S1677-04202008000300007"
refs[which(refs$Ref == "Chapotin et al. 2006"),"doi"] <- "10.1111/j.1365-3040.2005.01456.x"
refs[which(refs$Ref == "Chen et al. 2009a, b"),"doi"] <- "10.1007/s00425-009-0959-6"
refs[which(refs$Ref == "Chen et al. 2017"),"doi"] <- "10.1111/1365-2435.12724"
refs[which(refs$Ref == "Choat et al. 2005"),"doi"] <- "10.1007/s00468-004-0392-1"
refs[which(refs$Ref == "Choat et al. 2007"),"doi"] <- "10.1111/j.1469-8137.2007.02137.x"
# refs[which(refs$Ref == "Cochard 2006"),"doi"] <- "10.1016/j.crhy.2006.10.012"
refs[which(refs$Ref == "De Guzman et al. 2016"),"doi"] <- "10.1093/treephys/tpw086"
refs[which(refs$Ref == "Domec et al. 2006a"),"doi"] <- "10.1111/j.1365-3040.2005.01397.x"
refs[which(refs$Ref == "Ewers et al. 2004"),"doi"] <- "10.1093/treephys/24.9.1057"
refs[which(refs$Ref == "Feild & Balun 2008"),"doi"] <- "10.1111/j.1469-8137.2007.02306.x"
refs[which(refs$Ref == "Feild & Holbrook 2000"),"doi"] <- "10.1046/j.1365-3040.2000.00626.x"
# refs[which(refs$Ref == "Feild et al. 2009 + Feild & Isnard 2013"),"doi"] <- "10.1111/j.1472-4669.2009.00189.x"
refs[which(refs$Ref == "Gartner et al. 1990"),"doi"] <- "10.1002/j.1537-2197.1990.tb14464.x"
refs[which(refs$Ref == "Hacke et al. 2007"),"doi"] <- "10.1086/520724"
refs[which(refs$Ref == "Hao et al. 2008"),"doi"] <- "10.1007/s00442-007-0918-5"
refs[which(refs$Ref == "Johnson et al. 2013"),"doi"] <- "10.3732/ajb.1200590"
refs[which(refs$Ref == "Lopez et al. 2005"),"doi"] <- "10.1093/treephys/25.12.1553"
refs[which(refs$Ref == "Machado & Tyree 1994"),"doi"] <- "10.1093/treephys/14.3.219"
refs[which(refs$Ref == "Meinzer et al. 2003"),"doi"] <- "10.1046/j.1365-3040.2003.01039.x"
refs[which(refs$Ref == "Meinzer et al. 2008"),"doi"] <- "10.1111/j.1365-2435.2009.01577.x"
refs[which(refs$Ref == "Mendez-Alonzo et al. 2012"),"doi"] <- "10.1890/11-1213.1"
refs[which(refs$Ref == "Santiago et al. 2004"),"doi"] <- "10.1007/s00442-004-1624-1"
refs[which(refs$Ref == "Scholz et al. 2008"),"doi"] <- "10.1590/S1677-04202008000300006"
refs[which(refs$Ref == "Sobrado 1996"),"doi"] <- "10.1007/BF02873864"
refs[which(refs$Ref == "Sobrado 1997"),"doi"] <- "10.1023/A:1001725808647"
refs[which(refs$Ref == "Sperry et al. 1988"),"doi"] <- "10.1111/j.1399-3054.1988.tb00632.x"
refs[which(refs$Ref == "Sperry et al. 2007"),"doi"] <- "10.1086/520726"
refs[which(refs$Ref == "Tyree & Sperry 1989"),"doi"] <- "10.1111/j.1365-3040.1990.tb01319.x"
refs[which(refs$Ref == "Tyree et al. 1991"),"doi"] <- "10.1111/j.1469-8137.1991.tb00035.x"
refs[which(refs$Ref == "Tyree et al. 1998"),"doi"] <- "10.1093/treephys/18.8-9.583"
# refs[which(refs$Ref == "Vander Willigen et al. 2000"),"doi"] <- "10.1046/j.1469-8137.2000.00549.x"
refs[which(refs$Ref == "Zhang et al. 2009"),"doi"] <- "10.1111/j.1365-3040.2009.02012.x"
refs[which(refs$Ref == "Zhu & Cao 2009"),"doi"] <- "10.1007/s11258-009-9592-5"
refs[which(refs$Ref == "Zhu et al. 2017"),"doi"] <- "10.1093/treephys/tpx094"
refs[which(refs$Ref == "Zotz et al. 1994"),"doi"] <- "10.1111/j.1469-8137.1994.tb04279.x"
refs[which(refs$Ref == "Zotz et al. 1997"),"doi"] <- "10.1093/treephys/17.6.359"
refs[which(refs$Ref == "Van der Sande et al. 2013 + unpublished data"),"doi"] <- "10.1007/s00442-012-2563-x"

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

dat <- left_join(dat,refs) %>% select(-one_of("Ref", "doi"))

tmp1 <- file.path(datapath, "tmp", paste0(datafile,"_tmp1", ".csv"))
write.csv(dat, file = tmp1)

#------------------------------------------------------------------------------#
# Sites

dat <- read.csv(file = tmp1, na.strings = NaN,  stringsAsFactors = FALSE)

# make a separate dataframe to work on getting sites in to the database

dat_site_cite <- dat %>%
  mutate(site_id = as.numeric(NA)) %>%
  select(one_of("citation_id", "lat", "lon", "site_id")) %>% distinct

# Resolve a few things by hand before running the automated function

idx <- which(dat_site_cite$lat == 9.166666667 &
               dat_site_cite$lon == -79.85)
dat_site_cite[idx, "site_id"] <- rep(2000000002, length(idx))

idx <- which(dat_site_cite$citation_id == 1000000117)
dat_site_cite[idx,"site_id"] <- 1000026724

idx <- which(dat_site_cite$citation_id == 1000000130)
dat_site_cite[idx,"site_id"] <- 1000025744

idx <- which(dat_site_cite$citation_id == 1000000126 &
               dat_site_cite$lat == 9.150000000 &
               dat_site_cite$lon == -79.85)
dat_site_cite[idx,"site_id"] <- 1000005010

idx <- which(dat_site_cite$citation_id == 1000000123 &
               dat_site_cite$lat == -16.083333330 &
               dat_site_cite$lon == 145.40000000)
dat_site_cite[idx,"site_id"] <- 1000026718

idx <- which(as.numeric(dat_site_cite$citation_id) == 1000000124 &
               dat_site_cite$lat == 9.283333333	 &
               dat_site_cite$lon == -79.975)
dat_site_cite[idx,"site_id"] <- 2000000002

####
# Run my function cite2site()

# First pass chek for exact matches
dat_site_cite <- cite2site(dat_site_cite, interval = 0)
dat_out_na <- dat_site_cite %>% filter(is.na(site_id)) %>%
  select(one_of("citation_id", "lat", "lon")) %>% distinct

# Not sure if it's worth doing this inermediate step
dat_site_cite <- cite2site(dat_site_cite, interval = .1)
dat_out_na <- dat_site_cite %>% filter(is.na(site_id)) %>%
  select(one_of("citation_id", "lat", "lon")) %>% distinct

# One final coarse search
dat_site_cite <- cite2site(dat_site_cite, interval = .5)
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

dat[which(dat$species == "Michelia hypolampra"), "species"] <- "Magnolia hypolampra"
dat[grep("Castanopsis", dat$species), "species"] <- "Castanopsis indica" # weird formatting problem ( I think an special character space?)
dat[which(dat$species == "Cordia collococa"), "species"] <- "Cordia nodosa"
dat[which(dat$species == "Galbulimima baccata"), "species"] <- "Galbulimima belgraveana"
dat[which(dat$species == "Pistacia weinmannifolia"), "species"] <- "Pistacia weinmanniifolia"
dat[which(dat$species == "Diospyros brachiata"), "species"] <- "Diospyros dictyoneura"

#####

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

# Review by hand - it's a pain, but I can't think of a better method.

sp[which(sp$submit_name == "acinodendron pohlianum"),"reviewed"] <- TRUE
sp[which(sp$submit_name == "antirrhea trichantha"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "aspidosperma cruenta"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "austromyrtus bidwillii"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "bauhinia variegate"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "eugenia ampullaria"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "eugenia bankensis"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "eugenia muelleri"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "miconia pohliana"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "millettia cubitti"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "myrsine ferruginea"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "piptadenia constricta"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "syzygium latilimbum"), "reviewed"] <- TRUE

sp <- insert_species(sp, case_accept_set, bety)

all(!is.na(sp$bety_id))
all(sp$in_PFT)

dat <- left_join(dat, sp %>% transmute(species = str_to_sentence(submit_name), species_id = bety_id)) %>% select(-species)

tmp3 <- file.path(datapath, "tmp", paste0(datafile,"_tmp3", ".csv"))
write.csv(dat, file = tmp3)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Preparation of data

dat <- read.csv(file = tmp3, na.strings = NaN,  stringsAsFactors = FALSE) %>% select(-one_of("X","X.1"))

dat <- dat %>% mutate(ksat = as.numeric(ksat),
                      kl = as.numeric(kl),
                      P50 = as.numeric(P50),
                      ax = as.numeric(ax),
                      SLA = as.numeric(SLA),
                      WD = as.numeric(WD))


## Kmax
##
## Data Units :kg     m-1 MPa-1 s-1
## ED units:   kg H2O m-1       s-1

# conversion
dat <- dat %>% mutate(wood_Kmax = ksat / MPa2m)

# diagnostic plot
wood_Kmax_default <-  get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "wood_Kmax")
wood_Kmax_id <- tbl(bety,"variables") %>% filter(name == "wood_Kmax") %>% pull(id)
wood_Kmax_fit <- tbl(bety, "priors") %>%
  filter(variable_id == wood_Kmax_id) %>% collect()
wood_Kmax_prior <- rdistn(wood_Kmax_fit)

p <- prior_plot(prior = wood_Kmax_prior,
                q = c(0,.975),
                plot_default = wood_Kmax_default,
                title = sprintf("wood_Kmax: %s", wood_Kmax_fit$distn),
                type = "data")
p + geom_density(data = dat, aes(x = wood_Kmax, fill = "obs"), alpha = .3, color = NA)


var <- "wood_Kmax"
varid <- wood_Kmax_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("var" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), var_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

wood_Kmax_df <- df


## psi50
##
## Data Units : MPa
## ED units:    m
## BETY units: -m (this is so we can fit the distribution)

# conversion
dat <- dat %>% mutate(wood_psi50 = -P50 * MPa2m)

# diagnostic plot
wood_density_default <-  get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "rho")
wood_psi50_default <- (-1.09 - (3.57 * wood_density_default ^ 1.73)) * MPa2m
wood_psi50_id <- tbl(bety, "variables") %>%
  filter(name == "wood_psi50") %>% pull(id)
wood_psi50_fit <- tbl(bety, "priors") %>%
  filter(variable_id == wood_psi50_id) %>% collect()
# In this case, let's use the lnorm
wood_psi50_fit <- wood_psi50_fit %>% filter(distn == "lnorm")
wood_psi50_prior <- rdistn(wood_psi50_fit)
p <- prior_plot(prior = wood_psi50_prior,
                q = c(0,.975),
                plot_default = wood_psi50_default,
                title = sprintf("Water potential at which 50perc. of stem cond. is lost (wood_psi50): %s", wood_psi50_fit$distn),
                type = "prior")
p + geom_density(data = dat, aes(x = wood_psi50, fill = "obs"), alpha = .3, color = NA)

var <- "wood_psi50"
varid <- wood_psi50_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("var" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), var_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

wood_psi50_df <- df

## Kexp
## Kexp = - ax*4*P50/100
## Data Units : unitless
## ED units:    unitless

# conversion
dat <- dat %>% mutate(wood_Kexp = - ax*4*P50/100)

# diagnostic plot
wood_Kexp_default <- 0.544 * 4. * (-wood_psi50_default / MPa2m) ^ (-0.17)

wood_Kexp_id <- tbl(bety,"variables") %>% filter(name == "wood_Kexp") %>% pull(id)
wood_Kexp_fit <- tbl(bety, "priors") %>%
  filter(variable_id == wood_Kexp_id) %>% collect()
wood_Kexp_prior <- rdistn(wood_Kexp_fit)

p <- prior_plot(prior = wood_Kexp_prior,
                q = c(0,.975),
                plot_default = wood_Kexp_default,
                title = sprintf("wood_Kexp: %s", wood_Kexp_fit$distn),
                type = "data")
p + geom_density(data = dat, aes(x = wood_Kexp, fill = "obs"), alpha = .3, color = NA)

var <- "wood_Kexp"
varid <- wood_Kexp_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("var" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), var_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))

wood_Kexp_df <- df

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
