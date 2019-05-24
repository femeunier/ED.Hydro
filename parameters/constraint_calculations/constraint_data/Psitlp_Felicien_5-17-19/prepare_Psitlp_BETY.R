# Prepare the Psitlp data

#------------------------------------------------------------------------------#
# Setup

# Loading my package also loads any necessary pecan packages that are needed
library(ED.Hydro.Helpers)

# Other optional settings
options(digits = 10) # I set digits to 10 so it's easier to read bety ids
options(geonamesUsername = "ecowdery") # this can be anything, "pecan" probably works or just use mine
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php") # the path to my config.php

datapath <- "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Psitlp_Felicien_5-17-19/"
datafile <- "Psitlp_BETY"

#------------------------------------------------------------------------------#
# Read in the data

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

# Remove plants that were grown in a greenhouse? Maybe?
# Talk to Mike about this.
dat <- dat[-grep("Feild et al. 2009 \\+ Feild \\& Isnard", dat$Ref),]


# make a separate dataframe for working on getting refs in to the database
refs <- dat %>%
  mutate(citation_id = as.numeric(NA), doi = NA) %>%
  select(one_of("Ref", "doi", "citation_id")) %>%
  distinct %>% arrange(Ref)
# View(refs)

refs[which(refs$Ref == "Baltzer et al. 2008"),"doi"] <- "10.1111/j.1365-2435.2007.01374.x"
refs[which(refs$Ref == "Bartlett unpublished data"),"citation_id"] <- 1000000139
refs[which(refs$Ref == "Borchert & Pockman 2005"),"doi"] <- "10.1093/treephys/25.4.457"
refs[which(refs$Ref == "Brodribb etal. 2003"),"doi"] <- "10.1046/j.1365-3040.2003.00975.x"
refs[which(refs$Ref == "Brodribb et al. 2003, Brodribb&Holbrook 2005"),"doi"] <- "10.1046/j.1365-3040.2003.00975.x"
refs[which(refs$Ref == "Brodribb&Holbrook 2003"),"doi"] <- "10.1104/pp.103.023879"
refs[which(refs$Ref == "Carrasco et al. 2014"),"doi"] <- "10.1093/treephys/tpu087"
refs[which(refs$Ref == "Chen et al. 2017"),"doi"] <- "10.1111/1365-2435.12724"
refs[which(refs$Ref == "Chen et al. 2017 + unpublished data"),"doi"] <- "10.1111/1365-2435.12724"
refs[which(refs$Ref == "Choat et al. 2007"),"doi"] <- "10.1111/j.1469-8137.2007.02137.x"
refs[which(refs$Ref == "De Guzman et al. 2016"),"doi"] <- "10.1093/treephys/tpw086"
refs[which(refs$Ref == "De Guzman et al. 2016 + unpublished data"),"doi"] <- "10.1093/treephys/tpw086"
# refs[which(refs$Ref == "Feild et al. 2009 + Feild & Isnard 2013"),"doi"] <- "10.1111/j.1472-4669.2009.00189.x
refs[which(refs$Ref == "Fanjul & Barradas 1987"),"doi"] <- "10.2307/2403805"
refs[which(refs$Ref == "Fu et al. 2012"),"doi"] <- "10.1093/aob/mcs092"
refs[which(refs$Ref == "Hao et al. 2008"),"doi"] <- "10.1007/s00442-007-0918-5"
refs[which(refs$Ref == "Hao et al. 2010"),"doi"] <- "10.1111/j.1365-2435.2010.01724.x"
refs[which(refs$Ref == "Johnson et al. 2013"),"doi"] <- "10.3732/ajb.1200590"
refs[which(refs$Ref == "Le Roux & Bariac 1998"),"doi"] <- "10.1007/s004420050398"
refs[which(refs$Ref == "Machado & Tyree 1994"),"doi"] <- "10.1093/treephys/14.3.219"
refs[which(refs$Ref == "Marechaux et al. 2015"),"doi"] <- "10.1111/1365-2435.12452"
refs[which(refs$Ref == "McCulloh et al. 2012"),"doi"] <- "10.1111/j.1365-3040.2011.02421.x"
refs[which(refs$Ref == "Meinzer et al 2008"),"doi"] <- "10.1093/treephys/28.11.1609"
refs[which(refs$Ref == "Meinzer et al. 1990"),"doi"] <- "10.1104/pp.94.4.1781"
refs[which(refs$Ref == "Meinzer et al 2008"),"doi"] <- "10.1093/treephys/28.11.1609"
refs[which(refs$Ref == "Meinzer et al. 2008"),"doi"] <- "10.1111/j.1365-2435.2009.01577.x"
refs[which(refs$Ref == "Olivares & Medina 1992"),"doi"] <- "10.2307/3235764"
refs[which(refs$Ref == "Rosado & De Mattos 2010"),"doi"] <- "10.1111/j.1654-1103.2009.01119.x"
refs[which(refs$Ref == "Scholz et al. 2007"),"doi"] <- "10.1111/j.1365-2435.2008.01452.x"
refs[which(refs$Ref == "Scoffoni et al. 2008"),"doi"] <- "10.1111/j.1365-3040.2008.01884.x"
refs[which(refs$Ref == "Shrestha et al. 2008"),"doi"] <- "10.3126/hjs.v4i6.982"
refs[which(refs$Ref == "Singh et al. 2006"),"citation_id"] <- 1000000141
refs[which(refs$Ref == "Sobrado 1986"),"doi"] <- "10.1007/BF01036748"
refs[which(refs$Ref == "Sobrado 1986; Sobrado 1993"),"doi"] <- "10.1007/BF00318025"
refs[which(refs$Ref == "Sobrado 2009"),"doi"] <- "10.1017/S026646740900604X"
refs[which(refs$Ref == "Sobrado 2012"),"doi"] <- "10.1017/S0266467412000454"
refs[which(refs$Ref == "Tobin et al. 1999"),"doi"] <- "10.1111/j.1744-7429.1999.tb00404.x"
refs[which(refs$Ref == "Tyree et al. 1991"),"doi"] <- "10.1111/j.1469-8137.1991.tb00035.x"
refs[which(refs$Ref == "Villagra et al. 2013"),"doi"] <- "10.1093/treephys/tpt098"
refs[which(refs$Ref == "Wenhui 1998"),"doi"] <- "10.1007/BF02912325"
refs[which(refs$Ref == "Werden et al 2018"),"doi"] <- "10.1093/treephys/tpx135"
refs[which(refs$Ref == "Zhu & Cao 2009"),"doi"] <- "10.1007/s11258-009-9592-5"
refs[which(refs$Ref == "Carrasco et al. 2014"),"doi"] <- "10.1093/treephys/tpu087"
refs[which(refs$Ref == "Meinzer et al. 2008"),"doi"] <- "10.1111/j.1365-2435.2009.01577.x"
refs[which(refs$Ref == "Tyree et al. 1991"),"doi"] <- "10.1111/j.1469-8137.1991.tb00035.x"

refs$doi <- str_trim(refs$doi) # because I always miss something

all_doi <- unique(na.omit(refs$doi))
bibs <- insert_dois(all_doi)

length(all_doi)
length(bibs)

for(i in seq_along(bibs)){
  idx <- which(tolower(refs$doi) == tolower(bibs[[i]]$doi)) #DOI's are case insensitive
  authors <- bibs[[i]]$author %>% str_split(" ") %>% unlist()
  author <- authors %>% head(min(length(authors), 5)) %>% clean_chars() # clean_chars() is one of my functions
  year <- bibs[[i]]$year
  for(j in idx){
    print(paste0(i," | ",unique(refs[j, "Ref"])))
    print(bibs[[i]]$key)
    print(str_match(unique(refs[j, "Ref"]), author))
    print(str_match(unique(refs[j, "Ref"]), year))
    print("--------------------------------------------")
    refs$citation_id[j] <- bibs[[i]]$citation_id
  }
}

length(unique(refs$Ref))
length(unique(refs$citation_id))

dat <- left_join(dat,refs) %>% select(-one_of("Ref", "doi"))

all(!is.na(dat$citation_id))

tmp1 <- file.path(datapath, "tmp", paste0(datafile,"_tmp1", ".csv"))
write.csv(dat, file = tmp1)

#------------------------------------------------------------------------------#
# Sites

tmp1 <- file.path(datapath, "tmp", paste0(datafile,"_tmp1", ".csv"))
dat <- read.csv(file = tmp1, na.strings = NaN,  stringsAsFactors = FALSE)

# Resolve a few things by hand before running the automated function

# Data from citation_id = 1000000103 are given the wrong coordinates
dat[which(dat$citation_id == 1000000103), "lat"] <- 10.451983
dat[which(dat$citation_id == 1000000103), "lon"] <- -85.127044

# Data from citation_id = 1000000146 are given the wrong coordinates
# but even so, it still falls somewhat far away from the site.
dat[which(dat$citation_id == 1000000146), "lon"] <- 101.25
dat[which(dat$citation_id == 1000000146), "lon"] <- 101.7

# make a separate dataframe to work on getting sites in to the database

dat_site_cite <- dat %>%
  mutate(site_id = as.numeric(NA)) %>%
  select(one_of("citation_id", "lat", "lon", "site_id")) %>%
  distinct %>%
  arrange(citation_id)

####
# Run my function cite2site()

# First pass chek for exact matches
dat_site_cite <- cite2site(dat_site_cite, interval = 0)
dat_out_na <- dat_site_cite %>% filter(is.na(site_id)) %>%
  select(one_of("citation_id", "lat", "lon")) %>% distinct

# View(dat_out_na)
# View(dat_site_cite)

# One final coarse search
dat_site_cite <- cite2site(dat_site_cite, interval = .4)
dat_out_na <- dat_site_cite %>% filter(is.na(site_id)) %>%
  select(one_of("citation_id", "lat", "lon")) %>% distinct

# dat_out_na %>%
# filter(citation_id == 1000000146) %>%
# left_join(dat) %>% select(-one_of("Organ", "Biome", "X")) %>% select(one_of("lat", "lon")) %>%
# mutate("elev" = geonames::GNsrtm3(lat,lon)[[1]]) %>% distinct

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

# Fix species names that I know won't work with the taxonomic name resolver

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

# Make sure to check the "proposed bety name."
# That is what will ultimately be put in to BETY (as opposed to the submit name.)

sp[which(sp$submit_name == "antirrhea trichantha"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "aspidosperma cruenta"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "bauhinia variegate"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "bursera simarouba"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "chrysophllum cainito"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "hymenea courbaril"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "mallotus penangensis"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "micranda sprucei"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "millettia atropurpurea"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "rhedera trinervis"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "sweitenia macrophylla"), "reviewed"] <- TRUE

sp[which(sp$submit_name == "micranda sprucei"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "rhedera trinervis"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "sweitenia macrophylla"), "reviewed"] <- TRUE

sp[which(sp$submit_name == "tachigalia versicolor"), "bety_name"] <- "tachigali versicolor"
sp[which(sp$submit_name == "tachigalia versicolor"), "reviewed"]  <- TRUE

sp[which(sp$submit_name == "michelia hypolampra"), "bety_name"] <- "magnolia hypolampra"
sp[which(sp$submit_name == "michelia hypolampra"), "reviewed"]  <- TRUE

sp[which(sp$submit_name == "xanthophyllum affine"), "bety_name"] <- "xanthophyllum flavescens"
sp[which(sp$submit_name == "xanthophyllum affine"), "reviewed"]  <- TRUE

sp[which(sp$case == 11), "reviewed"]  <- TRUE
sp[which(sp$case == 8), "reviewed"]  <- TRUE

sp <- insert_species(sp, case_accept_set, insert_PFT = TRUE, bety)

sp %>% filter(!reviewed)

all(!is.na(sp$bety_id))
all(sp$in_PFT)

dat <- left_join(dat, sp %>% transmute(species = str_to_sentence(submit_name), species_id = bety_id)) %>% select(-species)
all(!is.na(dat$species_id))

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

dat <- dat %>% mutate(Ptlp = as.numeric(Ptlp),
                      SLA = as.numeric(SLA),
                      WD = as.numeric(WD))
dat$y <- 0


## SLA
## Don't need to do any conversions

SLA_id <- tbl(bety, "variables") %>% filter(name == "SLA") %>% pull(id)
SLA_fit <- tbl(bety, "priors") %>% filter(variable_id == SLA_id) %>% filter(id == 142) %>% collect()
SLA_prior <- rdistn(SLA_fit, n = 100000)
SLA_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "SLA")

max(SLA_prior) > max(dat$SLA, na.rm = TRUE)

# SLA_id <- tbl(bety, "variables") %>% filter(name == "SLA") %>% pull(id)
# SLA_fits <-
#   tbl(bety, "priors") %>% filter(variable_id == SLA_id) %>%
#   collect %>%
#   mutate(name = paste(distn, parama, paramb, sep = "_"))
#

#
# p + geom_point(data = dat, aes(x = SLA, y = y))

# rdistns_list <- list()
# for(i in 1:nrow(SLA_fits)){
#   d <- rdistn(SLA_fits[i,])
#   name <- SLA_fits$name[i]
#   if(max(d) > max(dat$SLA, na.rm = TRUE) & SLA_fits$distn[i] == "weibull"){
#     rdistns_list[[name]] <- d
#   }
# }

# rdistns <- bind_rows(rdistns_list)
# SLA_priors <- gather(rdistns, key = "dist")
#
# ggplot(SLA_priors) +
#   geom_density(data = dat, aes(x = SLA), fill = "black") +
#   geom_density(aes(x = value, fill = dist), alpha = .3) +
#   xlim(0,100)

p <- prior_plot(prior = SLA_prior,
                plot_default = SLA_default,
                title = sprintf("(SLA): %s", SLA_fit$distn),
                type = "prior")

p + geom_density(data = dat, aes(x = SLA, fill = "obs"), alpha = .3, color = NA) + geom_point(data = dat, aes(x = SLA, y = y))

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
min(dat$wood_density, na.rm = T)

which(dat$wood_density < min(wood_density_prior))
which(dat$wood_density > max(wood_density_prior))

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

## TLP
##

ggplot(dat) + geom_density(aes(x = Ptlp, fill = Organ), alpha = .5)

dat <- dat %>%
  mutate(wood_psi_tlp = case_when(Organ == "Xylem" ~ - Ptlp * MPa2m,
                                  Organ == "Leaf"  ~ as.numeric(NA))) %>%
  mutate(leaf_psi_tlp = case_when(Organ == "Leaf"  ~ - Ptlp * MPa2m,
                                  Organ == "Xylem" ~ as.numeric(NA)))




wood_psi_tlp_id <- tbl(bety, "variables") %>% filter(name == "wood_psi_tlp") %>% pull(id)
wood_psi_tlp_fit <- tbl(bety, "priors") %>% filter(variable_id == wood_psi_tlp_id) %>% filter(distn == "lnorm") %>% collect()
wood_psi_tlp_prior <- rdistn(wood_psi_tlp_fit)
wood_psi_tlp_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "wood_psi_tlp")

p <- prior_plot(prior = wood_psi_tlp_prior,
                plot_default = -wood_psi_tlp_default,
                q = c(0,.99),
                title = sprintf("(wood_psi_tlp): %s", wood_psi_tlp_fit$distn),
                type = "prior")
p + geom_density(data = dat, aes(x = wood_psi_tlp, fill = "obs"), alpha = .3, color = NA)

var <- "wood_psi_tlp"
varid <- wood_psi_tlp_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("value" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), variable_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))


leaf_psi_tlp_id <- tbl(bety, "variables") %>% filter(name == "leaf_psi_tlp") %>% pull(id)
leaf_psi_tlp_fit <- tbl(bety, "priors") %>% filter(variable_id == leaf_psi_tlp_id) %>% filter(distn == "lnorm") %>% collect()
leaf_psi_tlp_prior <- rdistn(leaf_psi_tlp_fit)
leaf_psi_tlp_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "leaf_psi_tlp")

p <- prior_plot(prior = leaf_psi_tlp_prior,
                plot_default = -leaf_psi_tlp_default,
                q = c(0,.99),
                title = sprintf("(leaf_psi_tlp): %s", leaf_psi_tlp_fit$distn),
                type = "prior")
p + geom_density(data = dat, aes(x = leaf_psi_tlp, fill = "obs"), alpha = .3, color = NA)

var <- "leaf_psi_tlp"
varid <- leaf_psi_tlp_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("value" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), variable_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))
