library(ED.Hydro.Helpers)

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

# traits_example <- tbl(bety, "traits") %>% collect() %>% head()

wdns <- 1.000e3
grav <- 9.80665
MPa2m <- wdns / grav

################################################################################
# Load Data

datafile <- "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Capacitance_BETY.csv"
dat_in <- read.csv(datafile, stringsAsFactors = FALSE) %>% rename(species = Species)
dat_in$Ref <- str_replace(string = dat_in$Ref, pattern = "[.]", replacement = "")

trait.data <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/get_trait_results/ED_tropical_hydro/trait.data.csv", stringsAsFactors = FALSE)

################################################################################
## Load in the species that we have resolved (which is now all of them! yay!)

sp_in <- read.csv(file = "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species_final.csv",
                  stringsAsFactors = FALSE)
sp <- sp_in %>%
  select(one_of("submit_name", "bety_id")) %>%
  mutate(species = tolower(submit_name), specie_id = bety_id) %>%
  select(one_of("species", "specie_id"))

dat <- inner_join(dat_in, sp, by = "species")

################################################################################
# Set treatment id

dat$treatment <- 2000000012

################################################################################
##  Check that citations are in the database

unique(dat$Ref) %>% length()
unique(dat$doi) %>% length()
dat %>% select("Ref", "doi") %>% distinct()

# Some doi's are wrong in the data!
dat$doi[which(dat$Ref == "Werden et al 2018")] <- "10.1093/treephys/tpx135"

# Here begins the mess that is the Meinzer data
# M <- dat %>%
#   filter(str_detect(Ref, "Meinzer")) %>%
#   filter(!Ref == "Meinzer et al 2003") %>% # The 2003 ref is fine
#   mutate(s = paste(Long, Lat)) %>%
#   mutate(site = factor(s, labels=c(1:length(unique(s))))) %>%
#   select(-one_of("s")) %>%
#   arrange(doi, site, species)
# M
# M <- M %>% filter(!doi == "10.1093/treephys/28.11.1609") # One of the 2008 refs is fine
# M
# # Fixing another 2008 Ref ... well I thought this was right until I realized that site 3 also sort of counts.
# M[which(M$site == 1 | M$site == 4), "doi"] <- "10.1007/s00442-008-0974-5"
# M <- M %>% filter(!doi == "10.1007/s00442-008-0974-5")

# Currently I'm still leaving this shit mess out.
dat <- dat %>% filter(!(Ref == "Meinzer et al 2008" & doi == "10.1046/j.1365-3040.2003.01039.x"))

################################################################################

dat$citation_id <- NA

all_doi <- unique(na.omit(dat$doi))

bibs <- insert_dois(all_doi)

for(i in seq_along(bibs)){
  idx <- which(dat$doi == bibs[[i]]$doi)
  authors <- bibs[[i]]$author %>% str_split(" ") %>% unlist()
  author <- authors %>% head(min(length(authors), 5))
  year <- bibs[[i]]$year

  print(paste0(i," | ",unique(dat[idx, "Ref"])))
  print(bibs[[i]]$key)

  print(str_match(unique(dat[idx, "Ref"]), author))
  print(str_match(unique(dat[idx, "Ref"]), year))

  print("--------------------------------------------")
  dat$citation_id[idx] <- bibs[[i]]$citation_id
}

################################################################################
##  Check that sites are in the database

# Steps:
# 1) Check for the site in the citation
# 2) Check for the site in all sites (if it's there but not linked to citation, I guess I should probably link it.)
# 3) If site in neither places, add it and also link to citation

dat_sites <- dat %>%
  select(one_of("Lat", "Long", "Biome", "citation_id")) %>%
  distinct() %>%
  mutate(site_id = NA)

dat_sites$citation_id <- as.numeric(paste(dat_sites$citation_id))
dat_sites <- dat_sites[order(dat_sites$citation_id),]

for(i in 1:nrow(dat_sites)){

  print(paste0("Citation ID:", dat_sites$citation_id[i]))
  print(sprintf("%.2f, %.2f, %s", dat_sites$Lat[i], dat_sites$Long[i], dat_sites$Biome[i]))

  # 1) Does the citation already have a site attached?
  cit_id <- dat_sites$citation_id[i]
  test1 <- tbl(bety, "citations_sites") %>% filter(citation_id == cit_id) %>% collect()

  # 2) Are there things in BETY that are nearby the site?
  lat <- as.numeric(dat_sites[i,"Lat"])
  lon <- as.numeric(dat_sites[i,"Long"])
  test2 <- nearby_sites(lat, lon, interval = .1, bety = bety)

  if(nrow(test1) >= 1 & nrow(test2) >= 1){
    join <- inner_join(test1 %>% select("citation_id", "site_id"), test2, by = c("site_id" = "id"))

    if(nrow(join) == 1) {
      dat_sites[i, "site_id"] <- join$site_id
      print("DONE")
    }else{
      print("Got as far as join:")
      print(join)
    }
  }else{
    print("Test 1:")
    print(test1)
    print("Test 2:")
    print(test2)
  }
}
dat_sites
dat_sites[which(dat_sites$citation_id == 1000000079), "site_id"] <- 2000000002
dat_sites
dat_sites[which(dat_sites$citation_id == 1000000104), "site_id"] <- 1000026715
dat_sites
dat_sites[which(dat_sites$citation_id == 1000000103), "site_id"] <- 1000026716
dat_sites

dat <- left_join(dat, dat_sites)
################################################################################

unique(dat$Organ)

dat <- dat %>% mutate(water_cap =  case_when(
  Organ == "Leaf" ~ "leaf_water_cap",
  Organ == "Xylem" ~ "wood_water_cap"
))

dat <- dat %>% mutate( Cft_conv = case_when(
  Organ == "Leaf" ~ Cft * 1/MPa2m * 1000,
  Organ == "Xylem" ~ Cft * 1/MPa2m #* 1/WD
))

ggplot(dat) + geom_density(aes(x = Cft_conv, col = Organ))

dat <- dat %>%
  mutate(leaf_water_cap = case_when(water_cap == "leaf_water_cap" ~ Cft_conv,
                                    TRUE ~ NA_real_)) %>%
  mutate(wood_water_cap = case_when(water_cap == "wood_water_cap" ~ Cft_conv,
                                    TRUE ~ NA_real_))

ggplot(dat) + geom_density(aes(x = leaf_water_cap)) + geom_density(aes(x = wood_water_cap))

dat$specie_id <- as.numeric(dat$specie_id)

dat_ready <- dat %>% rename("wood_density" = "WD") %>%
  select(one_of("wood_density", "SLA", "leaf_water_cap", "wood_water_cap",
                "treatment", "specie_id", "citation_id", "site_id"))

################################################################################

################################################################################
## Wood Density

### Removing some redundant data. WD is counted twice because it is recorded for
### multiple other kinds of measurements on the same tree

var <- "wood_density"
var_id <- tbl(bety, "variables") %>% filter(name == var) %>% pull(id)

insert_dat <- dat_ready %>%
  select(one_of(var, "specie_id", "citation_id", "site_id", "treatment")) %>%
  rename("var" = var) %>%
  filter(!is.na(var)) %>% filter(!is.na(specie_id)) %>%
  mutate(variable_id = var_id) %>%
  na.omit %>%
  mutate(trait_id = NA) %>%
  distinct()

wood_density_traits <- insert_traits(insert_dat = insert_dat, bety = bety)

wood_density_fit <- tbl(bety, "priors") %>%
  filter(variable_id == var_id) %>%
  filter(id == 1000000281) %>% collect()
wood_density_prior <- rdistn(wood_density_fit)
wood_density_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "rho")

p <- prior_plot(prior = wood_density_prior,
                plot_default = wood_density_default,
                title = sprintf("Wood Density"),
                type = "prior")

p + geom_density(data = wood_density_traits,
                 aes(x = var, fill = "obs"), alpha = .3, color = NA)

ggplot() +
  geom_rect(aes(xmin=wood_density_fit$parama, xmax=wood_density_fit$paramb, ymin=0, ymax=Inf), alpha = .5) +
  geom_density(data = wood_density_traits,
                        aes(x = var, fill = as.factor(citation_id)), alpha = .5, position="stack") +
  theme_classic()

ggplot() +
  geom_rect(aes(xmin=wood_density_fit$parama, xmax=wood_density_fit$paramb, ymin=0, ymax=Inf), alpha = .5) +
  geom_histogram(data = wood_density_traits,
               aes(x = var, fill = as.factor(site_id)), bins = 60) +
  theme_classic()

ggplot() +
  geom_rect(aes(xmin=wood_density_fit$parama, xmax=wood_density_fit$paramb, ymin=0, ymax=Inf), alpha = .5) +
  geom_density(data = wood_density_traits,
               aes(x = var, fill = as.factor(site_id)), alpha = .5, position="stack") +
  theme_classic()

ggplot() +
  geom_rect(aes(xmin=wood_density_fit$parama, xmax=wood_density_fit$paramb, ymin=0, ymax=Inf), alpha = .5) +
  geom_density(data = wood_density_traits,
                        aes(x = var, fill = as.factor(citation_id)), alpha = .3) +
  theme_classic()

################################################################################
## Leaf Water Capacitance
## Leaf: gH2O g-1 dry weight MPa-1

var <- "leaf_water_cap"
var_id <- tbl(bety, "variables") %>% filter(name == var) %>% pull(id)

insert_dat <- dat_ready %>%
  select(one_of(var, "specie_id", "citation_id", "site_id", "treatment")) %>%
  rename("var" = var) %>%
  filter(!is.na(var)) %>% filter(!is.na(specie_id)) %>%
  mutate(variable_id = var_id) %>%
  na.omit %>%
  mutate(trait_id = NA) %>%
  distinct()

# for(i in seq_along(insert_dat$var)){
#   update.query <- sprintf("UPDATE traits SET mean = %.0f WHERE mean = %.0f;",
#                           insert_dat$var[i]*1000, insert_dat$var[i])
#   trait_id <- db.query(insert.query, bety$con)
# }


leaf_water_cap_traits <- insert_traits(insert_dat = insert_dat, bety = bety)

leaf_water_cap_fit <- tbl(bety, "priors") %>%
  filter(variable_id == var_id) %>%
  collect()

leaf_water_cap_prior <- rdistn(leaf_water_cap_fit)
leaf_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "leaf_water_cap") * 1000

p <- prior_plot(prior = leaf_water_cap_prior,
                q = c(0,.995),
                plot_default = leaf_water_cap_default,
                title = sprintf("Leaf Water Capacitance"),
                type = "prior")
p + geom_density(data = leaf_water_cap_traits, aes(x = var, fill = "obs"), alpha = .3, color = NA)

################################################################################
## Wood water capacitance
## Sapwood: kg m-3 MPa-1
## (~1/(1000*WD*MPa2M I guess)

var <- "wood_water_cap"
var_id <- tbl(bety, "variables") %>% filter(name == var) %>% pull(id)

insert_dat <- dat_ready %>%
  select(one_of(var, "specie_id", "citation_id", "site_id", "treatment")) %>%
  rename("var" = var) %>%
  filter(!is.na(var)) %>% filter(!is.na(specie_id)) %>%
  mutate(variable_id = var_id) %>%
  na.omit %>%
  mutate(trait_id = NA) %>%
  distinct()

wood_water_cap_traits <- insert_traits(insert_dat = insert_dat, bety = bety)

wood_water_cap_fit <- tbl(bety, "priors") %>%
  filter(variable_id == var_id) %>%
  collect()

wood_water_cap_prior <- rdistn(wood_water_cap_fit)
wood_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "wood_water_cap") * 1000

p <- prior_plot(prior = wood_water_cap_prior,
                q = c(0,.995),
                plot_default = wood_water_cap_default,
                title = sprintf("Wood Water Capacitance"),
                type = "prior")
p + geom_density(data = wood_water_cap_traits, aes(x = var, fill = "obs"), alpha = .3, color = NA)


################################################################################
## SLA

var <- "SLA"
var_id <- tbl(bety, "variables") %>% filter(name == var) %>% pull(id)

insert_dat <- dat_ready %>%
  select(one_of(var, "specie_id", "citation_id", "site_id", "treatment")) %>%
  rename("var" = var) %>%
  filter(!is.na(var)) %>% filter(!is.na(specie_id)) %>%
  mutate(variable_id = var_id) %>%
  na.omit %>%
  mutate(trait_id = NA) %>%
  distinct()

SLA_traits <- insert_traits(insert_dat = insert_dat, bety = bety)


SLA_fit <- tbl(bety, "priors") %>% filter(variable_id == var_id) %>% filter(id == 142) %>% collect()
SLA_prior <- rdistn(SLA_fit)
SLA_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "SLA")

p <- prior_plot(prior = SLA_prior,
                plot_default = SLA_default,
                title = sprintf("SLA"),
                type = "prior")
p + geom_density(data = SLA_traits, aes(x = var, fill = "obs"), alpha = .3, color = NA)
