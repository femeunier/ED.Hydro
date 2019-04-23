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

unique(dat_in$Ref) %>% length()
unique(dat_in$doi) %>% length()

dat_in %>% select("Ref", "doi") %>% distinct()

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

# Set treatment id

dat$treatment <- 2000000012

################################################################################
##  Check that citations are in the database

# Some doi's are wrong in the data!
dat$doi[which(dat$Ref == "Werden et al 2018")] <- "10.1093/treephys/tpx135"

# I can't reconcile these ones:
# dim(dat)
# dim(dat %>% filter((Ref == "Meinzer et al 2008" & doi == "10.1046/j.1365-3040.2003.01039.x")))
# dat %>% filter((Ref == "Meinzer et al 2008" & doi == "10.1093/treephys/28.11.1609")) %>% arrange(Long)
#
# example <- dat %>% filter(doi == "10.1046/j.1365-3040.2003.01039.x") %>% arrange(Ref, Long) %>% select(-specie_id) %>% mutate(in_paper = FALSE)
# example$in_paper[which(example$WD == .39)] <- TRUE
# example$in_paper[which(example$WD == .29)] <- TRUE
# example$in_paper[which(example$WD == .28)] <- TRUE
# example$in_paper[which(example$WD == .52)] <- TRUE
# example

dat <- dat %>% filter(!(Ref == "Meinzer et al 2008" & doi == "10.1046/j.1365-3040.2003.01039.x"))


#################

unique(dat$Ref) %>% length()
unique(dat$doi) %>% length()
dat %>% select("Ref", "doi") %>% distinct()


dat$citation_id <- NA

all_doi <- unique(na.omit(dat$doi))

bibs <- list()

for(i in seq_along(all_doi)){
  print(i)
  bibs[[i]] = rcrossref::cr_cn(dois = all_doi[i], format = "bibentry")

  check_doi <- tbl(bety, "citations") %>% filter(doi == bibs[[i]]$doi) %>% collect()

  queries <- data.frame(include = c("user_id", "created_at", "updated_at"),
                        values = c("1000000003", "NOW()", "NOW()"),
                        stringsAsFactors = FALSE)

  if(nrow(check_doi) == 0){

    if(!is.null(bibs[[i]]$author)){
      new <- c("author", paste0("'", paste(bibs[[i]]$author, collapse = ", ")
                                %>% clean_chars(), "'"))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bibs[[i]]$year)){
      new <- c("year", paste0(as.numeric(bibs[[i]]$year)))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bibs[[i]]$title)){
      new <- c("title", paste0("'", paste(bibs[[i]]$title, collapse = ", ")
                               %>% clean_chars(), "'"))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bibs[[i]]$journal)){
      new <- c("journal", paste0("'", paste(bibs[[i]]$journal, collapse = ", ")
                                 %>% clean_chars(), "'"))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bibs[[i]]$volume)){
      new <- c("vol", paste0(as.numeric(bibs[[i]]$volume)))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bibs[[i]]$pg)){
      new <- c("pg", paste0("'", bibs[[i]]$pages %>%
                              str_replace(pattern = "--",replacement = "-") %>%
                              str_squish, "'"))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bibs[[i]]$url)){
      new <- c("url", paste0("'", bibs[[i]]$url %>% str_squish, "'"))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bibs[[i]]$doi)){
      new <- c("doi", paste0("'", bibs[[i]]$doi %>% str_squish, "'"))
      queries <- rbind.data.frame(queries, new)
    }

    paste(queries$include, collapse = ", ")
    paste(queries$values, collapse = ", ")

    insert.query <- sprintf("INSERT INTO citations (%s) VALUES(%s) RETURNING id;",
                            paste(queries$include, collapse = ", "),
                            paste(queries$values, collapse = ", "))

    citation_id <- db.query(insert.query, bety$con)
    sprintf("Citation %10.0f added to BETY", citation_id)

  }else{
    citation_id <- check_doi$id
    print(sprintf("Citation %10.0f already in BETY", citation_id))
  }

  bibs[[i]]$citation_id <- citation_id

  idx <- which(dat$doi == bibs[[i]]$doi)
  dat$citation_id[idx] <- citation_id
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
  test1 <- tbl(bety, "citations_sites") %>% filter(citation_id == dat_sites$citation_id[i]) %>% collect()

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
  Organ == "Leaf" ~ Cft * 1/MPa2m,
  Organ == "Xylem" ~ Cft * 1/MPa2m * 1/1000 #* 1/WD
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

ggplot() + geom_density(data = dat, aes(x = WD, fill = Organ), alpha = .3, color = NA) +
  geom_density(data = data.frame(WD = wood_density_prior), aes(x = WD))

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

leaf_water_cap_traits <- insert_traits(insert_dat = insert_dat, bety = bety)

leaf_water_cap_fit <- tbl(bety, "priors") %>%
  filter(variable_id == var_id) %>%
  collect()

leaf_water_cap_prior <- rdistn(leaf_water_cap_fit) * (1/1000)
leaf_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "leaf_water_cap")

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

wood_water_cap_prior <- rdistn(wood_water_cap_fit) * (1/1000)
wood_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "wood_water_cap")

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
p + geom_density(data = dat, aes(x = SLA, fill = "obs"), alpha = .3, color = NA)
