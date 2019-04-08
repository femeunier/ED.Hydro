library(ED.Hydro.Helpers)

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

wdns <- 1.000e3
grav <- 9.80665
MPa2m <- wdns / grav

################################################################################
# Load Data

datafile <- "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Capacitance_BETY.csv"
dat <- read.csv(datafile, stringsAsFactors = FALSE) %>% rename(species = Species)

names(dat)

unique(dat$Organ)

dat <- dat %>% mutate(water_cap =  case_when(
  Organ == "Leaf" ~ "leaf_water_cap",
  Organ == "Xylem" ~ "wood_water_cap"
))

dat <- dat %>% mutate( Cft_conv = case_when(
  Organ == "Leaf" ~ Cft * 1/MPa2m,
  Organ == "Xylem" ~ Cft * 1/MPa2m * 1/1000 #* 1/WD
))

# ggplot(dat) + geom_density(aes(x = Cft_conv, col = Organ))

################################################################################
## Wood Density

wood_density_variable_id <- tbl(bety, "variables") %>% filter(name == "wood_density") %>% pull(id)

# Note here we are choosing specifically the prior with id = 1000000281

wood_density_fit <- tbl(bety, "priors") %>%
  filter(variable_id == wood_density_variable_id) %>%
  filter(id == 1000000281) %>% collect()
wood_density_prior <- rdistn(wood_density_fit)
wood_density_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "rho")

p <- prior_plot(prior = wood_density_prior,
                plot_default = wood_density_default,
                title = sprintf("Wood Density"),
                type = "prior")

p + geom_density(data = dat, aes(x = WD, fill = "obs"), alpha = .3, color = NA)

ggplot() + geom_density(data = dat, aes(x = WD, fill = Organ), alpha = .3, color = NA) +
  geom_density(data = data.frame(WD = wood_density_prior), aes(x = WD))

################################################################################
## Leaf Water Capacitance
## Leaf: gH2O g-1 dry weight MPa-1

leaf_water_cap_variable_id <- tbl(bety, "variables") %>%
  filter(name == "leaf_water_cap") %>% pull(id)
leaf_water_cap_fit <- tbl(bety, "priors") %>%
  filter(variable_id == leaf_water_cap_variable_id) %>%
  collect()

leaf_water_cap_prior <- rdistn(leaf_water_cap_fit) * (1/1000)
leaf_water_cap_data <- dat %>% filter(Organ == "Leaf")
leaf_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "leaf_water_cap")

prior_plot(prior = leaf_water_cap_prior,
           q = c(0,.995),
           plot_default = leaf_water_cap_default,
           title = sprintf("Leaf Water dat"),
           type = "prior") +
  geom_density(data = leaf_water_cap_data, aes(x = Cft_conv, fill = "obs"), alpha = .3, color = NA)

################################################################################
## Wood water capacitance
## Sapwood: kg m-3 MPa-1
## (~1/(1000*WD*MPa2M I guess)

wood_water_cap_variable_id <- tbl(bety, "variables") %>%
  filter(name == "wood_water_cap") %>% pull(id)
wood_water_cap_fit <- tbl(bety, "priors") %>%
  filter(variable_id == wood_water_cap_variable_id) %>%
  collect()

wood_water_cap_prior <- rdistn(wood_water_cap_fit) * (1/1000)
wood_water_cap_data <- dat %>% filter(Organ == "Xylem")
leaf_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "leaf_water_cap")

prior_plot(prior = wood_water_cap_prior,
           # q = c(0,1),
           plot_default = leaf_water_cap_default,
           title = sprintf("Wood Water dat"),
           type = "prior") +
  geom_density(data = wood_water_cap_data, aes(x = Cft_conv, fill = "obs"), alpha = .3, color = NA)

################################################################################
## SLA

SLA_variable_id <- tbl(bety, "variables") %>%
  filter(name == "SLA") %>% pull(id)
SLA_fit <- tbl(bety, "priors") %>% filter(variable_id == SLA_variable_id) %>% filter(id == 142) %>% collect()

SLA_prior <- rdistn(SLA_fit)
SLA_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "SLA")

p <- prior_plot(prior = SLA_prior,
                plot_default = SLA_default,
                title = sprintf("SLA"),
                type = "prior")
p + geom_density(data = dat, aes(x = SLA, fill = "obs"), alpha = .3, color = NA)

ggplot() + geom_density(data = dat, aes(x = SLA, fill = Organ), alpha = .3, color = NA)


################################################################################
##  Check that citations are in the database

source("/fs/data3/ecowdery/FRED/project_functions.R")

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
}

################################################################################

# Get species id

sp <- data.frame(species = sort(unique(dat$species)),
                 species_matched = NA,
                 species_id = NA,
                 stringsAsFactors = FALSE)

# # Species that will return multiple results that we can preemptively fill and one typo
# sp[sp$species == "anacardium excelsum", "species_id"] <- 49943
# # sp[sp$species == "bursera simarouba", "species_id"] <- 227
# sp[sp$species == "cedrela fissilis", "species_id"] <- 1000002385
# sp[sp$species == "luehea seemannii", "species_id"] <- 50567
# sp[sp$species == "manilkara bidentata", "species_id"] <- 43171
# sp[sp$species == "schefflera morototoni", "species_id"] <- 51206
#

for(i in seq_along(sp$species)){

  if(is.na(sp$species_id[i])){

    find <- tbl(bety, "species") %>%
      filter(toupper(scientificname) == toupper(sp$species[i])) %>%
      select(one_of("id", "genus", "species", "scientificname")) %>%
      collect()
      # filter(grepl(sp$species[i], scientificname, ignore.case = TRUE)) %>% collect

    if(dim(find)[1] == 1){
      sp$species_matched[i] <- find$scientificname
      sp$species_id[i] <- find$id

    } else {

      print(paste0(i, ": ", sp$species[i]))

      g <- strsplit(sp$species[i], " ") %>% unlist %>% .[1]
      s <- strsplit(sp$species[i], " ") %>% unlist %>% .[2]

      find2 <- tbl(bety, "species") %>%
        filter(grepl(g, genus, ignore.case = TRUE))   %>%
        filter(grepl(s, species, ignore.case = TRUE)) %>% collect

      if(dim(find2)[1]>=1){
        if(dim(find)[1]>=1){
          full_join(find, find2, by = c("id", "genus", "species", "scientificname")) %>% select(one_of("id", "genus", "species", "scientificname"))  %>%  print
        }else{ find2 %>% select(one_of("id", "genus", "species", "scientificname"))  %>% print }
      }
    }
  }
}


View(sp)

View(left_join(sp %>% filter(is.na(species_id)), dat))

sum(is.na(sp$species_id))

sp$species_matched <- NA

library(taxize)

names <- problem_species


resolved_names <- list()
for(n in names){
  resolved_names[[n]] <- gnr_resolve(n)
}

resolved_names$`albizia guachapele`


get_tsn("Albizia guachapele")


itis_acceptname(names)
