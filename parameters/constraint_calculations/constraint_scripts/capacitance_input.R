library(ED.Hydro.Helpers)

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

wdns <- 1.000e3
grav <- 9.80665
MPa2m <- wdns / grav

cap <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraints/Capacitance_BETY.csv")

names(cap)

unique(cap$Organ)

cap <- cap %>% mutate(water_cap =  case_when(
  Organ == "Leaf" ~ "leaf_water_cap",
  Organ == "Xylem" ~ "wood_water_cap"
))

cap <- cap %>% mutate( Cft_conv = case_when(
  Organ == "Leaf" ~ Cft * 1/MPa2m,
  Organ == "Xylem" ~ Cft * 1/MPa2m * 1/1000 #* 1/WD
))

ggplot(cap) + geom_density(aes(x = Cft_conv, col = Organ))

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

p + geom_density(data = cap, aes(x = WD, fill = "obs"), alpha = .3, color = NA)

################################################################################
## Leaf Water Capacitance
## Leaf: gH2O g-1 dry weight MPa-1

leaf_water_cap_variable_id <- tbl(bety, "variables") %>%
  filter(name == "leaf_water_cap") %>% pull(id)
leaf_water_cap_fit <- tbl(bety, "priors") %>%
  filter(variable_id == leaf_water_cap_variable_id) %>%
  collect()

leaf_water_cap_prior <- rdistn(leaf_water_cap_fit) * (1/1000)
leaf_water_cap_data <- cap %>% filter(Organ == "Leaf")
leaf_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "leaf_water_cap")

prior_plot(prior = leaf_water_cap_prior,
           q = c(0,.995),
           plot_default = leaf_water_cap_default,
           title = sprintf("Leaf Water Cap"),
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
wood_water_cap_data <- cap %>% filter(Organ == "Xylem")
leaf_water_cap_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "leaf_water_cap")

prior_plot(prior = wood_water_cap_prior,
           # q = c(0,1),
           plot_default = leaf_water_cap_default,
           title = sprintf("Wood Water Cap"),
           type = "prior") +
  geom_density(data = wood_water_cap_data, aes(x = Cft_conv, fill = "obs"), alpha = .3, color = NA)

################################################################################
## SLA

get.trait.data()


################################################################################
##  Check that citations are in the database

source("/fs/data3/ecowdery/FRED/project_functions.R")

all_doi <- unique(na.omit(cap$doi))

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
