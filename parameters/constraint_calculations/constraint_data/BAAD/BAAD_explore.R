# devtools::install_github("richfitz/datastorr")
# devtools::install_github("traitecoevo/baad.data")
# 
library(PEcAn.all)
library(tidyverse)
library(rcrossref)
library(datastorr)
library(baad.data)

source("/fs/data3/ecowdery/FRED/project_functions.R")

################################################################################
# Load the species from the PFT

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

pftid <- 1000000132 # I'm using the PFT I made for ED_Hydro

tropical_species <- tbl(bety, "pfts") %>% dplyr::rename(pft_id = id) %>% filter(pft_id == pftid) %>%
  inner_join(., tbl(bety, "pfts_species"), by = "pft_id") %>% 
  inner_join(., tbl(bety, "species") %>% dplyr::rename(specie_id = id), by = "specie_id") %>% 
  dplyr::select(one_of("pft_id", "specie_id", "genus", "species", "scientificname")) %>% 
  collect()

################################################################################
# Load the BAAD data

# baad <- baad.data::baad_data()

# This doesn't always work in Rstudio so I saved it as an Rds file using the terminal
load("/fs/data3/ecowdery/BAAD/BAAD.Rds")
baad <- BAAD

# write.csv(baad$data, "/fs/data3/ecowdery/BAAD/BAAD_data.csv")
# write.csv(baad$dictionary, "/fs/data3/ecowdery/BAAD/BAAD_dictionary.csv")
# write.csv(baad$metadata, "/fs/data3/ecowdery/BAAD/BAAD_metadata.csv")
# write.csv(baad$methods, "/fs/data3/ecowdery/BAAD/BAAD_methods.csv")


BAAD <- baad$data
### SAVE ###
BAAD_versions <- list()
BAAD_versions$BAAD_original <- BAAD

dictionary <- baad$dictionary

#############################################
# Subset the data to only the columns we need 

meta <- c(
  "studyName",
  "location",
  "latitude",
  "longitude"
)

ident <- c(
  "vegetation",
  "species",
  "speciesMatched", 
  "pft"
)

traits_load <- c(
  "m.lf",  # leaf biomass
  "m.rf",  # fine root biomass
  "ma.ilf" # LMA kg/m2 
)

stats <- c() 

keep_cols <- c(traits_load, stats, meta, ident)
keep_cols %in% names(BAAD)

BAAD <- BAAD %>% select(one_of(keep_cols)) 

#############################
# Calculate any new variables

# Calculate fine root to leaf ratio
BAAD <- BAAD %>% mutate(m.q = BAAD$m.rf/BAAD$m.lf) 
traits_load <- c(traits, "m.q")

#############################
# Choose vars to proceed with

sum(!is.na(BAAD$m.q))
traits <- "m.q"

sum(!is.na(BAAD$ma.ilf))
traits <- "ma.ilf"

BAAD <- BAAD %>% select(-c(setdiff(traits_load, traits)))

### SAVE ###
BAAD_versions$BAAD_keep <- BAAD

BAAD <- BAAD %>% 
  gather(key = "trait" , value = "value", traits) %>%
  filter_at("value", any_vars(!is.na(.)))


#####################################
# Insert variable ids 
# Right now this list is made by hand

BAAD <- BAAD %>% mutate( variable_id = case_when(
  trait == "m.q" ~ 21,
  trait == "ma.ilf" ~ 254,
  TRUE ~ NA_real_
))

unique(BAAD$variable_id)
if(sum(is.na(BAAD$variable_id))>0){print("YOU NEED IDS FOR ALL VARIABLES!")}

### SAVE ###
BAAD_versions$BAAD_gather <- BAAD

# Tackle the problem where species is not the same as species matched since either
# of the entries could match up with the other data

BAAD <- BAAD %>% mutate(scientificname = case_when(
  species == speciesMatched ~ speciesMatched, 
  TRUE ~ ""
))


problem <- BAAD %>% filter(scientificname == "") %>% 
  select(one_of("species", "speciesMatched", "scientificname")) %>% 
  distinct()

fixed <- matrix(0, ncol = ncol(BAAD), nrow = 0) %>% as.data.frame()
colnames(fixed) <- names(BAAD)

for(i in 1:nrow(problem)){
  old <-  problem[i,] %>% dplyr::select(-"scientificname")
  if(old$speciesMatched %in% tropical_species$scientificname){
    new <- data.frame(c(old, scientificname = old$speciesMatched), stringsAsFactors = FALSE)
    fixed <- rbind.data.frame(fixed, new)
    
  }else if(old$species %in% tropical_species$scientificname){
    new <- data.frame(c(old, scientificname = old$species), stringsAsFactors = FALSE)
    fixed <- rbind.data.frame(fixed, new)
  }
}


#################################################################
# Create the final dataset that has been subsetted by PFT species
# 
join_fixed <- inner_join(
  inner_join(BAAD %>% 
               select(-one_of("scientificname")), fixed, by = c("species", "speciesMatched")) %>% 
    select(-one_of("species", "speciesMatched")), 
  tropical_species, by = "scientificname")

join_good <- inner_join(BAAD %>% 
                          filter(scientificname != "") %>% 
                          select(-one_of("species", "speciesMatched")), 
                        tropical_species, by = "scientificname")

BAAD <- rbind.data.frame(join_fixed, join_good)
unique(BAAD$studyName)

### SAVE ###
BAAD_versions$BAAD_joined <- BAAD

################################################################################
# Play around with plotting the data

BAAD_plot <- BAAD %>% filter(trait == "ma.ilf") 
BAAD_plot$SLA <- 1/BAAD$value
mean(BAAD_plot$SLA)

p <- ggplot(BAAD_plot)

p + geom_density(aes(x = SLA))
p + geom_density(aes(x = SLA, color = pft))

give.n <- function(x){
  return(data.frame(y = min(x), label = paste0("\n",paste0("n = ",length(x)))))
}


p1 <- ggplot(BAAD_plot) + 
  geom_density(aes(x = value)) + 
  coord_flip()  + 
  scale_y_reverse() +
  geom_vline(aes(xintercept = mean(value)), size = 1, color = "gray")

p2 <- ggplot(BAAD_plot, aes(x = as.factor(location), y = value)) + 
  geom_boxplot() +
  geom_jitter(aes(x = as.factor(location), y = value, color = as.factor(studyName)), width = .1, size = 4, alpha = .5) +
  stat_summary(fun.data = give.n, geom = "text", size = 6)

p1 <- ggplot(BAAD_plot) + 
  geom_density(aes(x = SLA)) + 
  coord_flip()  + 
  scale_y_reverse() +
  geom_vline(aes(xintercept = mean(SLA)), size = 1, color = "gray")

p2 <- ggplot(BAAD_plot, aes(x = as.factor(location), y = SLA)) + 
  geom_boxplot() +
  geom_jitter(aes(x = as.factor(location), y = SLA, color = as.factor(studyName)), width = .1, size = 4, alpha = .5) +
  stat_summary(fun.data = give.n, geom = "text", size = 6)

lay <- rbind(c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2),
             c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2))

png(filename = "/fs/data3/ecowdery/BAAD/test_figure.png", height = 600, width = 1200, units = "px" )
grid.arrange(p1,p2, layout_matrix = lay)
dev.off()

################################################################################
## Citations

# I'm so confused - rcrossref isn't working in rstudio. 
# I think it's a problem with the addin
# Just go run Rscript /fs/data3/ecowdery/FRED/FRED_citations.R in the command line

# source("/fs/data3/ecowdery/FRED/FRED_citations.R")
# system2("Rscript /fs/data3/ecowdery/FRED/FRED_citations.R")


studies <- BAAD$studyName %>% unique

dc <- list()


for(i in seq_along(studies)){
  bib <- baad$bib[studies[i]]
  
  check_doi <- tbl(bety, "citations") %>% filter(doi == bib[[studies[i]]]$doi) %>% collect()
  
  queries <- data.frame(include = c("user_id", "created_at", "updated_at"), 
                        values = c("1000000003", "NOW()", "NOW()"), 
                        stringsAsFactors = FALSE)
  
  if(nrow(check_doi) == 0){
    
    if(!is.null(bib[[studies[i]]]$author)){
      new <- c("author", paste0("'", paste(bib[[studies[i]]]$author, collapse = ", ")
                                %>% clean_chars(), "'"))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bib[[studies[i]]]$year)){
      new <- c("year", paste0(as.numeric(bib[[studies[i]]]$year)))
      queries <- rbind.data.frame(queries, new)
    }              
    if(!is.null(bib[[studies[i]]]$title)){
      new <- c("title", paste0("'", paste(bib[[studies[i]]]$title, collapse = ", ")
                               %>% clean_chars(), "'"))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bib[[studies[i]]]$journal)){
      new <- c("journal", paste0("'", paste(bib[[studies[i]]]$journal, collapse = ", ")
                                 %>% clean_chars(), "'"))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bib[[studies[i]]]$volume)){
      new <- c("vol", paste0(as.numeric(bib[[studies[i]]]$volume)))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bib[[studies[i]]]$pg)){
      new <- c("pg", paste0("'", bib[[studies[i]]]$pages %>% 
                              str_replace(pattern = "--",replacement = "-") %>% 
                              str_squish, "'"))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bib[[studies[i]]]$url)){
      new <- c("url", paste0("'", bib[[studies[i]]]$url %>% str_squish, "'"))
      queries <- rbind.data.frame(queries, new)
    }
    if(!is.null(bib[[studies[i]]]$doi)){
      new <- c("doi", paste0("'", bib[[studies[i]]]$doi %>% str_squish, "'"))
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
  dc[[i]] = data.frame(studyName = studies[i], citation_id = citation_id, doi = bib[[studies[i]]]$doi, stringsAsFactors = FALSE)
}

dc <- do.call(rbind.data.frame, dc) 

BAAD <- inner_join(BAAD, dc, by = "studyName") 

### SAVE ### 

BAAD_versions$BAAD_citations <- BAAD

################################################################################
## Sites

# Specific changes for two sites
BAAD <- BAAD %>% 
  mutate(latitude = 
           case_when(location == "Inpa" ~ -16.06,
                     location == "La Chonta" ~ -15.47,
                     TRUE ~ latitude)) %>%
  mutate(longitude = 
           case_when(location == "Inpa" ~ -61.42,
                     location == "La Chonta" ~ -62.55,
                     TRUE ~ longitude))

dat_sites <- BAAD %>% select(one_of("latitude", "longitude", "studyName", "location", "citation_id", "doi")) %>% distinct()
dat_sites$site_id <- NA

for(i in 1:nrow(dat_sites)){
  
  lat <- as.numeric(dat_sites[i,"latitude"])
  lon <- as.numeric(dat_sites[i,"longitude"])
  test1 <- nearby_sites(lat, lon, interval = 0, bety$con)
  if(nrow(test1) == 1){
    dat_sites[i, "site_id"] <- test1$id
  }else{
    
    print(paste("Nothing conclusive found for", dat_sites[i, "location"]))
    print(paste("Lookup:", dat_sites[i, "doi"]))
    print(nearby_sites(lat, lon, interval = .5, bety$con))
    
  }
}

dat_sites

# Manually add in a site
dat_sites[dat_sites$studyName == "Kenzo2009", "site_id"] <- 1000004978
dat_sites

BAAD <- inner_join(BAAD, dat_sites %>% select(one_of("location", "site_id")))

################################################################################
## Treatment

# I'm assuming everything is in situ observation

BAAD$treatment <- 2000000012


################################################################################
## Finally put things in BETY



######################
# Fine root allocation

var_id <- 21

in_dat <- BAAD %>% 
  mutate(variable_id = var_id) %>%
  filter(!is.na(specie_id)) %>%
  filter(!is.na(value))

p <- tbl(bety, "priors") %>% filter(variable_id == var_id) %>% collect

lines <- list()
for(i in 1:nrow(p)){
  if(p[[i, "distn"]] == "lnorm"){
    a = p[[i, "parama"]]
    b = p[[i, "paramb"]]
    lines[[i]] <- rlnorm(n = 1000, meanlog = a, sdlog = b)
  }
  if(p[[i, "distn"]] == "norm"){
    a = p[[i, "parama"]]
    b = p[[i, "paramb"]]
    lines[[i]] <- rnorm(n = 1000, mean = a,  sd = b)
  }
}

plot(density(in_dat$value), col = "blue")

for(i in seq_along(lines)){
  lines(density(lines[[i]]), col = "red")
}

legend("topright", lty = c(1, 1), col =  c("red", "blue"),  legend = c("prior", "data"))

in_dat$trait_id <- NA

for(i in seq_along(in_dat$value)){
  
  check <- tbl(bety, "traits") %>% 
    filter(site_id == in_dat$site_id[i]) %>%
    filter(specie_id == in_dat$specie_id[i]) %>%
    filter(citation_id == in_dat$citation_id[i]) %>%
    filter(treatment_id == in_dat$treatment[i]) %>% 
    filter(variable_id == in_dat$variable_id[i]) %>% 
    collect()
  check <- check %>% filter(near(mean, in_dat$value[i]))
  
  
  
  if(dim(check)[1] == 0){
    
    insert.query <- sprintf("INSERT INTO traits (site_id, specie_id, citation_id, treatment_id, variable_id, mean, user_id, access_level, created_at, updated_at) VALUES(%.0f, %.0f, %.0f, %.0f, %.0f, %.10f, 'Imported from BAAD version %s', 1000000003, 4, NOW(), NOW()) RETURNING id;",
                            in_dat$site_id[i], in_dat$specie_id[i], 
                            in_dat$citation_id[i], in_dat$treatment[i], 
                            in_dat$variable_id[i], in_dat$value[i],
                            baad.data::baad_data_version_current())
    
    trait_id <- db.query(insert.query, bety$con)
    print(paste("Inserted", trait_id))
    
  }else{
    trait_id <- check$id
    print(paste("Found", trait_id))
    insert.query2 <- sprintf("UPDATE traits SET notes = 'Imported from BAAD version %s' WHERE id = %.0f RETURNING id;", 
            baad.data::baad_data_version_current(), trait_id)
  }
  trait_id <- db.query(insert.query2, bety$con)
  in_dat$trait_id[i] <- trait_id
}


test <- inner_join(BAAD, in_dat)
