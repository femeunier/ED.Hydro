library(PEcAn.DB)
library(PEcAn.utils)
library(PEcAn.settings)

library(dplyr)
library(tidyr)
library(tidyverse)

library(googlesheets4)
library(stringr)

################################################################################
## Read in the dat
## Currently reads from google spreadsheets

bety <- betyConnect("web/config.php")

url <- "https://docs.google.com/spreadsheets/d/1feKS04I2eErSvQrSKLfPuLSHYqDGH7ccRfRh5M6tN0U/edit?usp=sharing"

dat <- read_sheet(url, na = "NaN", col_types = "cncccnccnnn") %>% 
  mutate(species_id = NA) %>% 
  mutate(citation_id = NA)

# Convert to MPa

dat$wood_Kmax  <- dat$wood_Kmax * 0.009804139432
dat$wood_psi50 <- dat$wood_psi50 / 0.009804139432

# Get species id

all_s <- unique(dat$species)

for(i in seq_along(all_s)){
  
  find <-tbl(bety, "species") %>% 
    filter(grepl(all_s[i], scientificname, ignore.case = TRUE)) %>% collect
  
  if(dim(find)[1] == 1){
    find_id <- find %>% pull(id)
    dat$species_id[which(dat$species == all_s[i])] <- find_id 
    
  } else {
    dat$species_id[which(dat$species == all_s[i])] <- NA 
    
    print(paste(all_s[i],"Not found"))
    
    g <- strsplit(dat$species[i], " ") %>% unlist %>% .[1]
    s <- strsplit(dat$species[i], " ") %>% unlist %>% .[2]
    
    tbl(bety, "species") %>% select(one_of("id", "genus", "species", "scientificname"))  %>%
      filter(grepl(g, genus, ignore.case = TRUE))   %>% 
      filter(grepl(s, species, ignore.case = TRUE)) %>% collect %>% print
  }
}


dat[dat$species == "Anacardium excelsum", "species_id"] <- 49943
dat[dat$species == "Brosimum utile", "species_id"] <- 30553
dat[dat$species == "cordia panamensis", "species_id"] <- 1000003460
dat[dat$species == "luehea seemannii", "species_id"] <- 50567
dat[dat$species == "manilkara bidentata", "species_id"] <- 543171
dat[dat$species == "schefflera morototoni", "species_id"] <- 51206

dat[dat$species == "Aspidosperma cruenta", "species_id"] <- 1000015879

sum(is.na(dat$species_id))

# Get citation id

for(i in seq_along(dat$citation_author)){
  
  find <-tbl(bety, "citations") %>% 
    filter(grepl(dat$citation_author[i], author, ignore.case = TRUE)) %>% filter(year == dat$citation_year[i]) %>% collect()
  
  
  if(dim(find)[1] == 1){
    dat$citation_id[i] <- find %>% pull(id)
    
  } else {
    dat$citation_id[i] <- NA
    print(paste(dat$citation_author[i],"Not found"))
  }
}
sum(is.na(dat$citation_id))

# Set treatment id

dat$treatment <- 2000000012


par(mfrow = c(2,2))
##############################################
## Wood Kmax


var_id <- tbl(bety, "variables") %>% filter(name == "wood_Kmax") %>% pull(id)

wood_Kmax <- dat %>% 
  filter(!is.na(species_id)) %>%
  filter(!is.na(wood_Kmax)) %>% 
  mutate(variable_id = var_id) %>% 
  na.omit %>% 
  mutate(trait_id = NA)

p <- tbl(bety, "priors") %>% filter(variable_id == var_id) %>% collect

plot(density(rgamma(n = 1000, shape = p$parama, rate = p$paramb)), col = "red", main = "Kmax")
lines(density(wood_Kmax$wood_Kmax), col = "blue")
legend("topright", lty = c(1, 1), col =  c("red", "blue"),  legend = c("prior", "data"))

for(i in seq_along(wood_Kmax$wood_Kmax)){
  
  check <- tbl(bety, "traits") %>% 
    filter(site_id == wood_Kmax$site_id[i]) %>%
    filter(specie_id == wood_Kmax$species_id[i]) %>%
    filter(citation_id == wood_Kmax$citation_id[i]) %>%
    filter(treatment_id == wood_Kmax$treatment[i]) %>% 
    filter(mean - wood_Kmax$wood_Kmax[i] < 10^-6)  %>% 
    filter(variable_id == wood_Kmax$variable_id[i]) %>% 
    collect
  
  
  if(dim(check)[1] == 0){
    
    insert.query <- sprintf("INSERT INTO traits (site_id, specie_id, citation_id, treatment_id, variable_id, mean, user_id, access_level, created_at, updated_at) VALUES(%.0f, %.0f, %.0f, %.0f, %.0f, %f, 1000000003, 4, NOW(), NOW()) RETURNING id;",
                            wood_Kmax$site_id[i], wood_Kmax$species_id[i], wood_Kmax$citation_id[i], 
                            wood_Kmax$treatment[i], wood_Kmax$variable_id[i], wood_Kmax$wood_Kmax[i])
    
    trait_id <- db.query(insert.query, bety$con)
    wood_Kmax$trait_id[i] <- trait_id
    
  }
}

View(wood_Kmax)



##############################################
## Wood Kexp


var_id <- tbl(bety, "variables") %>% filter(name == "wood_Kexp") %>% pull(id)

wood_Kexp <- dat %>% 
  filter(!is.na(species_id)) %>%
  filter(!is.na(wood_Kexp)) %>% 
  mutate(variable_id = var_id) %>% 
  na.omit %>% 
  mutate(trait_id = NA)


p <- tbl(bety, "priors") %>% filter(variable_id == var_id) %>% collect

plot(density(rnorm(1000, p$parama, p$paramb)), col = "red", main = "Kexp")
lines(density(wood_Kexp$wood_Kexp), col = "blue")
legend("topright", lty = c(1, 1), col =  c("red", "blue"),  legend = c("prior", "data"))


for(i in seq_along(wood_Kexp$wood_Kexp)){
  
  check <- tbl(bety, "traits") %>% 
    filter(site_id == wood_Kexp$site_id[i]) %>%
    filter(specie_id == wood_Kexp$species_id[i]) %>%
    filter(citation_id == wood_Kexp$citation_id[i]) %>%
    filter(treatment_id == wood_Kexp$treatment[i]) %>% 
    filter(mean - wood_Kexp$wood_Kexp[i] < 10^-6)  %>% 
    filter(variable_id == wood_Kexp$variable_id[i]) %>% 
    collect
  
  
  if(dim(check)[1] == 0){
    
    insert.query <- sprintf("INSERT INTO traits (site_id, specie_id, citation_id, treatment_id, variable_id, mean, user_id, access_level, created_at, updated_at) VALUES(%.0f, %.0f, %.0f, %.0f, %.0f, %f, 1000000003, 4, NOW(), NOW()) RETURNING id;",
                            wood_Kexp$site_id[i], wood_Kexp$species_id[i], wood_Kexp$citation_id[i], 
                            wood_Kexp$treatment[i], wood_Kexp$variable_id[i], wood_Kexp$wood_Kexp[i])
    
    trait_id <- db.query(insert.query, bety$con)
    wood_Kexp$trait_id[i] <- trait_id
    
  }
}

View(wood_Kexp)






##############################################
## psi50 

var_id <- tbl(bety, "variables") %>% filter(name == "wood_psi50") %>% pull(id)

wood_psi50 <- dat %>% 
  filter(!is.na(species_id)) %>%
  filter(!is.na(wood_psi50)) %>% 
  mutate(variable_id = var_id) %>% 
  na.omit %>% 
  mutate(trait_id = NA)


p <- tbl(bety, "priors") %>% filter(variable_id == var_id) %>% collect



plot(density(rnorm(1000, p$parama, p$paramb)), col = "red")
lines(density(wood_psi50$wood_psi50), col = "blue")

hist(wood_psi50$wood_psi50)
max(wood_psi50$wood_psi50)
tail(sort(wood_psi50$wood_psi50))

wood_psi50[which(wood_psi50$wood_psi50 >0),]

for(i in seq_along(wood_psi50$wood_psi50)){
  
  check <- tbl(bety, "traits") %>% 
    filter(site_id == wood_psi50$site_id[i]) %>%
    filter(specie_id == wood_psi50$species_id[i]) %>%
    filter(citation_id == wood_psi50$citation_id[i]) %>%
    filter(treatment_id == wood_psi50$treatment[i]) %>% 
    filter(mean - wood_psi50$wood_psi50[i] < 10^-6)  %>% 
    filter(variable_id == wood_psi50$variable_id[i]) %>% 
    collect
  
  
  if(dim(check)[1] == 0){
    
    insert.query <- sprintf("INSERT INTO traits (site_id, specie_id, citation_id, treatment_id, variable_id, mean, user_id, access_level, created_at, updated_at) VALUES(%.0f, %.0f, %.0f, %.0f, %.0f, %f, 1000000003, 4, NOW(), NOW()) RETURNING id;",
                            wood_psi50$site_id[i], wood_psi50$species_id[i], wood_psi50$citation_id[i], 
                            wood_psi50$treatment[i], wood_psi50$variable_id[i], wood_psi50$wood_psi50[i])
    
    trait_id <- db.query(insert.query, bety$con)
    wood_psi50$trait_id[i] <- trait_id
    
  }
}

View(wood_psi50)
