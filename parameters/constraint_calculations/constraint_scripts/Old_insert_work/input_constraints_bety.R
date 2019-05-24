################################################################################
## Read in the dat
## Currently reads from google spreadsheets

library(ED.Hydro.Helpers)
priors <- prior_load_data()

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

url <- "https://docs.google.com/spreadsheets/d/1feKS04I2eErSvQrSKLfPuLSHYqDGH7ccRfRh5M6tN0U/edit?usp=sharing"


# googlesheets4::sheets_auth(use_oob = TRUE)
dat <- googlesheets4::read_sheet(ss=url, na = "NaN", col_types = "cncccnccnnn") %>%
  mutate(species_id = NA) %>%
  mutate(citation_id = NA)

wdns <- 1.000e3
grav <- 9.80665
MPa2m <- wdns / grav

###############################################################################
# Convert Units and check against priors


## Kmax
##
## Data Units :kg     m-1 MPa-1 s-1
## ED units:   kg H2O m-1       s-1
##

range(dat$wood_Kmax, na.rm = TRUE)

wood_Kmax_data  <- dat$wood_Kmax[!is.na(dat$wood_Kmax)] / MPa2m
mean(wood_Kmax_data)

variable_id.in <- tbl(bety, "variables") %>%
  filter(name == "wood_Kmax") %>% pull(id)
wood_Kmax_fit <- tbl(bety, "priors") %>%
  filter(variable_id == variable_id.in) %>% collect()
wood_Kmax_fit <- tbl(bety, "priors") %>%
  filter(variable_id == variable_id.in) %>%
  collect()
wood_Kmax_prior <- rdistn(wood_Kmax_fit)

mean(wood_Kmax_prior)

df_Kmax <- rbind.data.frame(
  data.frame( value = wood_Kmax_prior, key = "wood_Kmax_prior", stringsAsFactors = FALSE),
  data.frame( value = wood_Kmax_data, key = "wood_Kmax_data", stringsAsFactors = FALSE)
)

p_Kmax <- ggplot(df_Kmax) +
  geom_density(aes(x = value, col = key))  +
  geom_vline(aes(xintercept =  get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "wood_Kmax"))) +
  ggtitle("Kmax") +
  xlim(0, 0.2)

## psi50
##
## Data Units : MPa
## ED units:    m


default_wood_density <-  get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "rho")
default_wood_psi50 <- (-1.09 - (3.57 * default_wood_density ^ 1.73)) * MPa2m

wood_psi50_data  <- dat$wood_psi50[!is.na(dat$wood_psi50)] * MPa2m

variable_id.in <- tbl(bety, "variables") %>%
  filter(name == "wood_psi50") %>% pull(id)
wood_psi50_fit <- tbl(bety, "priors") %>%
  filter(variable_id == variable_id.in) %>% collect()
wood_psi50_fit <- tbl(bety, "priors") %>%
  filter(variable_id == variable_id.in) %>%
  collect()
wood_psi50_prior <- rdistn(wood_psi50_fit)

mean(wood_psi50_prior)

df_psi50 <- rbind.data.frame(
  data.frame( value = wood_psi50_prior, key = "wood_psi50_prior", stringsAsFactors = FALSE),
  data.frame( value = wood_psi50_data, key = "wood_psi50_data", stringsAsFactors = FALSE)
)

p_psi50 <- ggplot(df_psi50) +
  geom_density(aes(x = value, col = key))  +
  geom_vline(aes(xintercept =  get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", "wood_psi50"))) +
  geom_vline(aes(xintercept =  default_wood_psi50)) +
  ggtitle("psi50")

## Kexp
##
## Data Units : unitless
## ED units:    unitless


default_wood_Kexp <- 0.544 * 4. * (-default_wood_psi50 / MPa2m) ^ (-0.17)

# Why are there Kexp values that are REALLY negative? Need to check those units?
# And check the google spreadsheet
wood_Kexp_data  <- dat$wood_Kexp[!is.na(dat$wood_Kexp) & dat$wood_Kexp > 0]

variable_id.in <- tbl(bety, "variables") %>%
  filter(name == "wood_Kexp") %>% pull(id)
wood_Kexp_fit <- tbl(bety, "priors") %>%
  filter(variable_id == variable_id.in) %>% collect()
wood_Kexp_fit <- tbl(bety, "priors") %>%
  filter(variable_id == variable_id.in) %>%
  filter(distn == "norm") %>%
  collect()
wood_Kexp_prior <- rdistn(wood_Kexp_fit)


plot(density(wood_Kexp_prior))
abline(v = default_wood_Kexp)
mean(wood_Kexp_data)


df_Kexp <- rbind.data.frame(
  data.frame( value = wood_Kexp_prior, key = "wood_Kexp_prior", stringsAsFactors = FALSE),
  data.frame( value = wood_Kexp_data, key = "wood_Kexp_data", stringsAsFactors = FALSE)
)

p_Kexp <- ggplot(df_Kexp) +
  geom_density(aes(x = value, col = key))  +
  geom_vline(aes(xintercept =  default_wood_Kexp)) +
  ggtitle("Kexp")


gridExtra::grid.arrange(p_Kmax, p_psi50, p_Kexp, ncol = 1)


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
