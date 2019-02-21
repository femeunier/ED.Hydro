library(SHELF) # remotes::install_version("SHELF", "1.3.0")
library(PEcAn.DB)
library(PEcAn.utils)
library(PEcAn.settings)

library(tidyverse)

source("/fs/data3/ecowdery/ED_Tropics/priors/prior_functions.R")
bety <- betyConnect("web/config.php")
################################################################################
# Load the tables

# Christoffersen2ED_id <- googlesheets4::as_sheets_id( "https://docs.google.com/spreadsheets/d/1voUxoLgOWVNrum8VgyLrKc4gC-rjp_ulrjrBZaL5MA8/edit?usp=sharing")
# googledrive::drive_download(file = Christoffersen2ED_id, "/fs/data3/ecowdery/ED_Tropics/priors/Christoffersen2ED.csv", overwrite = TRUE)

# ED_id <- googlesheets4::as_sheets_id("https://docs.google.com/spreadsheets/d/1gn_MBoq1Yvzx2QVPCaDyXbfpAv-EfeJoxdjzHzFwMVE/edit?usp=sharing")
# googledrive::drive_download(file = ED_id, "/fs/data3/ecowdery/ED_Tropics/priors/ED.csv", overwrite = TRUE)

Christoffersen2ED <- read.csv("/fs/data3/ecowdery/ED_Tropics/priors/Christoffersen2ED.csv", stringsAsFactors = FALSE) 

Christoffersen <- readxl::read_excel("~/ecowdery/ED_Tropics/priors/FATES-Hydraulics_Pecan_Priors.xlsx") %>% dplyr::rename(Christoffersen_name = Parameter) %>% dplyr::rename(Christoffersen_Equation = Equation)

ED <- read.csv("/fs/data3/ecowdery/ED_Tropics/priors/ED.csv", stringsAsFactors = FALSE) %>% dplyr::rename(ED_Equation = Equation)

priors <- full_join(Christoffersen, Christoffersen2ED) %>% full_join(., ED)
names(priors) <- str_replace_all(names(priors) ," ",".")

# sum(!is.na(priors$ED_name))

################################################################################
# Setup the variables we are interested in

variables <- c(
  "leaf_elastic_mod", "wood_elastic_mod",
  "leaf_psi_osmotic", "wood_psi_osmotic",
  "rwc_tlp_wood", 
  "leaf_water_sat", "wood_water_sat",
  "leaf_water_cap", "wood_water_cap",
  "leaf_psi_min", "wood_psi_min",
  "wood_Kmax",
  "wood_psi50",
  "wood_Kexp"
)
length(variables)

priors <- priors %>% filter(ED_name %in% variables)

################################################################################
# Calculate priors 

stats <- c("theor.min",
           "low.025", 
           "low0.25", 
           "mean",
           "upp.75",
           "upp.975",
           "theor.max")

priors_sub <- priors %>% select(one_of(
  c("ED_name",
    "Christoffersen_name",
    "BETY_variable_id", 
    "BETY_prior_id",
    "ED_units",
    "Christoffersen_units",
    stats)
))

priors_given <- priors_sub %>% filter(!is.na(Christoffersen_name))
priors_calc <- priors_sub %>% filter(is.na(Christoffersen_name))

# First deal with the parameters that have direct connections to Brad's priors

################################################################################
# Do unit conversions

wdns <- 1.000e3    # Liquid water density [kg/m3]
grav <- 9.80665    # Gravity acceleration  [m/s2]
MPa2m <- wdns / grav

rho_wood  <- 0.7099999785 # g cm-3 from PFT 3

for(i in seq_along(priors_given$Christoffersen_name)){
  print(sprintf("%i: %s (%s)", i, priors_given$ED_name[i], priors_given$Christoffersen_name[i]))
  
  if(!(priors_given$ED_units[i] == priors_given$Christoffersen_units[i])){
    print(sprintf("Need to convert from %s to %s", priors_given$Christoffersen_units[i], priors_given$ED_units[i]))
    
    if(priors_given$Christoffersen_units[i] == "MPa" & priors_given$ED_units[i] == "m"){
      priors_given[i, stats] <- priors_given[i, stats] * MPa2m
      print("Conversion complete")
      print("")
      
    }else if(priors_given$ED_name[i] == "leaf_water_sat"){
      # Leaf water saturation 
      # We are going to need to do a calculation on this 
      # but I don't actually have it done yet so let's just say it's fine. 
      # I'm using the default values from ED for PFT 3
      leaf_density_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000304) %>% collect()
      leaf_density <- rnorm(1000000, leaf_density_fit$parama, leaf_density_fit$paramb)
      mean(leaf_density)
      leaf_density[leaf_density < (0.1 * 1.e3)] <-  (0.1 * 1.e3)
      sum(leaf_density < (0.1 * 1.e3))
      mean(leaf_density)
      # plot(density(leaf_density))
    
      priors_given[i, stats] <- priors_given[i, stats] * (wdns/(mean(leaf_density)))
      print("Conversion complete")
      print("")
      
    }else if(priors_given$ED_name[i] == "wood_water_sat"){
      # Wood water saturation 
      # In this case, to get from Brad's units to ED units, we need to multiply 
      # thetas_node * (density of wood)
      # I am currently using the default wood density for PFT 3
      # but we could technically draw from a prior
      
      priors_given[i, stats] <- priors_given[i, stats] * (wdns/(rho_wood * 1e3))
      print("Conversion complete")
      print("")
      
      
    }else if(priors_given$ED_name[i] == "wood_Kmax"){

      priors_given[i, stats] <- priors_given[i, stats] / MPa2m
      print("Conversion complete")
      print("")
      
    }else{
      print("Still need to do conversion")
      print("")
    }
  }else{
    print("")
  }
  
}

j = 1
par(mfrow = c(1,1))

for(j in seq_along(priors_given$Christoffersen_name)){
  
  print(sprintf("%i: %s (%s)", j, priors_given$ED_name[j], priors_given$Christoffersen_name[j]))
  
  myfit <- get_fit(priors = priors_given, j, plot = TRUE)
  variable_id.in <- tbl(bety,"variables") %>% filter(name == priors_given$ED_name[j]) %>% pull(id)
  phylogeny.in <- "plants"
  distn.in <- myfit$ssq %>% dplyr::select(one_of("Normal", "Gamma", "Log normal", "Beta")) %>%
    which.min %>% names
  
  input_prior(bety, myfit, variable_id.in, phylogeny.in, distn.in)
  
  print("")
  
}

source("/fs/data3/ecowdery/ED_Tropics/priors/Sanity_plots.R")
