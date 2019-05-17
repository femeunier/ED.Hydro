# Prepare the template data

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

datapath <- "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Template_data"
datafile <- ""

dat_in <- read.csv(file.path(datapath, paste0(datafile,".csv")), na.strings = NaN,  stringsAsFactors = FALSE)

#------------------------------------------------------------------------------#
# Data I'm removing for various reasons,
# some of these decisions may be wrong and we can come back to them,
# but if I don't feel comfortable adding it in to the database, I won't.


#------------------------------------------------------------------------------#
# References

tmp1 <- file.path(datapath, "tmp", paste0(datafile,"_tmp1", ".csv"))
write.csv(dat, file = tmp1)

#------------------------------------------------------------------------------#
# Sites

dat <- read.csv(file = tmp1, na.strings = NaN,  stringsAsFactors = FALSE) %>%
  select(-one_of("X","X.1"))

tmp2 <- file.path(datapath, "tmp", paste0(datafile,"_tmp2", ".csv"))
write.csv(dat, file = tmp2)

#------------------------------------------------------------------------------#
# Species

dat <- read.csv(file = tmp2, na.strings = NaN,  stringsAsFactors = FALSE) %>%
  select(-one_of("X","X.1"))

tmp3 <- file.path(datapath, "tmp", paste0(datafile,"_tmp3", ".csv"))
write.csv(dat, file = tmp3)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Preparation of data

dat <- read.csv(file = tmp3, na.strings = NaN,  stringsAsFactors = FALSE) %>% select(-one_of("X","X.1"))


var <- "wood_density"
varid <- wood_density_id
df <- dat %>%
  select(one_of(var, "species_id", "citation_id", "site_id")) %>%
  rename("var" = var) %>%
  na.omit() %>%
  distinct() %>%
  mutate(treatment = 2000000012, trait_id = as.numeric(NA), var_id = varid)

write.csv(df, file.path(datapath, paste0(datafile,"_",var,".csv")))
