################################################################################
# Compare the old and new data Felicien gave me

library(ED.Hydro.Helpers)
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

dat_old <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/data_hydraulics.csv", na.strings = NaN,  stringsAsFactors = FALSE)

dat_new <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/conductivity_BETY.csv", na.strings = NaN,  stringsAsFactors = FALSE)

use_names <- intersect(names(dat_old), names(dat_new))

dat_old <- dat_old  %>% select(one_of(use_names))
dat_new <- dat_new  %>% select(one_of(use_names))

check <- do.call(paste0, dat_old) %in% do.call(paste0, dat_new)
all(check)
sum(!check)
which(!check)
dat_old[!check,]

