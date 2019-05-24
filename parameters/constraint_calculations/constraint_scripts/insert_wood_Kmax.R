# DONE

# wood_Kmax data

library(ED.Hydro.Helpers)
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php") # the path to my config.php

d1 <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Conductivity_Felicien_4-23-19/conductivity_BETY_wood_Kmax.csv",  stringsAsFactors = FALSE) %>% select(-X)

dat <- rbind.data.frame(d1)

# checks

unique(dat$variable_id)
unique(dat$treatment)

# There may be repeats that are entered through different data sheets
# need to be removed

insert_dat <- dat %>% distinct()

# There shouldn't be any NA's at this point but just in case:

insert_dat <- insert_dat %>% drop_na(-trait_id)

# Double check the data looks ok against the database

wood_Kmax_id <- tbl(bety, "variables") %>% filter(name == "wood_Kmax") %>% pull(id)
wood_Kmax_fit <- tbl(bety, "priors") %>% filter(variable_id == wood_Kmax_id) %>% collect()
wood_Kmax_prior <- data.frame(x = rdistn(wood_Kmax_fit, n = 100000))

insert_dat$y <- 0

ggplot() + geom_density(data = wood_Kmax_prior, aes(x = x), fill = "black", alpha = .3) +
  geom_density(data = insert_dat, aes(x = value), fill = "black", alpha = .5, color = NA) +
  geom_point(data = insert_dat, aes(x = value, y = y, color = as.factor(citation_id)), shape = "|", size = 10)

which(insert_dat$value < min(wood_Kmax_prior))
which(insert_dat$value > max(wood_Kmax_prior))

insert_dat2 <- insert_dat %>% filter(citation_id != 1000000105)

ggplot() + geom_density(data = wood_Kmax_prior, aes(x = x), fill = "black", alpha = .3) +
  geom_density(data = insert_dat2, aes(x = value), fill = "black", alpha = .5, color = NA) +
  geom_point(data = insert_dat2, aes(x = value, y = y, color = as.factor(citation_id)), shape = "|", size = 10)

# Check if the data falls outside of the boudns of the prior and if that is
# actually a serious violation of the assumtions

insert_dat <- insert_traits(insert_dat = insert_dat, check_only = TRUE, bety = bety)

# Insert the data

insert_dat <- insert_traits(insert_dat = insert_dat, bety = bety)

all(!is.na(insert_dat$trait_id))
