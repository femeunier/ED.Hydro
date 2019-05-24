# DONE

# wood_water_cap data

library(ED.Hydro.Helpers)
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php") # the path to my config.php

d1 <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Capacitance_Felicien_2-15-19/capacitance_BETY_wood_water_cap.csv",  stringsAsFactors = FALSE) %>% select(-X)

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

wood_water_cap_id <- tbl(bety, "variables") %>% filter(name == "wood_water_cap") %>% pull(id)
wood_water_cap_fit <- tbl(bety, "priors") %>% filter(variable_id == wood_water_cap_id) %>% collect()
wood_water_cap_prior <- data.frame(x = rdistn(wood_water_cap_fit, n = 100000))

insert_dat$y <- 0

ggplot() + geom_density(data = wood_water_cap_prior, aes(x = x), fill = "black", alpha = .3) +
  geom_density(data = insert_dat, aes(x = value), fill = "black", alpha = .5, color = NA) +
  geom_point(data = insert_dat, aes(x = value, y = y, color = as.factor(citation_id)), shape = "|", size = 10)


# Check if the data falls outside of the boudns of the prior and if that is
# actually a serious violation of the assumtions

which(insert_dat$value < min(wood_water_cap_prior))
which(insert_dat$value > max(wood_water_cap_prior))

# First just do a check to see how much of the data is already in bety
# Also a sanity check to make sure I'm not about to send in a bunch of duplicates

insert_dat <- insert_traits(insert_dat = insert_dat, check_only = TRUE, bety = bety)

# Insert the data

insert_dat <- insert_traits(insert_dat = insert_dat, bety = bety)

all(!is.na(insert_dat$trait_id))

#############

# leaf_water_cap

rm(list=setdiff(ls(), "bety"))

d1 <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Capacitance_Felicien_2-15-19/capacitance_BETY_leaf_water_cap.csv",  stringsAsFactors = FALSE) %>% select(-X)

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

leaf_water_cap_id <- tbl(bety, "variables") %>% filter(name == "leaf_water_cap") %>% pull(id)
leaf_water_cap_fit <- tbl(bety, "priors") %>% filter(variable_id == leaf_water_cap_id) %>% collect()
leaf_water_cap_prior <- data.frame(x = rdistn(leaf_water_cap_fit, n = 100000))

insert_dat$y <- 0

ggplot() + geom_density(data = leaf_water_cap_prior, aes(x = x), fill = "black", alpha = .3) +
  geom_density(data = insert_dat, aes(x = value), fill = "black", alpha = .5, color = NA) +
  geom_point(data = insert_dat, aes(x = value, y = y, color = as.factor(citation_id)), shape = "|", size = 10)


# Check if the data falls outside of the boudns of the prior and if that is
# actually a serious violation of the assumtions

which(insert_dat$value < min(leaf_water_cap_prior))
which(insert_dat$value > max(leaf_water_cap_prior))

# First just do a check to see how much of the data is already in bety
# Also a sanity check to make sure I'm not about to send in a bunch of duplicates

insert_dat <- insert_traits(insert_dat = insert_dat, check_only = TRUE, bety = bety)

# Insert the data

insert_dat <- insert_traits(insert_dat = insert_dat, bety = bety)

all(!is.na(insert_dat$trait_id))
