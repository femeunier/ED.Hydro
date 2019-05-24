# DONE

# wood_psi_tlp data

library(ED.Hydro.Helpers)
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php") # the path to my config.php

d1 <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Psitlp_Felicien_5-17-19/Psitlp_BETY_wood_psi_tlp.csv",  stringsAsFactors = FALSE) %>% select(-X)

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

wood_psi_tlp_id <- tbl(bety, "variables") %>% filter(name == "wood_psi_tlp") %>% pull(id)
wood_psi_tlp_fit <- tbl(bety, "priors") %>% filter(variable_id == wood_psi_tlp_id)%>% filter(distn == "lnorm") %>% collect()
wood_psi_tlp_prior <- data.frame(x = rdistn(wood_psi_tlp_fit, n = 100000))

insert_dat$y <- 0

ggplot() + geom_density(data = wood_psi_tlp_prior, aes(x = x), fill = "black", alpha = .3) +
  geom_density(data = insert_dat, aes(x = value), fill = "black", alpha = .5, color = NA) +
  geom_point(data = insert_dat, aes(x = value, y = y, color = as.factor(citation_id)), shape = "|", size = 10)


# Check if the data falls outside of the boudns of the prior and if that is
# actually a serious violation of the assumtions

which(insert_dat$value < min(wood_psi_tlp_prior))
which(insert_dat$value > max(wood_psi_tlp_prior))

# First just do a check to see how much of the data is already in bety
# Also a sanity check to make sure I'm not about to send in a bunch of duplicates

insert_dat <- insert_traits(insert_dat = insert_dat, check_only = TRUE, bety = bety)

# Insert the data

insert_dat <- insert_traits(insert_dat = insert_dat, bety = bety)

all(!is.na(insert_dat$trait_id))

#############

# leaf_psi_tlp data

rm(list=setdiff(ls(), "bety"))

d1 <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Psitlp_Felicien_5-17-19/Psitlp_BETY_leaf_psi_tlp.csv",  stringsAsFactors = FALSE) %>% select(-X)

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

leaf_psi_tlp_id <- tbl(bety, "variables") %>% filter(name == "leaf_psi_tlp") %>% pull(id)
leaf_psi_tlp_fit <- tbl(bety, "priors") %>% filter(variable_id == leaf_psi_tlp_id)%>% filter(distn == "lnorm") %>% collect()
leaf_psi_tlp_prior <- data.frame(x = rdistn(leaf_psi_tlp_fit, n = 100000))

insert_dat$y <- 0

ggplot() + geom_density(data = leaf_psi_tlp_prior, aes(x = x), fill = "black", alpha = .3) +
  geom_density(data = insert_dat, aes(x = value), fill = "black", alpha = .5, color = NA) +
  geom_point(data = insert_dat, aes(x = value, y = y, color = as.factor(citation_id)), shape = "|", size = 10)


# Check if the data falls outside of the boudns of the prior and if that is
# actually a serious violation of the assumtions

which(insert_dat$value < min(leaf_psi_tlp_prior))
which(insert_dat$value > max(leaf_psi_tlp_prior))

# First just do a check to see how much of the data is already in bety
# Also a sanity check to make sure I'm not about to send in a bunch of duplicates

insert_dat <- insert_traits(insert_dat = insert_dat, check_only = TRUE, bety = bety)

# Insert the data

insert_dat <- insert_traits(insert_dat = insert_dat, bety = bety)

all(!is.na(insert_dat$trait_id))
