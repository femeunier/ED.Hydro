# DONE

# fineroot2leaf data

library(ED.Hydro.Helpers)
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php") # the path to my config.php

d1 <- read.csv(
  "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/FRED/FRED2_20180518_fineroot2leaf.csv",
  stringsAsFactors = FALSE) %>%
  select(-X)

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

pftid <- 1000000131
pft_priors <- tbl(bety,"pfts_priors") %>% filter(pft_id == pftid) %>% pull(prior_id)

fineroot2leaf_id <- tbl(bety, "variables") %>% filter(name == "fineroot2leaf") %>% pull(id)
fineroot2leaf_fit <- tbl(bety, "priors") %>% filter(variable_id == fineroot2leaf_id) %>%
  filter(id %in% pft_priors) %>% collect()
fineroot2leaf_prior <- data.frame(x = rdistn(fineroot2leaf_fit, n = 100000))

insert_dat$y <- 0

ggplot() +
  geom_density(data = fineroot2leaf_prior, aes(x = x), fill = "black", alpha = .3) +
  geom_density(data = insert_dat, aes(x = value), fill = "darkblue", alpha = .5, color = NA) +
  geom_point(data = insert_dat, aes(x = value, y = y, color = as.factor(species_id)), shape = "|", size = 10) + theme(legend.position="bottom") + xlim(0,2)



# Check if the data falls outside of the boudns of the prior and if that is
# actually a serious violation of the assumtions

which(insert_dat$value < min(fineroot2leaf_prior))
which(insert_dat$value > max(fineroot2leaf_prior))

# First just do a check to see how much of the data is already in bety
# Also a sanity check to make sure I'm not about to send in a bunch of duplicates

insert_dat <- insert_traits(insert_dat = insert_dat, check_only = TRUE, bety = bety)

# Insert the data

insert_dat <- insert_traits(insert_dat = insert_dat, bety = bety)

all(!is.na(insert_dat$trait_id))

