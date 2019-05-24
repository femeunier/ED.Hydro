# Deal with cases

library(taxize)
library(knitr)
library(ED.Hydro.Helpers)

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")
pftid <- 1000000131

sp = read.csv(
  file = "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species.csv",
  stringsAsFactors = FALSE) %>% mutate(submit_name = str_to_sentence(submit_name))
sp <- sp %>% select(-X)
sp <- sp[order(sp$case),]

unique(sp$case)

case_accept_set <- c(7,8,12)

sp <- sp %>%
  mutate(use = case_when(case %in% case_accept_set ~ "accept",
                         TRUE ~ "submit"),
         proposed_bety_name = case_when(case %in% case_accept_set ~ accept_name,
                               TRUE ~ submit_name),
         reviewed = case_when(case %in% case_accept_set ~ as.logical(FALSE),
                              case == 13 ~ FALSE,
                              TRUE ~ TRUE),
         bety_name = NA)

idx <- which(is.na(sp$bety_name) & sp$reviewed)
sp$bety_name[idx] <- sp$proposed_bety_name[idx]

  # mutate(bety_id = case_when(case %in% case_accept_set ~ accept_bety_id,
  #                            TRUE ~ submit_bety_id)) %>%
  # mutate(in_PFT = case_when(case %in% case_accept_set ~ accept_in_PFT,
  #                           TRUE ~ submit_in_PFT))

View(sp %>% select(one_of("submit_name", "case", "proposed_bety_name" , "bety_name", "reviewed")))

# BEGIN CASE 7

sp[which(sp$submit_name == "Acinodendron pohlianum"),"reviewed"] <- TRUE
sp[which(sp$submit_name == "Antirrhea trichantha"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Aspidosperma cruenta"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Bauhinia variegate"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Bursera simarouba"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Chrysophllum cainito"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Hymenaea stignocarpa"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Hymenea courbaril"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Miconia pohliana"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Myrsine ferruginea"), "reviewed"] <- TRUE

idx <- which(is.na(sp$bety_name) & sp$reviewed)
sp$bety_name[idx] <- sp$proposed_bety_name[idx]

# THUS ENDS CASE 7 HAVE I MENTIONED I HATE MY LIFE

sp[which(sp$submit_name == "Austromyrtus bidwillii"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Eugenia ampullaria"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Eugenia bankensis"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Eugenia muelleri"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Millettia cubitti"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Piptadenia constricta"), "reviewed"] <- TRUE
sp[which(sp$submit_name == "Syzygium latilimbum"), "reviewed"] <- TRUE

idx <- which(is.na(sp$bety_name) & sp$reviewed)
sp$bety_name[idx] <- sp$proposed_bety_name[idx]

# NOW WE ARE DONE WITH CASE 8 ARE YOU HAVING FUN

sp[which(sp$submit_name == "Michelia hypolampra"), "bety_name"] <- "Magnolia hypolampra"
sp[which(sp$submit_name == "Michelia hypolampra"), "reviewed"]  <- TRUE

# CASE 12 DONE NOW IT GETS INTERESTING

sp[which(sp$submit_name == "Byrsonima crassa"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Symplocos mosenii"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Agathis borneensis"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Aglaia glabrata"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Canarium caudatum"), "reviewed"]  <- TRUE
sp[grep("Castanopsis", sp$submit_name), "reviewed"]  <- TRUE
sp[grep("Castanopsis", sp$submit_name), "bety_name"]  <- "Castanopsis indica"
sp[which(sp$submit_name == "Cordia collococa"), "bety_name"] <- "Cordia nodosa"
sp[which(sp$submit_name == "Cordia collococa"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Cotylelobium burckii"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Diospyros brachiata"), "bety_name"] <- "Diospyros dictyoneura"
sp[which(sp$submit_name == "Diospyros brachiata"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Diospyros mindanaensis"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Dipterocarpus globosus"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Dryobalanops aromatica"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Dryobalanops sumatrensis"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Dussia munda"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Galbulimima baccata"), "bety_name"] <- "Galbulimima belgraveana"
sp[which(sp$submit_name == "Galbulimima baccata"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Heritiera sumatrana"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Isonandra lanceolata"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Lophopetalum subobovatum"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Mallotus wrayi"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Payena endertii"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Pentace adenophora"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Pistacia weinmannifolia"), "bety_name"] <- "Pistacia weinmanniifolia"
sp[which(sp$submit_name == "Pistacia weinmannifolia"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Santiria mollis"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Shorea ovalis"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Sindora leiocarpa"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Stemonurus umbellatus"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Tachigalia versicolor"), "bety_name"] <- "Tachigali versicolor"
sp[which(sp$submit_name == "Tachigalia versicolor"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Trimenia neocaledonica"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Zygogynum baillonii"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Zygogynum bicolor"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Zygogynum pomiferum"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Zygogynum queenslandianum"), "reviewed"]  <- TRUE
sp[which(sp$submit_name == "Zygogynum semecarpoides"), "reviewed"]  <- TRUE

idx <- which(is.na(sp$bety_name) & sp$reviewed)
sp$bety_name[idx] <- sp$proposed_bety_name[idx]

# WOW ARE WE REALLY DONE? PROBABLY NOT
sum(is.na(sp$bety_name))
# HOLY SHIT IT'S ZERO ... FOR NOW
# probably save that
write.csv(sp, "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species_reviewed.csv")

################################################################################
# Next just write a script that goes through each of the species
# and if it's not in the database, put it in and add it to the pft.
# Don't use any of the old information.
# Just link the species id with the submit name.

library(taxize)
library(knitr)
library(ED.Hydro.Helpers)

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")
pftid <- 1000000131

sp1 <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species_reviewed.csv", stringsAsFactors = FALSE) %>% select(-X)

final <- sp1 %>% mutate(bety_name = str_trim(bety_name)) %>%
  mutate(scientificname = bety_name) %>%
  mutate(test = bety_name) %>%
  separate(test, c("genus", "species"), sep = " ", extra = "drop") %>%
  mutate(bety_id = as.numeric(NA), in_PFT = as.logical(FALSE)) %>%
  select(one_of("submit_name", "genus", "species", "scientificname", "uri", "bety_id", "in_PFT"))


i = 1
for(i in seq_along(final$scientificname)){

  bn <- tolower(final$scientificname[i])
  check <- tbl(bety, "species") %>%
    filter(tolower(scientificname) == bn) %>% collect()

  if(nrow(check) == 1){
    final$bety_id[i] <- check$id
    print(sprintf("%i | %s already entered as %.0f", i, final$scientificname[i], final$bety_id[i]))
  }else if(nrow(check) == 0){

    insert.species.query <- sprintf("INSERT INTO species (genus, species, scientificname, notes, created_at, updated_at) VALUES('%s', '%s', '%s', '%s', NOW(), NOW()) RETURNING id;",
                                    final$genus[i], final$species[i], final$scientificname[i], final$uri[i])
    paste(insert.species.query)
    species_id <- db.query(insert.species.query, bety$con)
    species_id <- species_id$id
    final$bety_id[i] <- species_id
    print(sprintf("%i | %s entered as %.0f", i, final$scientificname[i], final$bety_id[i]))

    insert.pft.query <- sprintf("INSERT INTO pfts_species (pft_id, specie_id, created_at, updated_at) VALUES(%.0f, %.0f, NOW(), NOW()) RETURNING id;",
                                pftid, species_id)
    db.query(insert.pft.query, bety$con)

    d <- tbl(bety, "pfts_species") %>% filter(pft_id == pftid) %>% filter(specie_id == species_id) %>% collect()
    final$in_PFT[i] <- dim(d)[1]==1

  }
}


for(j in seq_along(final$in_PFT)){
  if(!final$in_PFT[j]){
    betyid <- final$bety_id[j]
    d <- tbl(bety, "pfts_species") %>% filter(pft_id == pftid) %>% filter(specie_id == betyid) %>% collect()
    if(nrow(d) == 0){
      insert.pft.query <- sprintf("INSERT INTO pfts_species (pft_id, specie_id, created_at, updated_at) VALUES(%.0f, %.0f, NOW(), NOW()) RETURNING id;",
                                  pftid, final$bety_id[j])
      db.query(insert.pft.query, bety$con)
    }
    d <- tbl(bety, "pfts_species") %>% filter(pft_id == pftid) %>% filter(specie_id == betyid) %>% collect()

    final$in_PFT[j] <- dim(d)[1]==1
  }else if(nrow(d)==1){
    final$in_PFT[j] <- TRUE
  }
}

##############################

all(final$in_PFT)
all(!is.na(final$bety_id))
all(is.numeric(final$bety_id))

write.csv(final, file = "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species_final.csv")
