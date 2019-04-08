library(taxize)
library(knitr)
library(ED.Hydro.Helpers)

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

pftid <- 1000000131
PFT_species <- tbl(bety, "pfts") %>% dplyr::rename(pft_id = id) %>% filter(pft_id == pftid) %>%
  inner_join(., tbl(bety, "pfts_species"), by = "pft_id") %>%
  inner_join(., tbl(bety, "species") %>% dplyr::rename(specie_id = id), by = "specie_id") %>%
  dplyr::select(one_of("pft_id", "name", "specie_id", "genus", "species", "scientificname")) %>%
  collect()

# datafile <- "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/Capacitance_BETY.csv"
# dat <- read.csv(datafile, stringsAsFactors = FALSE) %>% rename(species = Species)
#
# url <- "https://docs.google.com/spreadsheets/d/1feKS04I2eErSvQrSKLfPuLSHYqDGH7ccRfRh5M6tN0U/edit?usp=sharing"
#
# # googlesheets4::sheets_auth(use_oob = TRUE)
# dat2 <- googlesheets4::read_sheet(ss=url, na = "NaN", col_types = "cncccnccnnn") %>%
#   mutate(species_id = NA) %>%
#   mutate(citation_id = NA)
# 1
# species_all <- sort(unique(toupper(c(dat$species, dat2$species))))
#
# sp <- data.frame(submit_name = species_all,
#                  submit_bety_id = NA,
#                  submit_in_PFT = FALSE,
#
#                  accept_name = "",
#                  accept_bety_id = NA,
#                  accept_in_PFT = FALSE,
#
#                  sourceid = NA,
#                  score = NA,
#                  authority = NA,
#                  uri = NA,
#
#                  case = 0,
#
#                  stringsAsFactors = FALSE)
#
# write.csv(
#   x = sp,
#   file = "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species.csv")

sp = read.csv(
  file = "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species.csv",
  stringsAsFactors = FALSE)


i_range = 1
i_range <- 50:55
i_range <- seq_along(sp$submit_name)

for(i in i_range){

  if(sp$case[i] == 0){

    print(paste0(i, ": ", sp$submit_name[i]))

    # This is a bit redundant but I find it helpful for the debugging
    submit_bety <- FALSE
    submit_PFT  <- FALSE
    accept_bety <- FALSE
    accept_PFT  <- FALSE
    accept_name_exists <- FALSE

    # First check if the name is in BETY (without any changes/resolution)
    find_submit <- tbl(bety, "species") %>%
      filter(toupper(scientificname) == toupper(sp$submit_name[i])) %>%
      select(one_of("id", "genus", "species", "scientificname")) %>%
      collect()

    if(dim(find_submit)[1] == 1){
      submit_bety <- TRUE
      sp$submit_bety_id[i] <- find_submit$id
      if(find_submit$id %in% PFT_species$specie_id){
        submit_PFT  <- TRUE
        sp$submit_in_PFT[i] <- TRUE
      }
    }

    # Now put the name through the Taxanomic Name Resolution Service
    # Check that the species names are properly spelled and if they are accepted

    # Submit as a try() in case of connection error
    test_tnrs <- try(tnrs(query = sp$submit_name[i]))
    if(!class(test_tnrs) == "try-error"){
      sp$accept_name[i] = test_tnrs$acceptedname
      sp$sourceid[i] = test_tnrs$sourceid
      sp$score[i] = test_tnrs$score
      if("authority" %in% names(test_tnrs)) sp$uri[i] =  test_tnrs$authority
      if("uri" %in% names(test_tnrs)) sp$uri[i] = test_tnrs$uri

      # If the search retuns an accepted name, check to see if it's in the database and PFT
      accept_name_exists <- !sp$accept_name[i] == "" & !is.na(sp$accept_name[i])
      if(accept_name_exists){

        # If the names are the same, then find_accept is the same as find_submit
        # otherwise, look it up in bety
        if(toupper(sp$submit_name[i]) == toupper(sp$accept_name[i])){
          find_accept <- find_submit
        }else{
          find_accept <- tbl(bety, "species") %>%
            filter(toupper(scientificname) == toupper(sp$accept_name[i])) %>%
            select(one_of("id", "genus", "species", "scientificname")) %>%
            collect()
        }

        if(dim(find_accept)[1] == 1){
          accept_bety <- TRUE
          sp$accept_bety_id[i] <- find_accept$id
          if(find_accept$id %in% PFT_species$specie_id){
            accept_PFT  <- TRUE
            sp$accept_in_PFT[i] <- TRUE
          }
        }
      }
    } # End TNRS section

    ###########
    # Debugging

    # Both names in BETY
    submit_accept = sp$submit_bety_id[i] == sp$accept_bety_id[i]

    # They equal each other
    case01 <- submit_bety & accept_bety & submit_accept & accept_PFT # Best case, don't need to do anything.
    case02 <- submit_bety & accept_bety & submit_accept & !accept_PFT

    # They don't equal one another
    case03 <- submit_bety & accept_bety & !submit_accept & submit_PFT & accept_PFT
    case04 <- submit_bety & accept_bety & !submit_accept & submit_PFT & !accept_PFT
    case05 <- submit_bety & accept_bety & !submit_accept & !submit_PFT & accept_PFT
    case06 <- submit_bety & accept_bety & !submit_accept & !submit_PFT & !accept_PFT

    # Only one name in BETY

    # Submit not in bety, accept in bety
    case07 <- !submit_bety & accept_bety & accept_PFT
    case08 <- !submit_bety & accept_bety & !accept_PFT

    # Submit in bety, accept not in bety
    case09 <- submit_bety & submit_PFT & !accept_bety
    case10 <- submit_bety & !submit_PFT & !accept_bety

    # None of the names are in BETY
    case11 <- !submit_bety & !accept_bety & toupper(sp$submit_name[i]) == toupper(sp$accept_name[i])
    case12 <- !submit_bety & !accept_bety & toupper(sp$submit_name[i]) != toupper(sp$accept_name[i])

    # Special unlucky case that the TNRS could not find an accepted name.
    # Or TNRS returned a blank which is super lame.
    case13 <- !accept_name_exists
    if(case13){
      case01 = case02 = case03 = case04 = case05 = case06 = case07 = case08 =
      case09 = case10 = case11 = case12 = FALSE
    }

    if(sum(case01, case02, case03, case04, case05, case06, case07, case08,
           case09, case10, case11, case12, case13) > 1){
      print("More than one case is true!")
    }

    if(case01){sp$case[i] = 1}
    if(case02){sp$case[i] = 2}
    if(case03){sp$case[i] = 3}
    if(case04){sp$case[i] = 4}
    if(case05){sp$case[i] = 5}
    if(case06){sp$case[i] = 6}
    if(case07){sp$case[i] = 7}
    if(case08){sp$case[i] = 8}
    if(case09){sp$case[i] = 9}
    if(case10){sp$case[i] = 10}
    if(case11){sp$case[i] = 11}
    if(case12){sp$case[i] = 12}
    if(case13){sp$case[i] = 13}

    print(paste("Case", sp$case[i]))

    remove(case01, case02, case03, case04, case05, case06, case07, case08, case09, case10, case11, case12, case13)

    write.csv(x = sp, file = "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species_out.csv")

  } else {print(paste0(i, ": ", sp$submit_name[i], " already filled in."))}

} # End loop

write.csv(x = sp, file = "/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species.csv")

# view(sp)


#
# else { # This following section may not actually be necessary anymore. But I'll keep it for now.
#   print(paste0(i, ": ", sp$submit_name[i]))
#
#   g <- strsplit(sp$submit_name[i], " ") %>% unlist %>% .[1]
#   s <- strsplit(sp$submit_name[i], " ") %>% unlist %>% .[2]
#
#   find2 <- tbl(bety, "species") %>%
#     filter(grepl(g, genus, ignore.case = TRUE))   %>%
#     filter(grepl(s, species, ignore.case = TRUE)) %>% collect
#
#   if(dim(find2)[1]>=1){
#     if(dim(find)[1]>=1){
#       full_join(find, find2, by = c("id", "genus", "species", "scientificname")) %>% select(one_of("id", "genus", "species", "scientificname"))  %>%  print
#     }else{ find2 %>% select(one_of("id", "genus", "species", "scientificname"))  %>% print }
#   }
# }
# }
# }

# sum(is.na(sp$accept_bety_id))
#
#
# View(sp)
# test_tnrs <- list()
#
# for(i in seq_along(problem_names$species)){
#   print(i)
#   test_tnrs[[problem_names$species[i]]] <- try(tnrs(query = problem_names$species[i], source = "iPlant_TNRS"))
# }
# View(bind_rows(test_tnrs))
# View(problem_names)
#
# for(i in seq_along(problem_names$species)){
#   test_tnrs[[problem_names$species[i]]]$accept_name %>% print()
# }
#
#
