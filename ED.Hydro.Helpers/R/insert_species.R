#' Find nearby sites in BETY
#'
#' @param sp
#' @param case_accept_set
#' @param bety
#' @return sp
#' @export

insert_species <- function(sp, case_accept_set, bety){

  p1 <- !all(c("use", "proposed_bety_name", "reviewed", "bety_name") %in% c(names(sp), "bety_name"))
  p2 <- all(c("use", "proposed_bety_name", "reviewed", "bety_name") %in% c(names(sp), "bety_name"))

  review_set <- union(case_accept_set, c(7, 11, 12, 13))

  if(p1){
    sp <- sp %>%
      mutate(use = case_when(case %in% case_accept_set ~ "accept",
                             TRUE ~ "submit"),
             proposed_bety_name = case_when(case %in% case_accept_set ~ accept_name,
                                            TRUE ~ submit_name),
             reviewed = case_when(case %in% review_set ~ as.logical(FALSE),
                                  TRUE ~ TRUE),
             bety_name = NA)

    idx <- which(is.na(sp$bety_name) & sp$reviewed)
    sp$bety_name[idx] <- sp$proposed_bety_name[idx]

    print(sprintf("Cases (%s) must be reviewed by hand. Update and then rerun final_names().", paste(case_accept_set, collapse = ", ")))

    return(sp)
  }

  if(p2){
    if(!all(sp$reviewed)){
      print(sprintf("Must complete all reviews"))
      return(sp)
    }
    if(all(sp$reviewed)){
      idx <- which(is.na(sp$bety_name) & sp$reviewed)
      sp$bety_name[idx] <- sp$proposed_bety_name[idx]

      final <- sp %>% mutate(bety_name = str_trim(bety_name)) %>%
        mutate(scientificname = bety_name) %>%
        mutate(test = bety_name) %>%
        separate(test, c("genus", "species"), sep = " ", extra = "drop") %>%
        mutate(bety_id = as.numeric(NA), in_PFT = as.logical(FALSE)) %>%
        select(one_of("submit_name", "genus", "species", "scientificname", "uri", "bety_id", "in_PFT"))

      for(i in seq_along(final$scientificname)){

        bn <- tolower(final$scientificname[i])
        check <- tbl(bety, "species") %>%
          filter(tolower(scientificname) == bn) %>% collect()

        if(nrow(check) == 1){
          final$bety_id[i] <- check$id
          print(sprintf("%i | %s already in as %.0f", i, final$scientificname[i], final$bety_id[i]))
        }else if(nrow(check) == 0){

          insert.species.query <- sprintf("INSERT INTO species (genus, species, scientificname, notes, created_at, updated_at) VALUES('%s', '%s', '%s', '%s', NOW(), NOW()) RETURNING id;",
                                          final$genus[i], final$species[i], final$scientificname[i], final$uri[i])
          paste(insert.species.query)
          species_id <- db.query(insert.species.query, bety$con)
          species_id <- species_id$id
          final$bety_id[i] <- species_id
          print(sprintf("%i | %s INSERTED in as %.0f", i, final$scientificname[i], final$bety_id[i]))

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
            print(sprintf("%i | %s added to PFT", i, final$scientificname[i]))
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

    }
    return(final)
  }

}
