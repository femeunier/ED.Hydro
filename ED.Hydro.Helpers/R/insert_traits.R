#' Insert traits in to the database ... we may already have this function. Who knows.
#'
#' @param insert_dat dataframe
#' @param bety
#' @return insert_dat dataframe
#' @export

insert_traits <- function(insert_dat, check_only = FALSE, bety){

  for(i in seq_along(insert_dat$value)){

    check <- tbl(bety, "traits") %>%
      filter(site_id == insert_dat$site_id[i]) %>%
      filter(specie_id == insert_dat$species_id[i]) %>%
      filter(citation_id == insert_dat$citation_id[i]) %>%
      filter(treatment_id == insert_dat$treatment[i]) %>%
      filter(abs(mean - insert_dat$value[i]) < 10^-6)  %>%
      filter(variable_id == insert_dat$variable_id[i]) %>%
      collect

    if(dim(check)[1] == 0 & !check_only){

      insert.query <- sprintf("INSERT INTO traits (site_id, specie_id, citation_id, treatment_id, variable_id, mean, user_id, access_level, created_at, updated_at) VALUES(%.0f, %.0f, %.0f, %.0f, %.0f, %f, 1000000003, 4, NOW(), NOW()) RETURNING id;",
                              insert_dat$site_id[i], insert_dat$species_id[i], insert_dat$citation_id[i],
                              insert_dat$treatment[i], insert_dat$variable_id[i], insert_dat$value[i])
      trait_id <- db.query(insert.query, bety$con)
      insert_dat$trait_id[i] <- trait_id$id

      print(paste0(i, " |Inserted entry: ", insert_dat$trait_id[i]))
    }else if(dim(check)[1] == 0 & check_only){
      print(paste0(i, " |Entry does not exist but won't be entered"))
    }else if(dim(check)[1] == 1){
      print(paste0(i, " |Entry already exists: ", check$id))
      insert_dat$trait_id[i] <- check$id
    }else{
      print("Error, more than one entry returned")
    }
  }
  return(insert_dat)
}
