#' @param sp
#' @param species_all
#' @param PFT_species
#' @param bety
#' @param out_file
#' @param overwrite
#' @return sp
#' @export

taxanomic_resolution <- function(sp = NA, species_all = NA, PFT_species, bety,
                                 out_file = NA, overwrite = FALSE){

  if(!all(is.na(sp))){species_all <- NA}
  if(!is.na(species_all)){
    test <- species_all %in% tolower(PFT_species$scientificname)
    df <- data.frame( species_all = species_all, in_dat1 = test, stringsAsFactors = FALSE )
    df <- df %>% arrange(-in_dat1, species_all)
    species_all <- df$species_all # Wow that was probably the WORST way to do that.

    sp <- data.frame(submit_name = species_all,
                     submit_bety_id = NA,
                     submit_in_PFT = FALSE,

                     accept_name = "",
                     accept_bety_id = NA,
                     accept_in_PFT = FALSE,

                     sourceid = NA,
                     score = NA,
                     authority = NA,
                     uri = NA,

                     case = 0,

                     stringsAsFactors = FALSE)
    if(!is.na(out_file) & overwrite){ write.csv(x = sp, file = out_file) }
  }

  i_range <- seq_along(sp$submit_name)

  for(i in i_range){

    if(sp$case[i] != 1 ){
      Sys.sleep(.4)
      print(paste0(i, ": ", sp$submit_name[i]))

      # This is a bit redundant but I find it helpful for the debugging
      submit_bety <- FALSE
      submit_PFT  <- FALSE
      accept_bety <- FALSE
      accept_PFT  <- FALSE
      accept_name_exists <- FALSE

      # First check if the name is in BETY (without any changes/resolution)

      sub_name <- sp$submit_name[i]

      find_submit <- tbl(bety, "species") %>%
        filter(tolower(scientificname) == sub_name) %>%
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
      test_tnrs <- try(taxize::tnrs(query = sp$submit_name[i], source = "iPlant_TNRS"))
      if(!class(test_tnrs) == "try-error"){
        if("acceptedname" %in% names(test_tnrs)){
          acc_name <- test_tnrs$acceptedname
          if(acc_name != ""){
            sp$accept_name[i] = acc_name
          }else if("matchedname" %in% names(test_tnrs)){
            sp$accept_name[i] = test_tnrs$matchedname
          }else{
            sp$accept_name[i] = ""
          }
        }
        if("sourceid" %in% names(test_tnrs))     sp$sourceid[i] = test_tnrs$sourceid
        if("score" %in% names(test_tnrs))        sp$score[i] = test_tnrs$score
        if("authority" %in% names(test_tnrs))    sp$authority[i] =  test_tnrs$authority
        if("uri" %in% names(test_tnrs))          sp$uri[i] = test_tnrs$uri
      }

      # If the search retuns an accepted name, check to see if it's in the database and PFT
      accept_name_exists <- !sp$accept_name[i] == "" & !is.na(sp$accept_name[i])
      if(accept_name_exists){

        # If the names are the same, then find_accept is the same as find_submit
        # otherwise, look it up in bety
        if(sp$submit_name[i] == tolower(sp$accept_name[i])){
          find_accept <- find_submit
        }else{
          acc_name <- tolower(sp$accept_name[i])
          find_accept <- tbl(bety, "species") %>%
            filter(tolower(scientificname) == acc_name) %>%
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

      if(!is.na(out_file)){ write.csv(x = sp, file = out_file) }

    } else {print(paste0(i, ": ", sp$submit_name[i], " already case 1."))}

  } # End loop

  if(!is.na(out_file)){ write.csv(x = sp, file = out_file) }

  return(sp)

}
