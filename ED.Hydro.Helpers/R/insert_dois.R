#' @param all_doi
#' @return bibs
#' @export

insert_dois <- function(all_doi){

  bibs <- list()

  for(i in seq_along(all_doi)){
    print(paste0(i," | ", all_doi[i]))

    bibs[[i]] = rcrossref::cr_cn(dois = all_doi[i], format = "bibentry")

    # # old option
    # check_doi <- tbl(bety, "citations") %>% dplyr::filter(doi == bibs[[i]]$doi) %>% collect()
    # # Error message: Error: Can only index with strings

    #new option
    bib_doi <- bibs[[i]]$doi
    check_doi <- tbl(bety, "citations") %>% filter(doi == bib_doi) %>% collect()


    queries <- data.frame(include = c("user_id", "created_at", "updated_at"),
                          values = c("1000000003", "NOW()", "NOW()"),
                          stringsAsFactors = FALSE)

    if(nrow(check_doi) == 0){
      if(!is.null(bibs[[i]]$author)){
        new <- c("author", paste0("'", paste(bibs[[i]]$author, collapse = ", ")
                                  %>% clean_chars(), "'"))
        queries <- rbind.data.frame(queries, new)
      }
      if(!is.null(bibs[[i]]$year)){
        new <- c("year", paste0(as.numeric(bibs[[i]]$year)))
        queries <- rbind.data.frame(queries, new)
      }
      if(!is.null(bibs[[i]]$title)){
        new <- c("title", paste0("'", paste(bibs[[i]]$title, collapse = ", ")
                                 %>% clean_chars(), "'"))
        queries <- rbind.data.frame(queries, new)
      }
      if(!is.null(bibs[[i]]$journal)){
        new <- c("journal", paste0("'", paste(bibs[[i]]$journal, collapse = ", ")
                                   %>% clean_chars(), "'"))
        queries <- rbind.data.frame(queries, new)
      }
      if(!is.null(bibs[[i]]$volume)){
        new <- c("vol", paste0(as.numeric(bibs[[i]]$volume)))
        queries <- rbind.data.frame(queries, new)
      }
      if(!is.null(bibs[[i]]$pg)){
        new <- c("pg", paste0("'", bibs[[i]]$pages %>%
                                str_replace(pattern = "--",replacement = "-") %>%
                                str_squish, "'"))
        queries <- rbind.data.frame(queries, new)
      }
      if(!is.null(bibs[[i]]$url)){
        new <- c("url", paste0("'", bibs[[i]]$url %>% str_squish, "'"))
        queries <- rbind.data.frame(queries, new)
      }
      if(!is.null(bibs[[i]]$doi)){
        new <- c("doi", paste0("'", bibs[[i]]$doi %>% str_squish, "'"))
        queries <- rbind.data.frame(queries, new)
      }

      paste(queries$include, collapse = ", ")
      paste(queries$values, collapse = ", ")

      insert.query <- sprintf("INSERT INTO citations (%s) VALUES(%s) RETURNING id;",
                              paste(queries$include, collapse = ", "),
                              paste(queries$values, collapse = ", "))

      citation_id <- db.query(insert.query, bety$con)
      sprintf("Citation %10.0f added to BETY", citation_id)

    }else{
      citation_id <- check_doi$id
      print(sprintf("Citation %10.0f already in BETY", citation_id))
    }
    bibs[[i]]$citation_id <- citation_id
  }
  return(bibs)
}
