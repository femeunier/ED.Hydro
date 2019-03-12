#' Check if a prior is already in the database and enter it if it is not
#'
#' @param bety BETY connection
#' @param variable_id.in BETY variable id
#' @param phylogeny.in BETY phylogeny
#' @param dist.name.in BETY distribution
#' @param parama.in
#' @param paramb.in
#' @param paramc.in
#' @return BETY prior id
#' @export

prior_input <- function(bety, variable_id.in, phylogeny.in,
                        dist.name.in, parama.in = NA, paramb.in = NA, paramc.in = NA){

  # I need to write this function so it's better generalized.
  # The point is that the distribution names need to be readable by BUGS/JAGS
  # str_detect(dist.name , regex("dist" , ignore_case = T))

  if(str_detect(dist.name.in , regex("^norm" , ignore_case = T))){
    distn.in  <- "norm"
  }else if(str_detect(dist.name.in , regex("log\\s*norm", ignore_case = T)) |
           str_detect(dist.name.in , regex("lnorm", ignore_case = T))){
    distn.in  <- "lnorm"
  }else if(str_detect(dist.name.in , regex("chisq" , ignore_case = T))){
    distn.in <- "chisq"
  }else if(str_detect(dist.name.in , regex("beta" , ignore_case = T))){
    distn.in  <- "beta"
  }else if(str_detect(dist.name.in , regex("pois" , ignore_case = T))){
    distn.in  <- "pois"
  }else if(str_detect(dist.name.in , regex("exp" , ignore_case = T))){
    distn.in  <- "exp"
  }else if(str_detect(dist.name.in , regex("gamma" , ignore_case = T))){
    distn.in  <- "gamma"
  }else if(str_detect(dist.name.in , regex("weibull" , ignore_case = T))){
    distn.in  <- "weibull"
  }else if(str_detect(dist.name.in , regex("unif" , ignore_case = T))){
    distn.in  <- "unif"
  }

  check <- tbl(bety, "priors") %>%
    filter(variable_id == variable_id.in) %>%
    filter(phylogeny == phylogeny.in) %>%
    filter(distn == distn.in) %>% collect()

  if(dim(check)[1] == 0){

    if(is.na(paramc.in) & is.na(paramb.in)){
      insert.query <- sprintf("INSERT INTO priors(variable_id, phylogeny, distn, parama, created_at, updated_at) VALUES(%i, '%s', '%s', %f, NOW(), NOW()) RETURNING id;",
                              variable_id.in, phylogeny.in, distn.in, parama.in)
    }else if(is.na(paramc.in)){
      insert.query <- sprintf("INSERT INTO priors(variable_id, phylogeny, distn, parama, paramb, created_at, updated_at) VALUES(%i, '%s', '%s', %f, %f, NOW(), NOW()) RETURNING id;",
                              variable_id.in, phylogeny.in, distn.in, parama.in, paramb.in)
    }else{
      insert.query <- sprintf("INSERT INTO priors(variable_id, phylogeny, distn, parama, paramb, paramc, created_at, updated_at) VALUES(%i, '%s', '%s', %f, %f, NOW(), NOW()) RETURNING id;",
                              variable_id.in, phylogeny.in, distn.in, parama.in, paramb.in, paramc.in)
    }

    prior_id <- db.query(insert.query, bety$con)
    prior_id$id
    var_name <- tbl(bety, "variables") %>% filter(id == variable_id.in) %>% pull(name)
    message <- sprintf("%s entered in BETY with id = %i", var_name , prior_id$id)

  }else{
    prior_id <- check$id

    if(is.na(paramc.in) & is.na(paramb.in)){
      message <- ifelse(near(check$parama, parama.in,tol = 1e-4),
                        sprintf("Entry with id = %i already exists. Parameters look the same. Old: %s(%.4f) New: %s(%.4f)",
                                prior_id, check$distn, check$parama, distn.in, parama.in),
                        sprintf("Entry with id = %i already exists. Parameters look different. Old: %s(%.4f) New: %s(%.4f)",
                                prior_id, check$distn, check$parama, distn.in, parama.in))
    }else if(is.na(paramc.in)){
      message <- ifelse(near(check$parama, parama.in,tol = 1e-4) & near(check$paramb, paramb.in),
                        sprintf("Entry with id = %i already exists. Parameters look the same. Old: %s(%.4f, %.4f) New: %s(%.4f, %.4f)",
                                prior_id, check$distn, check$parama, check$paramb, distn.in, parama.in, paramb.in),
                        sprintf("Entry with id = %i already exists. Parameters look different. Old: %s(%.4f, %.4f) New: %s(%.4f, %.4f)",
                                prior_id, check$distn, check$parama, check$paramb, distn.in, parama.in, paramb.in))
    }else{
      message <- ifelse(near(check$parama, parama.in,tol = 1e-4) & near(check$paramb, paramb.in),
                        sprintf("Entry with id = %i already exists. Parameters look the same. Old: %s(%.4f, %.4f, %.4f) New: %s(%.4f, %.4f, %.4f)",
                                prior_id, check$distn, check$parama, check$paramb, check$paramc, distn.in, parama.in, paramb.in, paramc.in),
                        sprintf("Entry with id = %i already exists. Parameters look different. Old: %s(%.4f, %.4f, %.4f) New: %s(%.4f, %.4f, %.4f)",
                                prior_id, check$distn, check$parama, check$paramb, check$paramc, distn.in, parama.in, paramb.in, paramc.in))
    }
  }
  print(message)
  return(prior_id)
}
