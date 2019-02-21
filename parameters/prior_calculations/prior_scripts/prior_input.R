prior_input <- function(bety, myfit, variable_id.in, phylogeny.in, distn.in){
  # I don't like hardcoding this, but I don't trust the function to return
  # parameters in the same order that they should be entered in to the database

  if(distn.in == "Normal"){
    # pnorm(xval,myfit$Normal$mean,myfit$Normal$sd)
    distn.in  <- "norm"
    parama.in <- myfit$Normal$mean
    paramb.in <- myfit$Normal$sd
    paramc.in <- NA
  }else if(distn.in == "Gamma"){
    # pgamma(xval,myfit$Gamma$shape,myfit$Gamma$rate)
    distn.in  <- "gamma"
    parama.in <- myfit$Gamma$shape
    paramb.in <- myfit$Gamma$rate
    paramc.in <- NA
  }else if(distn.in == "Log normal"){
    # lines(xval,plnorm(xval,myfit$Log.normal$mean.log.X,myfit$Log.normal$sd.log.X),col=4,lwd=lwd[4])
    distn.in  <- "lnorm"
    parama.in <- myfit$Log.normal$mean.log.X
    paramb.in <- myfit$Log.normal$sd.log.X
    paramc.in <- NA
  }else if(distn.in == "Beta"){
    # pbeta(xval,myfit$Beta$shape1,myfit$Beta$shape2)
    distn.in  <- "beta"
    parama.in <- myfit$Beta$shape1
    paramb.in <- myfit$Beta$shape2
    paramc.in <- NA
  }
  # }else if(distn.in == "Student.t"){
  #   distn.in  <- "t"
  #   parama.in <- myfit$Student.t$location
  #   paramb.in <- myfit$Student.t$scale
  #   paramc.in <- myfit$Student.t$df
  # }else if(distn.in == "Log.Student.t"){
  #   distn.in  <- "lt"
  #   parama.in <- myfit$Log.Student.t$location.log.X
  #   paramb.in <- myfit$Log.Student.t$scale.log.X
  #   paramc.in <- myfit$Log.Student.t$df.log.X
  # }

  check <- tbl(bety, "priors") %>%
    filter(variable_id == variable_id.in) %>%
    filter(phylogeny == phylogeny.in) %>%
    filter(distn == distn.in) %>% collect()

  if(dim(check)[1] == 0){

    if(!is.na(paramc.in)){
      insert.query <- sprintf("INSERT INTO priors(variable_id, phylogeny, distn, parama, paramb, paramc, created_at, updated_at) VALUES(%i, '%s', '%s', %f, %f, NOW(), NOW()) RETURNING id;",
                              variable_id.in, phylogeny.in, distn.in, parama.in, paramb.in, paramc.in)
    }else{
      insert.query <- sprintf("INSERT INTO priors(variable_id, phylogeny, distn, parama, paramb, created_at, updated_at) VALUES(%i, '%s', '%s', %f, %f, NOW(), NOW()) RETURNING id;",
                              variable_id.in, phylogeny.in, distn.in, parama.in, paramb.in)
    }
    prior_id <- db.query(insert.query, bety$con)
    prior_id$id
    message <- sprintf("%s entered in BETY with id = %i", tbl(bety, "variables") %>% filter(id == variable_id.in) %>% pull(name), prior_id)
  }else{
    prior_id <- check$id

    message <- ifelse(near(check$parama, parama.in) & near(check$paramb, paramb.in),
                      sprintf("Entry with id = %i already exists with the same parameters.", prior_id),
                      sprintf("Entry with id = %i already exists with different parameters. Old: %s(%.4f, %.4f) New: %s(%.4f, %.4f)", prior_id, check$distn, check$parama, check$paramb, distn.in, parama.in, paramb.in))
  }
  print(message)
  return(prior_id)
}
