################################################################################
# Conversion things

conv <- 1/0.009804139432

1.81/101

1 / 1 / 1 / 0.009804139432

################################################################################

input_prior <- function(bety, myfit, variable_id.in, phylogeny.in, distn.in){
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



##############################################################################
## Determine parameters that can be used for fit

get_fit <- function(priors, i, plot = FALSE){
  
  quants <- tibble(name = c("low.025", "upp.975", "low.25", "upp.75", "mean"), 
                   quant = c(.025, .975, .25, .75, .5), stringsAsFactors = FALSE)
  
  test <- priors[i,] %>% select_if(~ !any(is.na(.)))
  
  sub <- priors[i,] %>% 
    select_if(~ !any(is.na(.))) %>% 
    select_if(names(test) %in% quants$name) %>% 
    gather(key = name)
  # glimpse(sub)
  
  #If there aren't any parameters, skip the variable
  if(dim(sub)[2] == 0){ 
    print("skipping")
    next
  } 
  
  sub$value <- as.numeric(sub$value)
  sub2 <- merge(sub, quants) %>% na.omit()
  
  # if(priors[i,"Unit"] != priors[i,"Old_unit"]){
  #   sprintf("Converting units from %s to %s", priors[i,"Old_unit"], priors[i,"Unit"])
  #   sub2$value <- udunits2::ud.convert(sub2$value, priors[i,"Old_unit"], priors[i,"Unit"])
  # }
  # 
  
  
  args <- list(
    vals = sub2$value, 
    probs = sub2$quant
  )
  
  lower <- test %>% select_if(names(test) %in% c("theor.min"))
  lower <- ifelse(all(dim(lower) == c(1,1)), pull(lower), NA)
  upper <- test %>% select_if(names(test) %in% c("theor.max"))
  upper <- ifelse(all(dim(upper) == c(1,1)), pull(upper), NA)
  
  if(!is.na(lower)) args = append(args, list(lower = lower))
  if(!is.na(upper)) args = append(args, list(upper = upper))
  
  ##############################################################################
  ## Calculate fit and insert in to the database 
  ## Supported distributions: Normal, Beta, Log Normal, Beta
  ## Distributions we want: chisq, exp, f, unif, weibull
  
  myfit <- do.call(fitdist, args)
  if(plot){
    print("plotting")
    lwd = 5*min(myfit$ssq,na.rm = TRUE)/myfit$ssq
    xval = seq(min(sub2$value),max(sub2$value),length=1000)
    plot(sub2$value, sub2$quant, main = priors$ED_name[i])
    
    if(!is.na(myfit$Normal[1])){
      lines(xval,pnorm(xval,myfit$Normal$mean,myfit$Normal$sd),col=2,lwd=lwd[1])
    }
    if(!is.na(myfit$Gamma[1])){
      lines(xval,pgamma(xval,myfit$Gamma$shape,myfit$Gamma$rate),col=3,lwd=lwd[3])
    }
    if(!is.na(myfit$Log.normal[1])){
      lines(xval,plnorm(xval,myfit$Log.normal$mean.log.X,myfit$Log.normal$sd.log.X),col=4,lwd=lwd[4])
    }
    if(!is.na(myfit$Beta[1])){
      lines(xval,pbeta(xval,myfit$Beta$shape1,myfit$Beta$shape2),col=5,lwd=lwd[5])
    }
    # if(!is.na(myfit$Student.t[1])){
    #   lines(xval,pt(xval,myfit$Student.t$location,myfit$Student.t$scale,myfit$Student.t$df),col=6,lwd=lwd[6])
    # }
    # if(!is.na(myfit$Log.Student.t[1])){
    #   lines(xval,pt(xval,myfit$Log.Student.t$location.log.X,myfit$Log.Student.t$scale.log.X,myfit$Log.Student.t$df.log.X, log.p = TRUE),col=7,lwd=lwd[7])
    # }
    legend("topleft",legend=c("Normal","Gamma","Log.normal","Beta", "Student t", "Log Student t"),col=2:7,lwd=2)
  }
  
  return(myfit)
}

################################################################################

rdistn <- function(fit){
  
  if(fit$distn == "beta"){
    out <- rbeta(1000000, fit$parama, fit$paramb)
  }else if(fit$distn == "unif"){
    out <- runif(1000000, fit$parama, fit$paramb)
  }else if(fit$distn == "exp"){
    out <- rexp(1000000, fit$parama)
  }else if(fit$distn == "gamma"){
    out <- rgamma(1000000, fit$parama, fit$paramb)
  }else if(fit$distn == "norm"){
    out <- rnorm(1000000, fit$parama, fit$paramb)
  }else if(fit$distn == "weibull"){
    out <- rweibull(1000000, fit$parama, fit$paramb)
  }else if(fit$distn == "lnorm"){
    out <- rlnorm(1000000, fit$parama, fit$paramb)
  }else if(fit$distn == "pois"){
    out <- rpois(1000000, fit$parama)
  # }else if(fit$distn == "t"){
  #   out <- rt(1000000, fit$parama, fit$paramb, fit$paramc)
  # }else if(fit$distn == "lt"){
  #   out <- rt(1000000, fit$parama, fit$paramb, fit$paramc, log.p = TRUE)
  }else{
    out <- NA
  }
  return(out)
}

################################################################################

get_ED_default <- function(history_list, var){
  out <- history_list %>% .[["pft"]] %>% .[[var]] %>% str_replace("/n", "") %>% str_trim() %>% as.numeric()
  return(out)
}

################################################################################
library(XML)

PFT3_defaults_history_XML <- xmlParse("/fs/data3/ecowdery/ED_Tropics/parameters/pft3_defaults_history.xml")
PFT3_defaults_history <- xmlToList(PFT3_defaults_history_XML)