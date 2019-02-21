#' Get a prior fit, specifically from the priors dataframe
#'
#' @param priors dataframe
#' @param i row of dataframe to be used
#' @param plot option to plot the fits
#' @return fit object
#' @export

prior_get_fit <- function(priors, i, plot = FALSE){

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

  ###############################################
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
