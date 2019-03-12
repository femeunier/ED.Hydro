#' Given fit results from BETY, determine the distribution and sample
#'
#' @param fit results from query to BETY priors table
#' @return distribution sample
#' @export


rdistn <- function(fit, n = 100000){

  if(fit$distn == "norm"){
    out <- rnorm(n, fit$parama, fit$paramb)
  }else if(fit$distn == "lnorm"){
    out <- rlnorm(n, fit$parama, fit$paramb)
  }else if(fit$distn == "chisq"){
    out <- rchisq(n, fit$parama)
  }else if(fit$distn == "beta"){
    out <- rbeta(n, fit$parama, fit$paramb)
  }else if(fit$distn == "pois"){
    out <- rpois(n, fit$parama)
  }else if(fit$distn == "exp"){
    out <- rexp(n, fit$parama)
  }else if(fit$distn == "gamma"){
    out <- rgamma(n, fit$parama, fit$paramb)
  }else if(fit$distn == "weibull"){
    out <- rweibull(n, fit$parama, fit$paramb)
  }else if(fit$distn == "unif"){
    out <- runif(n, fit$parama, fit$paramb)
  }else{
    out <- NA
  }
  return(out)
}







# }else if(fit$distn == "t"){
#   out <- rt(1000000, fit$parama, fit$paramb, fit$paramc)
# }else if(fit$distn == "lt"){
#   out <- rt(1000000, fit$parama, fit$paramb, fit$paramc, log.p = TRUE)