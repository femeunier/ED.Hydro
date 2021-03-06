#' Given fit results from BETY, determine the distribution and sample
#'
#' @param fit results from query to BETY priors table
#' @return distribution sample
#' @export


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
