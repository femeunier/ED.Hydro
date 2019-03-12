#' Add quartiles from sample data to the priors table
#'
#' @param prior_in
#' @param samp
#' @return priors dataframe
#' @export

prior_add_samp_data <- function(prior_in, samp){

  prior_in[, "low.025"] <-  quantile(samp, c(.025), na.rm = TRUE)
  prior_in[, "low.25"]  =  quantile(samp, c(.25), na.rm = TRUE)
  prior_in[, "mean"]    =  quantile(samp, c(.5), na.rm = TRUE)
  prior_in[, "upp.75"]  =  quantile(samp, c(.75), na.rm = TRUE)
  prior_in[, "upp.975"] =  quantile(samp, c(.975), na.rm = TRUE)

  # Check
  for(q in c("low.025","low.25")) {
    if(!is.na(prior_in[,"theor.min"])){
      if(prior_in[,q] < prior_in[,"theor.min"]){
        prior_in[,q] <- NA
      }
    }
  }
  for(p in c("upp.75","upp.975")) {
    if(!is.na(prior_in[,"theor.max"])){
      if(prior_in[,"theor.max"] < prior_in[,p]){
        prior_in[,p] <- NA
      }
    }
  }
  return(prior_in)
}

