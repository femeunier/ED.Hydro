#' Given distribution details, make random samples
#'
#' @param xval
#' @param dist_params
#' @return dataframe of distribution samples
#' @export

pdistn <- function(xval, dist_params){

  # I really wish I could automate this with match.fun

  out_list <- list(xval = xval)

  if("norm" %in% names(dist_params)){
    out_list[["norm"]] <- pnorm(xval,dist_params$norm[1],dist_params$norm[2])
  }
  if("lnorm" %in% names(dist_params)){
    out_list[["lnorm"]] <- plnorm(xval,dist_params$lnorm[1],dist_params$lnorm[2])
  }
  if("chisq" %in% names(dist_params)){
    out_list[["chisq"]] <- pchisq(xval,dist_params$chisq[1])
  }
  if("beta" %in% names(dist_params)){
    out_list[["beta"]] <- pbeta(xval,dist_params$beta[1],dist_params$beta[2])
  }
  # if("cauchy" %in% names(dist_params)){
  #   out_list[["cauchy"]] <- pcauchy(xval,dist_params$cauchy[1],dist_params$cauchy[2])
  # }
  if("pois" %in% names(dist_params)){
    out_list[["pois"]] <- ppois(xval,dist_params$pois[1],dist_params$pois[2])
  }
  # if("t" %in% names(dist_params)){
  #   out_list[["t"]] <- pt(xval,dist_params$t[1],dist_params$t[2])
  # }
  if("exp" %in% names(dist_params)){
    out_list[["exp"]] <- pexp(xval,dist_params$exp[1])
  }
  # if("f" %in% names(dist_params)){
  #   out_list[["f"]] <- pf(xval,dist_params$f[1], dist_params$f[2])
  # }
  if("gamma" %in% names(dist_params)){
    out_list[["gamma"]] <- pgamma(xval,dist_params$gamma[1],dist_params$gamma[2])
  }
  if("weibull" %in% names(dist_params)){
    out_list[["weibull"]] <- pweibull(xval,dist_params$weibull[1],dist_params$weibull[2])
  }
  if("unif" %in% names(dist_params)){
    out_list[["unif"]] <- punif(xval,dist_params$unif[1],dist_params$unif[2])
  }

  return(bind_cols(out_list))
}
