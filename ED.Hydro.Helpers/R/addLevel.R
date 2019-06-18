#' Add a level to a list of factors
#'
#' @param x
#' @param newlevel=NULL
#' @export

addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}
