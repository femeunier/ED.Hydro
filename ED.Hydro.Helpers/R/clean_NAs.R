#'
#'
#' @param x
#' @param traits
#' @return x
#' @export

clean_NAs <- function(x, traits){
  # Get rid of completely empty data rows
  x <- x %>% filter_at(vars(traits), any_vars(!is.na(.)))

  # Check if at this point some of the variables have no data in the columns
  test_empty <- sapply(x, function(x) all(is.na(x)))
  which(test_empty) %>% names

  for(t in traits){
    if(all(is.na(x[,t]))){
      x[,t] <- NULL
    }
  }
  return(x)
}
