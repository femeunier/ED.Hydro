
#' @param x
#' @return x
#' @export

clean_chars <- function(x){
  x <- x %>%
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>%
    str_replace_all("[^[:alnum:][:blank:]+:,.&]", "") %>%
    str_squish

  if(nchar(x) > 255){
    x <- paste(str_sub(x, start = 1, end = 251), "...")
  }
  return(x)
}
