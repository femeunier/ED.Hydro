# library(PEcAn.all)
library(PEcAn.DB)

################################################################################
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

################################################################################
ID2name <- function(FRED_in, id){
  return(names(FRED_in)[which(FRED_in[3,] == id)])
}
name2ID <- function(FRED_in, name){
  return(FRED_in[3,name])
}

################################################################################
# bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")
#
# lat = 42.5314674
# lon = -72.189941
# #
# interval = .1
################################################################################
nearby_sites <- function(lat, lon, interval, con){
  bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

  lat_high <- lat + interval
  lat_low  <- lat - interval
  lon_high <- lon + interval
  lon_low  <- lon - interval

  out <- tbl(bety, dbplyr::sql("SELECT id, sitename, ST_Y(ST_Centroid(geometry)) AS lat, ST_X(ST_Centroid(geometry)) AS lon FROM sites")) %>%
    filter(lat >= lat_low & lat <= lat_high) %>%
    filter(lon >= lon_low & lon <= lon_high) %>% collect()

  return(out)
}

################################################################################

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

################################################################################

