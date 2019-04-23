#' Find nearby sites in BETY
#'
#' @param lat
#' @param lon
#' @param interval
#' @param bety
#' @param config.php
#' @return dataframe
#' @export

nearby_sites <- function(lat, lon, interval, bety = NA, config.php = "/fs/data3/ecowdery/pecan/web/config.php"){

  if(all(is.na(bety))){
    bety <- betyConnect(config.php)

  }
  lat_high <- lat + interval
  lat_low  <- lat - interval
  lon_high <- lon + interval
  lon_low  <- lon - interval

  out <-
    tbl(bety, dbplyr::sql("SELECT id, sitename, ST_Y(ST_Centroid(geometry)) AS lat, ST_X(ST_Centroid(geometry)) AS lon FROM sites")) %>%
    filter(lat >= lat_low & lat <= lat_high) %>%
    filter(lon >= lon_low & lon <= lon_high) %>% collect()

  return(out)
}
