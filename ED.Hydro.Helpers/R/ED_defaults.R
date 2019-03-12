#' Pull values from a given ED history.xml file
#' Assuming it is a history file from a defaults run
#'
#' @param xml path to default history.xml
#' @param var variable of interest
#' @return value
#' @export

get_ED_default <- function(xml, var){
  history_xml <- XML::xmlParse(xml)
  history_list <- XML::xmlToList(history_xml)
  out <- history_list %>% .[["pft"]] %>% .[[var]] %>% str_replace("/n", "") %>% str_trim() %>% as.numeric()
  return(out)
}
