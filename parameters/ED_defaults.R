library(XML)

PFT3_defaults_history_XML <- xmlParse("/fs/data3/ecowdery/ED_Tropics/parameters/pft3_defaults_history.xml")
PFT3_defaults_history <- xmlToList(PFT3_defaults_history_XML)

################################################################################

get_ED_default <- function(history_list, var){
  out <- history_list %>% .[["pft"]] %>% .[[var]] %>% str_replace("/n", "") %>% str_trim() %>% as.numeric()
  return(out)
}
