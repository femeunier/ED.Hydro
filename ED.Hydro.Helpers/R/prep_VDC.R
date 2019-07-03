#' Perform UA and SA
#'
#' @param wf_id
#' @param var
#' @param path_to_config
#' @param REDO
#' @export

prep_VDC <- function(wf_id, var, path_to_config = NA, REDO = FALSE){

  if(is.na(path_to_config)){ # assume we're on test-pecan and auto generate the file path
    path_to_config <- sprintf("/fs/data2/output/PEcAn_%i", wf_id)
  }

  original_xml <- file.path(path_to_config, "pecan.CONFIGS.xml")
  new_xml <- file.path(path_to_config, "pecan.CONFIGS.NPP.xml")
  file.copy(original_xml, new_xml, overwrite = REDO)
  tx  <- readLines(new_xml)

  for(i in seq_along(var)){
    new_file <- paste0(path_to_config, "/pecan.CONFIGS.",var[i],".xml")

    check <- file.exists(new_file)
    if(REDO){check <- FALSE}

    if(!check){
      tx2  <- gsub(pattern = "NPP", replace = var[i], x = tx)
      writeLines(tx2, con=new_file)
      settings <- read.settings(new_file)
      runModule.get.results(settings)
      runModule.run.ensemble.analysis(settings, TRUE)
      runModule.run.sensitivity.analysis(settings)
    }
  }
}
