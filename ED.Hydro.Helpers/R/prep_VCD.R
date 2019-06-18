#' Perform UA and SA
#'
#' @param wf_id
#' @param var
#' @param REDO
#' @export

prep_VCD <- function(wf_id, var, REDO = FALSE){

  base_path <- sprintf("/fs/data2/output/PEcAn_%i", wf_id)
  original_xml <- file.path(base_path, "pecan.CONFIGS.xml")
  new_xml <- sprintf("/fs/data2/output/PEcAn_%i/pecan.CONFIGS.NPP.xml", wf_id)
  file.copy(original_xml, new_xml, overwrite = REDO)
  tx  <- readLines(new_xml)

  for(i in seq_along(var)){
    new_file <- paste0("/fs/data2/output/PEcAn_",wf_id,"/pecan.CONFIGS.",var[i],".xml")

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
