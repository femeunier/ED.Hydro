#' Load all the data that is used to calculate priors
#'
#' @param download option to re-download the data from google drive
#' @return priors dataframe
#' @export

prior_load_data <- function(download = FALSE){

  data_dir <- "/fs/data3/ecowdery/ED_Tropics/parameters/prior_calculations/prior_data"

  ED_file <- file.path(data_dir, "ED.csv")
  Christoffersen_file <- file.path(data_dir, "FATES-Hydraulics_Pecan_Priors.xlsx")
  Christoffersen2ED_file <- file.path(data_dir, "Christoffersen2ED.csv")

  if(download){
    # Download the table I made about ED parameters from Google
    ED_id <- googlesheets4::as_sheets_id("https://docs.google.com/spreadsheets/d/1gn_MBoq1Yvzx2QVPCaDyXbfpAv-EfeJoxdjzHzFwMVE/edit?usp=sharing")
    googledrive::drive_download(file = ED_id, ED_file, overwrite = TRUE)

    # Download the table I made linking ED parameters to Christoffersen data from Google
    Christoffersen2ED_id <- googlesheets4::as_sheets_id( "https://docs.google.com/spreadsheets/d/1voUxoLgOWVNrum8VgyLrKc4gC-rjp_ulrjrBZaL5MA8/edit?usp=sharing")
    googledrive::drive_download(file = Christoffersen2ED_id, Christoffersen2ED_file, overwrite = TRUE)

  }

  ED <- read.csv(ED_file, stringsAsFactors = FALSE) %>% dplyr::rename(ED_Equation = Equation)
  Christoffersen <- readxl::read_excel(Christoffersen_file) %>%
    dplyr::rename(Christoffersen_name = Parameter) %>%
    dplyr::rename(Christoffersen_Equation = Equation)
  Christoffersen2ED <- read.csv(Christoffersen2ED_file, stringsAsFactors = FALSE)

  priors <- full_join(Christoffersen, Christoffersen2ED) %>% full_join(., ED)
  names(priors) <- str_replace_all(names(priors) ," ",".")

  return(priors)

}









