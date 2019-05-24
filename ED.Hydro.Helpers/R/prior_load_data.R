#' Load all the data that is used to calculate priors
#'
#' @param download option to re-download the data from google drive\
#' @param subset
#' @return priors dataframe
#' @export

prior_load_data <- function(download = FALSE, subset = TRUE){

  data_dir <- "/fs/data3/ecowdery/ED.Hydro/parameters/prior_calculations/prior_data"

  ED_file <- file.path(data_dir, "ED.csv")
  Christoffersen_file <- file.path(data_dir, "FATES-Hydraulics_Pecan_Priors.xlsx")
  Christoffersen2ED_file <- file.path(data_dir, "Christoffersen2ED.csv")

  if(download){
    googledrive::drive_auth(reset=TRUE, use_oob = TRUE)
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

  priors <- full_join(Christoffersen, Christoffersen2ED, by = c("Christoffersen_name", "Tissue")) %>%
    full_join(., ED, by = "ED_name")
  names(priors) <- str_replace_all(names(priors) ," ",".")
  names(priors)[names(priors) == "low0.25"] <- "low.25" # Fixing a typo
  priors <- priors %>% mutate(C_name = paste(Christoffersen_name, Tissue, sep = "_"))

  if(subset){
    ED_variables <- c(
      "leaf_elastic_mod", "wood_elastic_mod",
      "leaf_psi_osmotic", "wood_psi_osmotic",
      "rwc_tlp_wood",
      "leaf_density",
      "leaf_water_sat",   "wood_water_sat",
      "leaf_psi_tlp",     "wood_psi_tlp",
      "leaf_water_cap",   "wood_water_cap",
      "leaf_psi_min",     "wood_psi_min",
      "wood_Kmax",
      "wood_psi50",
      "wood_Kexp"
    )
    C_variables <- c("avuln_node_stem")

    priors <- priors %>% filter(ED_name %in% ED_variables | C_name %in% C_variables)
    # left_join(data.frame(variables = variables, stringsAsFactors = FALSE),
    #           priors,
    #           by = c("variables" = "ED_name"))

    stats <- c("theor.min",
               "low.025",
               "low.25",
               "mean",
               "upp.75",
               "upp.975",
               "theor.max")

    priors <- priors %>% dplyr::select(one_of(
      c("ED_name",
        "Christoffersen_name",
        "BETY_variable_id",
        "BETY_prior_id",
        "ED_units",
        "Christoffersen_units",
        stats)
    ))

    #####################################################################
    # Add in prior data for variables that are blank
    need_priors <- priors %>%  filter_at(vars(stats), all_vars(is.na(.))) %>% pull(ED_name)
    print(need_priors)

    # Leaf_density
    priors[which(priors$ED_name == "leaf_density"),
           c("theor.min", "theor.max")] <- c(1e-7, 2000)

    # Leaf_psi_tlp
    priors[which(priors$ED_name == "leaf_psi_tlp"),
           c("theor.min", "theor.max")] <- c(-6, -0.5)

    # Leaf_psi_min
    priors[which(priors$ED_name == "leaf_psi_min"),
           c("theor.min", "theor.max")] <- c(-700, -0.1)

    # Leaf_water_cap
    priors[which(priors$ED_name == "leaf_water_cap"),
           c("theor.min", "theor.max")] <- c(1e-7, NA)

    # Wood_psi_tlp
    priors[which(priors$ED_name == "wood_psi_tlp"),
           c("theor.min", "theor.max")] <- c(NA, -0.1)

    # rwc_tlp_wood
    priors[which(priors$ED_name == "rwc_tlp_wood"),
           c("theor.min", "theor.max")]

    # Wood_psi_min
    priors[which(priors$ED_name == "wood_psi_min"),
           c("theor.min", "theor.max")] <- c(NA, -0.1)

    # Wood_water_cap
    priors[which(priors$ED_name == "wood_water_cap"),
           c("theor.min", "theor.max")] <- c(1e-7, NA)

    ###################################################
    # Reverse the values for the negative priors

    neg <- c(
      "leaf_psi_osmotic",
      "leaf_psi_tlp",
      "leaf_psi_min",
      "wood_psi_osmotic",
      "wood_psi_tlp",
      "wood_psi_min",
      "wood_psi50"
    )
    for(n in neg){
       new <- -rev(priors[which(priors$ED_name == n), stats])
       priors[which(priors$ED_name == n), stats] <- new
    }
  }
  return(priors)
}









