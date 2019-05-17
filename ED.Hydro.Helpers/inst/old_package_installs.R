# Additional installs that are necessary if R is less that 3.5

devtools::install_version(package = "SHELF", version = "1.3.0",
                          repos = "http://cran.us.r-project.org")

devtools::install_github("tidyverse/googlesheets4")
