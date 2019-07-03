#' Just an easy way to keep track of which traits we use in plots
#'
#' @export


which_keep_traits <- function(){
  # Setup grouping of parameters
  traits_hydro <- c("Water Conductance",
                    "Leaf water cap",
                    "Wood water cap",
                    "Kmax",
                    "Kexp",
                    "p50",
                    "leaf_psi_tlp",
                    "Root depth allom. int.",
                    "Root depth allom. slope")
  traits_photo <- c("Vcmax",
                    "Stomatal Slope",
                    "Quantum Efficiency")
  traits_alloc <- c("Wood Density",
                    "Specific Leaf Area",
                    "Specific Root Area",
                    "Frac Biomass Aboveground",
                    "Fine Root Allocation")
  traits_radtn <- c("Leaf orientation",
                    "Leaf NIR reflectance",
                    "Leaf NIR transmittance",
                    "Leaf VIS reflectance",
                    "Leaf VIS transmittance")
  traits_respr <- c("Growth Respiration",
                    "Leaf Respiration Rate",
                    "Veg. Resp. Q10",
                    "Leaf Turnover Rate",
                    "Root Turnover Rate")

  # Make keep.traits
  keep.traits <- data.frame(
    new.labels = c(
      traits_hydro,
      traits_photo,
      traits_alloc,
      traits_radtn,
      traits_respr
    ),

    trait.type = c(
      rep("Hydraulics",length(traits_hydro)),
      rep("Photo. + S.C.",length(traits_photo)),
      rep("Alloc. + Allom.",length(traits_alloc)),
      rep("Radiation",length(traits_radtn)),
      rep("Resp. + Turnovr",length(traits_respr))
    )
  )
}


