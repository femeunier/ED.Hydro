bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

variables <- c(
  "SLA",
  "wood_density",
  "leaf_elastic_mod", "wood_elastic_mod",
  "leaf_psi_osmotic", "wood_psi_osmotic",
  "rwc_tlp_wood",
  "leaf_water_sat", "wood_water_sat",
  "leaf_water_cap", "wood_water_cap",
  "leaf_psi_min", "wood_psi_min",
  "leaf_psi_tlp", "wood_psi_tlp",
  "wood_Kmax",
  "wood_psi50",
  "wood_Kexp",
  "fineroot2leaf"
)

pftid <- 1000000131

pft_info <- tbl(bety, "pfts") %>% filter(id == pftid) %>% collect()

pft_prior_vars <- tbl(bety, "pfts_priors") %>% filter(pft_id == pftid) %>% inner_join(tbl(bety, "priors") %>% rename(prior_id = id),., by= "prior_id") %>% inner_join(tbl(bety, "variables") %>% rename(variable_id = id),., by = "variable_id" ) %>% collect() %>% pull(name)

trait.names <- intersect(variables, pft_prior_vars)

###############


pft <- list()
pft$name <- pft_info$name
pft$outdir <- file.path("/fs/data3/ecowdery/ED.Hydro/parameters/constraints/", pft$name)

pft = pft
modeltype = tbl(bety, "modeltypes") %>% filter(id == pft_info$modeltype_id) %>% pull(name)
dbfiles = "/fs/data1/pecan.data/dbfiles"
dbcon = bety$con
trait.names = trait.names
forceupdate = FALSE

get.trait.data.pft(pft, modeltype, dbfiles, dbcon, trait.names, forceupdate = TRUE)

load(file.path(pft$outdir, "trait.data.Rdata"))

