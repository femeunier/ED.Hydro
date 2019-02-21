bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")

RUNS <- list()

runs <- rbind(
  c("/fs/data2/output/PEcAn_1000010046", 1000020406, "AGU_2019: H20_PLANT_LIM = 2"),
  c("/fs/data2/output/PEcAn_1000010046", 1000020407, "AGU_2019: H20_PLANT_LIM = 2"),
  c("/fs/data2/output/PEcAn_1000010047", 1000020408, "AGU_2019: H20_PLANT_LIM = 2"),
  c("/fs/data2/output/PEcAn_1000010047", 1000020409, "AGU_2019: H20_PLANT_LIM = 2"),
  c("/fs/data2/output/PEcAn_1000010049", 1000020420, "AGU_2019: H20_PLANT_LIM = 2"),
  c("/fs/data2/output/PEcAn_1000010049", 1000020421, "AGU_2019: H20_PLANT_LIM = 2"),
  c("/fs/data2/output/PEcAn_1000010050", 1000020422, "AGU_2019: H20_PLANT_LIM = 2"),
  c("/fs/data2/output/PEcAn_1000010050", 1000020423, "AGU_2019: H20_PLANT_LIM = 2"),
  c("/fs/data2/output/PEcAn_1000010230", 1000020934, "New test with fine roots added, H20_PLANT_LIM = 2"),
  c("/fs/data2/output/PEcAn_1000010230", 1000020935, "New test with fine roots added, H20_PLANT_LIM = 2")
) %>% as.data.frame(stringsAsFactors = FALSE)

names(runs) <- c("outfolder", "ensemble_id", "notes")

# i = 1

for(i in 1:nrow(runs)){
  
  pecan.xml.path <- file.path(runs[i, "outfolder"],"pecan.xml")
  settings <- PEcAn.settings::read.settings(pecan.xml.path) 
  
  args <- list()
  
  args$model_id <- settings$model$id
  args$ED_version <- tbl(bety, "models") %>% filter(id == args$model_id) %>% pull(revision)
  args$workflow_id <- ifelse(!is.null(settings$workflow$id), settings$workflow$id, NA)
  args$ensemble_id <- runs[i, "ensemble_id"]
  args$run_type <- tbl(bety, "ensembles") %>% filter(id == args$ensemble_id) %>% pull(runtype)
  args$met_id <- settings$run$inputs$met$id
  args$conditions <- ifelse(
    tbl(bety, "inputs") %>% filter(id == args$met_id) %>% pull(name) %>% 
      strsplit("_") %>% unlist %>% tail(1) == "DROUGHT", "dry", "wet")
  
  args$pft <- settings$pfts$pft$name[1]
  args$ed2_pft_number <- ifelse(!is.null(settings$pfts$pft$ed2_pft_number),settings$pfts$pft$ed2_pft_number, NA)
  
  pft_constants <- settings$pfts$pft$constants 
  
  for(j in seq_along(pft_constants)){
    args[paste0("hardcode_",pft_constants[j] %>% names())] <- pft_constants[[j]]
  }
  
  ma.file <- file.path(runs[i, "outfolder"], "pft", args$pft, "madata.Rdata")
  load(ma.file)
  for(k in seq_along(madata)){
    args[paste0("MA_", names(madata)[k])] <- 1
  }
  remove(madata)
  
  if(args$run_type == "sensitivity analysis"){
    sa.file <- file.path(runs[i, "outfolder"], paste0("sensitivity.samples.",runs[i, "ensemble_id"],".Rdata"))
    load(sa.file)
    for(l in seq_along(sa.samples[[args$pft]])){
      args[paste0("SA_",names(sa.samples[[args$pft]])[l])] <- 
        sprintf("%.2f",sa.samples[[args$pft]][[l]]) %>% paste(., collapse = ",")
    }
    remove(list = c("sa.run.ids", "sa.ensemble.id", "sa.samples", "pft.names", "trait.names"))
  }
  if(args$run_type == "ensemble"){
    es.file <- file.path(runs[i, "outfolder"], paste0("ensemble.samples.",runs[i, "ensemble_id"],".Rdata"))
    load(es.file)
    for(m in seq_along(ens.samples[[args$pft]])){
      args[paste0("ENS_",names(ens.samples[[args$pft]])[m])] <- 
        sprintf("%.2f",ens.samples[[args$pft]][[m]]) %>% paste(., collapse = ",")
    }
    remove(list = c("ens.run.ids", "ens.ensemble.id", "ens.samples", "pft.names", "trait.names"))
  }
  args$notes <- runs[i, "notes"]
  RUNS[[i]] <- data.frame(args, stringsAsFactors = FALSE)
}

RUNS_DF <- bind_rows(RUNS)
View(RUNS_DF)

write.csv(RUNS_DF, "/fs/data3/ecowdery/ED_Tropics/RUNS_summary.csv")
