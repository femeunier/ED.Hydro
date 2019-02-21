
source("/fs/data3/ecowdery/Miscellaneous/read_write_args.R")

# Before running pull_veg_module

string <- print("getveg.id  = getveg.id[[i]], 
                                       dbparms    = dbparms,
                                       input_veg  = input, 
                                       pfts       = settings$pfts,
                                       outfolder  = outfolder, 
                                       n.ensemble = i,
                                       dir        = dir, 
                                       machine    = machine, 
                                       model      = model,
                                       start_date = start_date, 
                                       end_date   = end_date,
                                       new_site   = new.site,
                                       host       = host, 
                                       overwrite  = overwrite$putveg")

filename = "/fs/data3/ecowdery/ED_Tropics/scripts/put_veg_module_args.txt"

write_args(string, filename)

put_veg_module_args <- read_args("/fs/data3/ecowdery/ED_Tropics/scripts/put_veg_module_args.txt")

list2env(put_veg_module_args, .GlobalEnv)

# Before runnign convert.input from put_veg_module

# fcn specific args 
input.args <- list(
  in.path = spp.file$file_path, 
  in.name = spp.file$file_name,
  model_info = model_info,
  new_site = new_site,
  pfts = pfts,
  source = input_veg$source
)

write_args(print("input.id = getveg.id$input.id,
                             outfolder = spp.file$file_path, 
                             formatname = formatname, 
                             mimetype = mimetype,
                             site.id = new_site$id, 
                             start_date = start_date, end_date = end_date, 
                             pkg = pkg, fcn = fcn, 
                             con = con, host = host, browndog = NULL, 
                             write = TRUE, 
                             overwrite = overwrite,
exact.dates = FALSE,

allow.conflicting.dates = TRUE,
insert.new.file = FALSE,
pattern = NULL,

forecast = FALSE,
ensemble = FALSE,
ensemble_name = NULL,
input.args = input.args"), 
           filename = "/fs/data3/ecowdery/ED_Tropics/scripts/put_veg_module_TO_convert_input_args.txt")



rm(list = setdiff(ls(), lsf.str()))
source("/fs/data3/ecowdery/Miscellaneous/read_write_args.R")
put_veg_module_TO_convert_input_args <- read_args("/fs/data3/ecowdery/ED_Tropics/scripts/put_veg_module_TO_convert_input_args.txt")
list2env(put_veg_module_TO_convert_input_args, .GlobalEnv)


# Before running write_ic

saveRDS(fcn.args, "/fs/data3/ecowdery/ED_Tropics/scripts/write_ic_args.Rds")
rm(list = setdiff(ls(), lsf.str()))
fcn.args <- readRDS("/fs/data3/ecowdery/ED_Tropics/scripts/write_ic_args.Rds")
list2env(fcn.args, .GlobalEnv)

# Before match_pfts


match_pfts_args = list(bety_species_id = obs$bety_species_id, 
                       pfts = pfts, 
                       modeltype_id = model_info$modeltype_id, 
                       query = NULL, 
                       con = NULL, 
                       allow_missing = FALSE)

saveRDS(object = match_pfts_args,  
        file = "/fs/data3/ecowdery/ED_Tropics/scripts/match_pfts_args.Rds")

match_pfts_args <- readRDS("/fs/data3/ecowdery/ED_Tropics/scripts/match_pfts_args.Rds")
list2env(match_pfts_args, .GlobalEnv)


veg2model.ED2_args <- list(outfolder = outfolder, veg_info = veg_info, start_date = start_date, new_site = new_site, source = source)

saveRDS(veg2model.ED2_args, "/fs/data3/ecowdery/ED_Tropics/scripts/veg2model.ED2_args.Rds")
veg2model.ED2_args <- readRDS("/fs/data3/ecowdery/ED_Tropics/scripts/veg2model.ED2_args.Rds")
list2env(veg2model.ED2_args, .GlobalEnv)












################################################################################
in.path='/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005'
in.name='ForestGEO.1981.veg.rds'
model='ED2'
new_site=data.frame(id =c(' 1000005005 '),lat =c(' 9.1543 '),lon =c(' -79.8461 '))
pfts=list(pft=list(name='broadleaf_evergreen_tropical_tree', constants='1'))
source='ForestGEO'
overwrite=TRUE
in.prefix='ForestGEO.1981.veg.rds'
outfolder='/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005/'
start_date='1981-03-11 06:00:00'
end_date='1983-07-30 05:00:00'


################################################################################

write_ic_args <- list(in.path='/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005', in.name='ForestGEO.1981.veg.rds', model_info=data.frame(modeltype_id =c(' 1 '),modeltype_name =c(' ED2 '),format_id =c(' 15 '),name =c(' ED2.patch '),mimetype_id =c(' 1073 '),type_string =c(' text/plain ')), new_site=data.frame(id =c(' 1000005005 '),lat =c(' 9.1543 '),lon =c(' -79.8461 ')), pfts=list(pft=list(name='broadleaf_evergreen_tropical_tree', constants='1')), source='ForestGEO', overwrite=FALSE, in.prefix='ForestGEO.1981.veg.rds', outfolder='/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005/', start_date='1981-03-11 06:00:00', end_date='1983-07-30 05:00:00')

list2env(write_ic_args, .GlobalEnv)



match_pfts_args = list(bety_species_id = obs$bety_species_id, 
                       pfts = pfts, 
                       modeltype_id = model_info$modeltype_id, 
                       query = NULL, 
                       con = NULL, 
                       allow_missing = FALSE)

list2env(match_pfts_args, .GlobalEnv)

remotefunc <- function() {PEcAn.data.land::write_ic(in.path='/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005', in.name='ForestGEO.1981.veg.rds', model_info=data.frame(modeltype_id =c(' 1 '),modeltype_name =c(' ED2 '),format_id =c(' 15 '),name =c(' ED2.patch '),mimetype_id =c(' 1073 '),type_string =c(' text/plain ')), new_site=data.frame(id =c(' 1000005005 '),lat =c(' 9.1543 '),lon =c(' -79.8461 ')), pfts=list(pft=list(name='broadleaf_evergreen_tropical_tree', constants='1')), source='ForestGEO', overwrite=FALSE, in.prefix='ForestGEO.1981.veg.rds', outfolder='/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005/', start_date='1981-03-11 06:00:00', end_date='1983-07-30 05:00:00')}