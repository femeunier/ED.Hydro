# library(stringr)
# library(dplyr)
source("/fs/data3/ecowdery/Miscellaneous/read_write_args.R")



# Setup for running ic_process
library(PEcAn.all)

# read in pecan settings - make sure the pecan xml has the css, pss, site sections in inputs
settings <- read.settings("/fs/data3/ecowdery/ED_Tropics/pecan.BCI_IC_process.xml")

# Choose one of the inputs entries to process, will need to do three for css, pss, site
input <- settings$run$inputs[[1]]
overwrite = FALSE

new_settings <- PEcAn.data.land::ic_process(settings, input, dir, overwrite)


# Currently gives the error 
# Error in convert.input(input.id = NA, outfolder = outfolder, formatname = "spp.info",  : 
# object 'new.site' not found

readRDS("/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005/ForestGEO.1981.veg.rds")

#####################
# get_veg_module_args
 
get_veg_module_args <- read_args("/fs/data3/ecowdery/ED_Tropics/scripts/get_veg_module_args.txt")
list2env(get_veg_module_args, .GlobalEnv)

i = 1
getveg.id[[i]] <- do.call(get_veg_module, get_veg_module_args)

rm(list = setdiff(ls(), lsf.str()))


#####################
# put_veg_module_args

source("/fs/data3/ecowdery/Miscellaneous/read_write_args.R")
put_veg_module_args <- read_args("/fs/data3/ecowdery/ED_Tropics/scripts/put_veg_module_args.txt")
list2env(put_veg_module_args, .GlobalEnv)
putveg.id[[i]] <- do.call(put_veg_module, put_veg_module_args)

# Next to convert.input

source("/fs/data3/ecowdery/Miscellaneous/read_write_args.R")

bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")
con <- bety$con

put_veg_module_TO_convert_input_args <- read_args("/fs/data3/ecowdery/ED_Tropics/scripts/put_veg_module_TO_convert_input_args.txt")
str(put_veg_module_TO_convert_input_args)

put_veg_module_TO_convert_input_args$con <- bety$con

# do.call(convert.input, put_veg_module_TO_convert_input_args)
list2env(put_veg_module_TO_convert_input_args, .GlobalEnv)

# Next to 

fcn.args <- readRDS("/fs/data3/ecowdery/ED_Tropics/scripts/write_ic_args.Rds")
list2env(fcn.args, .GlobalEnv)

results <- do.call(write_ic, fcn.args)


# Checking out match_pft
source("/fs/data3/ecowdery/Miscellaneous/read_write_args.R")
match_pft_args <-read_args("/fs/data3/ecowdery/ED_Tropics/scripts/match_pfts_args.txt")
list2env(fcn.args, .GlobalEnv)

# write_args(
# print("outfolder = outfolder, veg_info= veg_info, start_date = start_date, new_site = new_site, source = source"),
# filename = "/fs/data3/ecowdery/ED_Tropics/veg2model.ED2_args.txt")




# test <- readRDS("/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005/ForestGEO.1981.veg.rds")
# saveRDS(test[2], "/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005/ForestGEO.1981.veg.rds")
# 
# test2 <- readRDS("/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005/ForestGEO.1981.veg.test.rds")
# 
# test2[[2]] <- "hi"

################################################################################
# Gives an error further down the line about species ids
# 
# 2018-11-14 14:31:39 ERROR  [match_species_id] : 
# Species for the following code(s) not found : UNIDENTIFIED SPECIES 
# 2018-11-14 14:31:39 WARN   [PEcAn.data.land::load_veg] : 
#   IMPORTANT : No area info passed via metadata, if your model needs plot 
# area in IC calculations please provide it under 
# 'settings$run$inputs$css$metadata$area'. 
# Error in data.frame(..., check.names = FALSE) : 
#   arguments imply differing number of rows: 263934, 263936

# get.veg.module line 70 
# 
# Setup all the arguments to convert.inputs

input.id = NA
outfolder = outfolder
formatname = "spp.info"
mimetype = "application/rds"
site.id = site_id
start_date = start_date

end_date = end_date
pkg = pkg
fcn = fcn
con = con
host = host
browndog = NULL
write = TRUE

# format.vars = is this necessary?
overwrite = overwrite
exact.dates = FALSE 

allow.conflicting.dates = TRUE 
insert.new.file = FALSE
pattern = NULL

forecast = FALSE
ensemble = FALSE
ensemble_name = NULL


# fcn specific args 
new_site = new.site
source_id = source.id
format_name = input_veg$match.format
dbparms = dbparms
machine_host = machine_host
source = input_veg$source
icmeta = input_veg$metadata

input.args <- list(new_site = new.site,
                   source_id = source.id,
                   format_name = input_veg$match.format,
                   dbparms = dbparms,
                   machine_host = machine_host,
                   source = input_veg$source,
                   icmeta = input_veg$metadata)

getveg.id <- convert.input(input.id = NA,
                           outfolder = outfolder, 
                           formatname = "spp.info", 
                           mimetype = "application/rds",
                           site.id = site_id, 
                           start_date = start_date, end_date = end_date, 
                           pkg = pkg, fcn = fcn, 
                           con = con, host = host, browndog = NULL, 
                           write = TRUE, 
                           overwrite = overwrite, 
                           # fcn specific args 
                           new_site = new.site,
                           source_id = source.id,
                           format_name = input_veg$match.format,
                           dbparms = dbparms,
                           machine_host = machine_host,
                           source = input_veg$source,
                           ##  any metadata passed via settings to be used in the IC files (in veg2model)
                           ##  if different than defaults, e.g.:
                           ##
                           ##  <metadata>
                           ##   <trk>2</trk>
                           ##   <age>70</age>
                           ##  </metadata>
                           ##
                           icmeta = input_veg$metadata)

# Ultimately produce this command at convert.input line 603
# THIS is where the error happens
# 
# PEcAn.data.land::load_veg(new_site=data.frame(id =c(' 1000005005 '),lat 
# =c(' 9.1543 '),lon =c(' -79.8461 ')), source_id='1000011170', 
# format_name=NULL, dbparms=list(bety=list(user='bety', password='bety', 
#                                          host='128.197.168.114', dbname='bety', driver='PostgreSQL', 
#                                          write='false'), dbfiles='/fs/data1/pecan.data/dbfiles'), 
# machine_host='test-pecan.bu.edu', source='ForestGEO', icmeta=NULL, 
# overwrite=TRUE, 
# outfolder='/fs/data3/ecowdery/ED_Tropics//ForestGEO_site_1-5005/', 
# start_date='1981-03-11 06:00:00', end_date='1983-07-30 05:00:00') 

new_site=data.frame(id=c(' 1000005005 '),lat=c(' 9.1543 '),lon =c(' -79.8461 '))
source_id='1000011170',
format_name=NULL
dbparms=list(bety=list(user='bety', password='bety',
                       host='128.197.168.114', dbname='bety', driver='PostgreSQL',
                       write='false'), dbfiles='/fs/data1/pecan.data/dbfiles')
machine_host='test-pecan.bu.edu'
source='ForestGEO'
icmeta=NULL
overwrite=TRUE
outfolder='/fs/data3/ecowdery/ED_Tropics//ForestGEO_site_1-5005/'
start_date='1981-03-11 06:00:00'
end_date='1983-07-30 05:00:00'


PEcAn.data.land::load_veg(new_site=data.frame(id =c(' 1000005005 '),lat
                                              =c(' 9.1543 '),lon =c(' -79.8461 ')), source_id='1000011170',
                          format_name=NULL, dbparms=list(bety=list(user='bety', password='bety',
                                                                   host='128.197.168.114', dbname='bety', driver='PostgreSQL',
                                                                   write='false'), dbfiles='/fs/data1/pecan.data/dbfiles'),
                          machine_host='test-pecan.bu.edu', source='ForestGEO', icmeta=NULL,
                          overwrite=TRUE,
                          outfolder='/fs/data3/ecowdery/ED_Tropics//ForestGEO_site_1-5005/',
                          start_date='1981-03-11 06:00:00', end_date='1983-07-30 05:00:00')

# The match_species_id code is returning a dataframe that has two extra rows. Weird.

input_codes = obs[[code.col]]
format_name = format_name
bety = bety
translation_table = NULL

spp.info <- match_species_id(input_codes = obs[[code.col]], format_name = format_name, bety = bety)

################################################################################
################################################################################
# Whew Made it to put_veg_module

i = 1

getveg.id  = list(input.id = 1000025159, dbfile.id = 1000156072)
dbparms    = list(bety = list( user = "bety", password = "bety", 
                               host = "128.197.168.114", dbname = "bety", 
                               driver = "PostgreSQL", write = "false"),  
                  dbfiles = "/fs/data1/pecan.data/dbfiles")
input_veg  = list(source = "ForestGEO", output = "css", username = "pecan", id = "1000011170", useic = "TRUE")
pfts       = structure(list(pft = structure(list(name = "broadleaf_evergreen_tropical_tree", 
                                                 constants = structure(list(num = "1"), .Names = "num")),
                                            .Names = c("name", "constants"))), .Names = "pft")
outfolder  =  "/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005"
n.ensemble = i
dir        = "/fs/data1/pecan.data/dbfiles"
machine    = structure(list(id = 11, hostname = "test-pecan.bu.edu", 
                            created_at = structure(1397833372.16937, class = c("POSIXct", "POSIXt"), tzone = ""), 
                            updated_at = structure(1397833372.16937, class = c("POSIXct", "POSIXt"), tzone = ""), 
                            sync_host_id = NA_real_, sync_url = NA_character_, 
                            sync_contact = NA_character_, sync_start = NA_real_, sync_end = NA_real_), 
                       .Names = c("id", "hostname", "created_at", "updated_at", "sync_host_id", 
                                  "sync_contact", "sync_start", "sync_end"), 
                       row.names = 1L, class = "data.frame")
model      = "ED2"
start_date = "1981-03-11 06:00:00 EST"
end_date   = "1983-07-30 05:00:00 EDT"
new_site   = structure(list(id = 1000005005, lat = 9.1543, lon = -79.8461), 
                       .Names = c("id", "lat", "lon"), row.names = c(NA, -1L), class = "data.frame")
host       = structure(list(name = "localhost", user = "ecowdery", folder = "/projectnb/dietzelab/ecowdery/ED_Tropics/outputs", 
                            prerun = "module load udunits/2.2.20 R/3.5.0", qsub = "qsub -l h_rt=24:00:00 -pe omp 8 -q 'geo*' -V -v OMP_NUM_THREADS=8 -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash", 
                            qsub.jobid = "Your job ([0-9]+) .*", qstat = "qstat -j @JOBID@ || echo DONE", 
                            tunnel = "/fs/data3/ecowdery/ED_Tropics/tunnel/tunnel", rundir = "/projectnb/dietzelab/ecowdery/ED_Tropics/outputs/run",
                            outdir = "/projectnb/dietzelab/ecowdery/ED_Tropics/outputs/out"), 
                       .Names = c("name", "user", "folder", "prerun", "qsub", "qsub.jobid", "qstat", "tunnel", "rundir", "outdir")) 
overwrite  = FALSE

putveg.id[[i]] <- put_veg_module(getveg.id  = getveg.id[[i]], 
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
                                 overwrite  = overwrite$putveg)

# Args for convert.input

input.id = getveg.id
outfolder = spp.file$file_path
formatname = formatname
mimetype = mimetype
site.id = new_site$id
start_date = start_date

end_date = end_date
pkg = pkg
fcn = fcn
con = con
host = host
browndog = NULL
write = TRUE
# format.vars will be missing

overwrite = overwrite # The rest is auto filled
exact.dates = FALSE

allow.conflicting.dates = TRUE
insert.new.file = FALSE
pattern = NULL

forecast = FALSE
ensemble = FALSE
ensemble_name = NULL

# fcn specific args 
input.args <- list(
  in.path = spp.file$file_path,
  in.name = spp.file$file_name,
  model = model,
  new_site = new_site,
  pfts = pfts,
  source = input_veg$source
)




#############

in.path='/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005'
in.name='ForestGEO.1981.veg.rds'
model='ED2'
new_site=data.frame(id =c(' 1000005005 '),lat =c(' 9.1543 '),lon =c(' -79.8461 ')) 
pfts=list(pft=list(name='broadleaf_evergreen_tropical_tree', 
                   constants='1'))
source='ForestGEO'
overwrite=TRUE 
in.prefix='ForestGEO.1981.veg.rds'
outfolder='/fs/data1/pecan.data/dbfiles/ForestGEO_site_1-5005/' 
start_date='1981-03-11 06:00:00'
end_date='1983-07-30 05:00:00' 


PEcAn.data.land::write_ic(in.path, 
                          in.name, model, new_site, 
                          pfts, source, overwrite, 
                          in.prefix, 
                          outfolder, 
                          start_date, end_date) 

# Breaking at matching pfts
# BECAUSE IT HAS TO BREAK AT EVERY FUCKING STEP


bety_species_id = obs$bety_species_id
pfts = pfts
query = NULL
con = NULL
allow_missing = FALSE

PEcAn.data.land::match_pft(bety_species_id, pfts)

table_name <- 'modeltypes'
model_name <- 'ED2'


bety <- betyConnect()
tbl(bety, table_name) %>% filter(name == model_name)

traits::betydb_query(name = model_name, table = table_name, user = 'bety', pwd = 'bety')


##############################################

write_args(string, "/fs/data3/ecowdery/Miscellaneous/get_veg_module_args.txt")
args <- read_args("/fs/data3/ecowdery/Miscellaneous/get_veg_module_args.txt")

str(args)
do.call(get_veg_module, args)
