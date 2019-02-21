library(hdf5r)
library(ncdf4)
library(ncdf4.helpers)


for(i in seq_along(old_files)){
  file.copy(old_files[i], "/fs/data3/ecowdery/ED_Tropics/inputs/Drought/")
}



old_files <- dir("/fs/data3/ecowdery/ED_Tropics/inputs/", pattern = "BCI_2004", full.names = TRUE)

for(i in seq_along(old_files)){
  
  new_file <- file.path("/fs/data3/ecowdery/ED_Tropics/inputs/Drought", basename(old_files[i]))
  ed_met_h5 <- hdf5r::H5File$new(new_file)
  
  nc <- nc_open(old_files[i])
  vars <- nc.get.variable.list(nc)
  
  dims <- c(length(ncvar_get(nc,vars[i])), 1, 1)
    
  for(j in seq_along(vars)){
    ed_met_h5[[vars[j]]] <- array(ncvar_get(nc, vars[j]), dim = dims)
  }
  ed_met_h5$close_all()
  
  metvar <- c("nbdsf", "nddsf", "vbdsf", "vddsf", "prate", "dlwrf",
              "pres", "hgt", "ugrd", "vgrd", "sh", "tmp", "co2")
  metvar_table <- data.frame(
    variable = metvar,
    update_frequency = dt,
    flag = 1
  )
  
  
}

nc_close(nc)
