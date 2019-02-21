library(dplyr)
library(rhdf5)
rhdf5::h5closeAll()

rhdf5::h5ls("/fs/data3/ecowdery/ED_Tropics/inputs//BCI_2004APR.h5")

file.copy("/fs/data3/ecowdery/ED_Tropics/inputs//BCI_2004APR.h5", "/fs/data3/ecowdery/ED_Tropics/inputs/Drought/BCI_2004APR.h5", overwrite = TRUE)
fid <- rhdf5::H5Fopen("/fs/data3/ecowdery/ED_Tropics/inputs//BCI_2004APR.h5")

rain <- fid$"prate"
rain <- rain / 2

rhdf5::H5Fflush(fid)

fid$'prate'[668,,]


rhdf5::H5Dclose(rain)
rhdf5::H5Fclose(fid)


library(dplyr)
rhdf5::h5closeAll()

dir("/fs/data3/ecowdery/ED_Tropics/inputs/")

old_files <- dir("/fs/data3/ecowdery/ED_Tropics/inputs/", pattern = "BCI_20", full.names = TRUE)
i = 1

old_met <- c()
new_met <- c()

for(i in seq_along(old_files)){
  
  new_file <- file.path("/fs/data3/ecowdery/ED_Tropics/inputs/Drought", basename(old_files[i]))
  file.copy(old_files[i], new_file, overwrite = TRUE)
  fid <- rhdf5::H5Fopen(new_file)
  fid$prate[668,,]
  
  
  rain <- fid$"prate"
  new_rain <- fid$prate / 2
  new_rain[668,,]
 rain <- new_rain
 fid$"prate" <- new_rain
  
  rhdf5::H5Fflush(fid)
  fid$prate[668,,]

  rhdf5::h5closeAll()
  
  fold <- rhdf5::H5Fopen(old_files[i])
  fnew <- rhdf5::H5Fopen(new_file)
  
  old_met <- c(old_met, fold$prate[,1,1])
  new_met <- c(new_met, fnew$prate[,1,1])
  
  if(all(fold$prate[,1,1] == fnew$prate[,1,1])){
    print("fail")
  }
  if(all(fold$prate[,1,1] == 2 * fnew$prate[,1,1])){
    print("success")
  }
  
  rhdf5::h5closeAll()

}

plot(old_met, type = "l", lwd = 2)
lines(new_met, col = "red", lwd = 2)
