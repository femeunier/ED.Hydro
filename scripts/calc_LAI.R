nc <- nc_open("/fs/data2/output/PEcAn_1000010023/out/1002470402/history-S-2004-01-01-000000-g01.h5")
nc2 <- nc_open("/fs/data2/output/PEcAn_1000010023/out/1002470402/2004.nc")

history_xml <- XML::xmlParse("/fs/data2/output/PEcAn_1000010023/out/1002470402/history.xml")
history <- XML::xmlToList(history_xml)

b2B1 <- history$pft$b2Bl_large %>% strsplit(.,"\n") %>% 
  unlist(.) %>% .[2] %>% as.numeric
C2B <- history$pftconst$C2B %>% strsplit(.,"\n") %>% 
  unlist(.) %>% .[2] %>% as.numeric
dbh.crit <- history$pft$dbh_crit %>% strsplit(.,"\n") %>% 
  unlist(.) %>% .[2] %>% as.numeric
dbh.adult <- history$pft$dbh_adult %>% strsplit(.,"\n") %>% 
  unlist(.) %>% .[2] %>% as.numeric
SLA <- history$pft$SLA %>% strsplit(.,"\n") %>% 
  unlist(.) %>% .[2] %>% as.numeric


calc_LAI <- function(b1Bl){
  
  # b2Bl <- 0.9749494195
  # C2B  <- 2.0000000000
  # dbh.crit <- 96.2577896118
  # dbh.adult <- 10.0000000000
  
  dbh <- ncvar_get(nc, "DBH")
  
  dbhuse = pmin(dbh, dbh.crit) + 0. * dbh
  
  bleaf  = b1Bl /C2B * dbhuse ^ b2Bl
  
  data.frame(bleaf, ncvar_get(nc, "BLEAF"))
  
  nplant <- ncvar_get(nc, "NPLANT")
  
  SLA  <- ncvar_get(nc, "SLA")
  # SLA <- 33
  # SLA <- 27.0611785168147
  
  LAI <- sum(SLA * nplant * bleaf)
  
  LAI
  
}

b1Bl <- 0.7096262574
calc_LAI(b1Bl)

n = 1000
test_b1Bl <- seq(from = 0,to = 1,length.out = n)

test_LAI <- rep(NA, n)
for(i in 1:n){
  test_LAI[i] <- calc_LAI(test_b1Bl[i])
}

plot(test_b1Bl, test_LAI)
test_b1Bl[max(which(test_LAI < 7.3))]


calc_LAI(.46)

ncvar_get(nc2, "LAI")
