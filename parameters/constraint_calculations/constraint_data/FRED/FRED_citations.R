library(rcrossref)
library(dplyr)

FRED <- read.csv("/fs/data3/ecowdery/FRED/FRED_sub.csv")

all_doi <- c(unique(as.character(FRED$Data_source_DOI))) 
all_doi <- all_doi[!is.na(all_doi)]
bibs <- list()

for(i in seq_along(all_doi)){
  bibs[[i]] = cr_cn(dois = all_doi[i], format = "bibentry")
}

save(bibs, file = "/fs/data3/ecowdery/FRED/FRED_citations.Rds")