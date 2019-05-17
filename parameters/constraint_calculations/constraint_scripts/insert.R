library(ED.Hydro.Helpers)
bety <- betyConnect("/fs/data3/ecowdery/pecan/web/config.php")
wdns <- 1.000e3
grav <- 9.80665
MPa2m <- wdns / grav

###############################
# Load Data

dat_in <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/conductivity_BETY.csv", na.strings = NaN,  stringsAsFactors = FALSE)

dat <- dat_in %>%
  rename(species = Species, wood_Kmax = ksat, wood_psi50 = P50, wood_density = WD) %>%
  mutate(species = str_to_sentence(species))

###############################
# Merge with species info

sp <- read.csv("/fs/data3/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/species_final.csv", stringsAsFactors = FALSE) %>% select(one_of("submit_name", "bety_id")) %>% rename(species = submit_name, species_id = bety_id)

sp <- sp[-which(sp$species == "Corymbia erythrophloia"),]

# We will lose 5 rows, which are the Eucalyptus
dat <- inner_join(dat, sp)


###############################
# Get dois for the citations

dat <- dat %>% mutate(citation_id = as.numeric(NA), doi = NA)

refs <- dat %>% select(one_of("Ref", "doi", "citation_id")) %>% distinct
View(refs)
refs[which(refs$Ref == "Brodribb et al. 2002"),"doi"] <- "10.1046/j.1365-3040.2002.00919.x"
refs[which(refs$Ref == "Brodribb et al. 2003"),"doi"] <- "10.1046/j.1365-3040.2003.00975.x"
refs[which(refs$Ref == "Bucci et al. 2004"),"doi"] <- "10.1093/treephys/24.8.891"
refs[which(refs$Ref == "Bucci et al. 2006"),"doi"] <- "10.1111/j.1365-3040.2006.01591.x"
refs[which(refs$Ref == "Bucci et al. 2008"),"doi"] <- "10.1590/S1677-04202008000300007"
refs[which(refs$Ref == "Chapotin et al. 2006"),"doi"] <- "10.1111/j.1365-3040.2005.01456.x"
refs[which(refs$Ref == "Chen et al. 2009a, b"),"doi"] <- "10.1007/s00425-009-0959-6"
refs[which(refs$Ref == "Chen et al. 2017"),"doi"] <- "10.1111/1365-2435.12724"
refs[which(refs$Ref == "Choat et al. 2005"),"doi"] <- "10.1007/s00468-004-0392-1"
refs[which(refs$Ref == "Choat et al. 2007"),"doi"] <- "10.1111/j.1469-8137.2007.02137.x"
refs[which(refs$Ref == "Cochard 2006"),"doi"] <- "10.1016/j.crhy.2006.10.012"
refs[which(refs$Ref == "De Guzman et al. 2016"),"doi"] <- "10.1093/treephys/tpw086"
refs[which(refs$Ref == "Domec et al. 2006a"),"doi"] <- "10.1111/j.1365-3040.2005.01397.x"
refs[which(refs$Ref == "Ewers et al. 2004"),"doi"] <- "10.1093/treephys/24.9.1057"
refs[which(refs$Ref == "Feild & Balun 2008"),"doi"] <- "10.1111/j.1469-8137.2007.02306.x "
refs[which(refs$Ref == "Feild & Holbrook 2000"),"doi"] <- "10.1046/j.1365-3040.2000.00626.x"
refs[which(refs$Ref == "Feild et al. 2009 + Feild & Isnard 2013"),"doi"] <- "10.1111/j.1472-4669.2009.00189.x"
refs[which(refs$Ref == "Gartner et al. 1990"),"doi"] <- "10.1002/j.1537-2197.1990.tb14464.x"
refs[which(refs$Ref == "Hacke et al. 2007"),"doi"] <- "10.1086/520724"
refs[which(refs$Ref == "Hao et al. 2008"),"doi"] <- "10.1007/s00442-007-0918-5"
refs[which(refs$Ref == "Johnson et al. 2013"),"doi"] <- "10.3732/ajb.1200590"
refs[which(refs$Ref == "Lopez et al. 2005"),"doi"] <- "10.1093/treephys/25.12.1553"
refs[which(refs$Ref == "Machado & Tyree 1994"),"doi"] <- "10.1093/treephys/14.3.219"
refs[which(refs$Ref == "Meinzer et al. 2003"),"doi"] <- "10.1046/j.1365-3040.2003.01039.x"
refs[which(refs$Ref == "Meinzer et al. 2008"),"doi"] <- ""
refs[which(refs$Ref == "Mendez-Alonzo et al. 2012"),"doi"] <- "10.1890/11-1213.1"
refs[which(refs$Ref == "Santiago et al. 2004"),"doi"] <- "10.1007/s00442-004-1624-1"
refs[which(refs$Ref == "Scholz et al. 2008"),"doi"] <- "10.1590/S1677-04202008000300006"
refs[which(refs$Ref == "Sobrado 1996"),"doi"] <- "10.1007/BF02873864"
refs[which(refs$Ref == "Sobrado 1997"),"doi"] <- "10.1023/A:1001725808647"
refs[which(refs$Ref == "Sperry et al. 1988"),"doi"] <- "10.1111/j.1399-3054.1988.tb00632.x"
refs[which(refs$Ref == "Sperry et al. 2007"),"doi"] <- "10.1086/520726"
refs[which(refs$Ref == "Tyree & Sperry 1989"),"doi"] <- ""
refs[which(refs$Ref == "Tyree et al. 1991"),"doi"] <- ""
refs[which(refs$Ref == "Tyree et al. 1998"),"doi"] <- ""
refs[which(refs$Ref == "Vander Willigen et al. 2000"),"doi"] <- ""
refs[which(refs$Ref == "Zhang et al. 2009"),"doi"] <- ""
refs[which(refs$Ref == "Zhu & Cao 2009"),"doi"] <- ""
refs[which(refs$Ref == "Zhu et al. 2017"),"doi"] <- "10.1093/treephys/tpx094"
refs[which(refs$Ref == "Zotz et al. 1994"),"doi"] <- "10.1111/j.1469-8137.1994.tb04279.x"
refs[which(refs$Ref == "Zotz et al. 1997"),"doi"] <- ""
refs[which(refs$Ref == "Van der Sande et al. 2013 + unpublished data"),"doi"] <- "10.1007/s00442-012-2563-x"
refs[which(refs$Ref == "Van der Sande & Markesteijn et al. unpublished data"),"doi"] <- ""
refs[which(refs$Ref == ""),"doi"] <- ""

refs$citation_id <- NA

for(i in seq_along(refs$doi)){
  if(refs$doi[i] != ""){
    DOI <- refs$doi[i]
    check <- tbl(bety, "citations") %>% filter(doi == DOI) %>% collect()
    if(nrow(check) == 1){
      refs$citation_id[i] <- check$id
      print(sprintf("%i | Checked", i))
    }
  }
}
