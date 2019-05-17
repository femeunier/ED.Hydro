#' @param dat_site_cite
#' @return dat_site_cite
#' @export

cite2site <- function(dat_site_cite, interval = .1){

  # Steps:
  # 1) Check for the site in the citation
  # 2) Check for the site in all sites (if it's there but not linked to citation, I guess I should probably link it.)
  # 3) If site in neither places, add it and also link to citation

  dat_site_cite <- dat_site_cite[order(dat_site_cite$citation_id),]

  for(i in 1:nrow(dat_site_cite)){
    if(is.na(dat_site_cite$site_id[i])){

      print(paste("#---------------------------------------------------------#"))
      print(paste0("Citation ID:", dat_site_cite$citation_id[i]))
      print(sprintf("lat: %.2f, lon: %.2f", dat_site_cite$lat[i], dat_site_cite$lon[i]))

      # 1) Does the citation already have a site attached?
      cit_id <- dat_site_cite$citation_id[i]
      test1 <- tbl(bety, "citations_sites") %>% filter(citation_id == cit_id) %>% collect()

      # 2) Are there things in BETY that are nearby the site?
      lat <- as.numeric(dat_site_cite[i,"lat"])
      lon <- as.numeric(dat_site_cite[i,"lon"])
      test2 <- nearby_sites(lat, lon, interval, bety = bety)

      if(nrow(test1) >= 1 & nrow(test2) >= 1){
        join <- inner_join(test1 %>% select("citation_id", "site_id"), test2, by = c("site_id" = "id"))

        if(nrow(join) == 1) {
          dat_site_cite[i, "site_id"] <- join$site_id
          print("DONE")
        }else{
          print("Got as far as join:")
          print(join)
        }
      }else{
        print("Test 1:")
        print(test1)
        print("Test 2:")
        print(test2)
      }
    }
  }
  return(dat_site_cite)
}
