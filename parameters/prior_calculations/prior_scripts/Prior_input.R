library(SHELF) # remotes::install_version("SHELF", "1.3.0")
library(PEcAn.DB)
library(PEcAn.utils)
library(PEcAn.settings)

library(dplyr)
library(tidyr)
library(tidyverse)

library(googlesheets4)
library(stringr)



################################################################################
## Read in the data
## Currently reads from google spreadsheets

bety <- betyConnect("web/config.php")

url <- "https://docs.google.com/spreadsheets/d/1nfYeEZcZ335Y6NrCglG6m2absYYP9tzR8TmF7sdtNo4/edit?usp=sharing"
priors <- suppressWarnings(read_sheet(url, col_types = "nccccccccccnnnnnnnnncc"))
priors <- read.csv("/fs/data3/ecowdery/ED_Tropics/priors/Copy of ED2_hydraulics_priors_BC - Feuille 1 (3).csv")
names(priors) <- str_replace_all(names(priors) ," ",".")

quants <- tibble(name = c("low.025", "upp.975", "low.25", "upp.75", "mean"), 
                 quant = c(.025, .975, .25, .75, .5), stringsAsFactors = FALSE)

for(i in seq_along(priors$ED2.parameter.name)){
  
  print(paste0(i, ": ", priors$ED2.parameter.name[i]))
  
  ##############################################################################
  ## Determine parameters that can be used for fit
  test <- priors[i,] %>% select_if(~ !any(is.na(.)))
  
  sub <- priors[i,] %>% 
    select_if(~ !any(is.na(.))) %>% 
    select_if(names(test) %in% quants$name) %>% 
    gather(key = name)
  # glimpse(sub)
  
  #If there aren't any parameters, skip the variable
  if(dim(sub)[2] == 0){ 
    print("skipping")
    next
  } 
  
  sub$value <- as.numeric(sub$value)
  sub2 <- merge(sub, quants) %>% na.omit()
  
  # if(priors[i,"Unit"] != priors[i,"Old_unit"]){
  #   sprintf("Converting units from %s to %s", priors[i,"Old_unit"], priors[i,"Unit"])
  #   sub2$value <- udunits2::ud.convert(sub2$value, priors[i,"Old_unit"], priors[i,"Unit"])
  # }
  # 

  
  args <- list(
    vals = sub2$value, 
    probs = sub2$quant
  )
  
  lower <- test %>% select_if(names(test) %in% c("theor.min"))
  lower <- ifelse(all(dim(lower) == c(1,1)), pull(lower), NA)
  upper <- test %>% select_if(names(test) %in% c("theor.max"))
  upper <- ifelse(all(dim(upper) == c(1,1)), pull(upper), NA)
  
  if(!is.na(lower)) args = append(args, list(lower = lower))
  if(!is.na(upper)) args = append(args, list(upper = upper))
  
  ##############################################################################
  ## Calculate fit and insert in to the database 
  ## Supported distributions: Normal, Beta, Log Normal, Beta
  ## Distributions we want: chisq, exp, f, unif, weibull
  
  myfit <- do.call(fitdist, args)
  
  variable_id.in <- tbl(bety,"variables") %>% filter(name == priors$ED2.parameter.name[i]) %>% pull(id)
  phylogeny.in <- "plants"
  distn.in <- myfit$ssq %>% dplyr::select(one_of("Normal", "Gamma", "Log normal", "Beta")) %>%
    which.min %>% names
  
  input_prior(bety, variable_id.in, phylogeny.in, distn.in)
  
  ##############################################################################
  ## Plot 
  
  print("plotting")
  lwd = 5*min(myfit$ssq,na.rm = TRUE)/myfit$ssq
  xval = seq(min(sub2$value),max(sub2$value),length=1000)
  plot(sub2$value, sub2$quant, main = paste(priors$ED2.parameter.name[i],"
",priors$Description[i]))
  
  if(!is.na(myfit$Normal[1])){
    lines(xval,pnorm(xval,myfit$Normal$mean,myfit$Normal$sd),col=2,lwd=lwd[1])
  }
  if(!is.na(myfit$Gamma[1])){
    lines(xval,pgamma(xval,myfit$Gamma$shape,myfit$Gamma$rate),col=3,lwd=lwd[3])
  }
  if(!is.na(myfit$Log.normal[1])){
    lines(xval,plnorm(xval,myfit$Log.normal$mean.log.X,myfit$Log.normal$sd.log.X),col=4,lwd=lwd[4])
  }
  if(!is.na(myfit$Beta[1])){
    lines(xval,pbeta(xval,myfit$Beta$shape1,myfit$Beta$shape2),col=5,lwd=lwd[5])
  }
  legend("bottomright",legend=c("Normal","Gamma","Log.normal","Beta"),col=2:5,lwd=2)
}

################################################################################
## Calculate leaf_psi_tlp by hand

pinot_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000295) %>% collect
epsil_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000294) %>% collect

pinot_samp <- rnorm(1000000,mean = pinot_fit$parama, sd = pinot_fit$paramb) 
epsil_samp <- rgamma(1000000, shape = epsil_fit$parama, rate = epsil_fit$paramb) * 1/0.009804139432

pitlp_samp <- (pinot_samp*epsil_samp)/(pinot_samp+epsil_samp) 
plot(density(pitlp_samp))

qs <- quantile(pitlp_samp, c(.025,.25,.5,.75,.975))
max(pitlp_samp)
min(pitlp_samp)

myfit <- fitdist(vals = qs, probs =c(.025,.25,.5,.75,.975))

variable_id.in <- 1000000284
phylogeny.in <- "plants"
distn.in <- myfit$ssq %>% dplyr::select(one_of("Normal", "Gamma", "Log normal", "Beta")) %>%
  which.min %>% names

input_prior(bety, myfit, variable_id.in, phylogeny.in, distn.in)


################################################################################
## Calculate Kexp by hand

avuln_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000302) %>% collect()
avuln_samp <- rgamma(100000, shape = avuln_fit$parama, rate = avuln_fit$paramb)

psi50_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000289) %>% collect()
psi50_samp <- rnorm(100000,mean = psi50_fit$parama, sd = psi50_fit$paramb)

hist(psi50_samp)
hist(psi50_samp * 0.009804139432)

Kexp_samp <- 4 * avuln_samp / 100 * (-psi50_samp * 0.009804139432)
hist(Kexp_samp)
mean(Kexp_samp)

qs <- quantile(Kexp_samp, c(.025,.25,.5,.75,.975))
max(pitlp_samp)
min(pitlp_samp)

myfit <- fitdist(vals = qs, probs =c(.025,.25,.5,.75,.975))

variable_id.in <- 1000000291
phylogeny.in <- "plants"
distn.in <- myfit$ssq %>% dplyr::select(one_of("Normal", "Gamma", "Log normal", "Beta")) %>%
  which.min %>% names

input_prior(bety, myfit, variable_id.in, phylogeny.in, distn.in)


################################################################################
## Calculate water capacity by hand

# Brad has pinot and epsil the same for leaf and wood so I'll use the same for both priors
# Similarly, this means psi_tlp can use the same priors

psi_osmotic_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000295) %>% collect
psi_osmotic_samp <- rnorm(1000000, mean = psi_osmotic_fit$parama, sd = psi_osmotic_fit$paramb)

psi_tlp_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000284) %>% collect()
psi_tlp_samp <- rnorm(1000000, mean = psi_tlp_fit$parama, sd = psi_tlp_fit$paramb)

leaf_water_sat_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000285) %>% collect()
leaf_water_sat_samp <- rnorm(1000000, mean = leaf_water_sat_fit$parama, sd = leaf_water_sat_fit$paramb)

wood_water_sat_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000286) %>% collect()
wood_water_sat_samp <- rnorm(1000000, mean = wood_water_sat_fit$parama, sd = wood_water_sat_fit$paramb)

plot(density(leaf_water_sat_samp))
lines(density(wood_water_sat_samp))

leaf_water_cap <- (1 - psi_osmotic_samp / (4 * psi_tlp_samp)) * (leaf_water_sat_samp / (4 * abs(psi_tlp_samp)))
wood_water_cap <- (1 - psi_osmotic_samp / (4 * psi_tlp_samp)) * (wood_water_sat_samp / (4 * abs(psi_tlp_samp)))

plot(density(leaf_water_cap))
lines(density(wood_water_cap))

qs_leaf <- quantile(leaf_water_cap, c(.025,.25,.5,.75,.975))
qs_wood <- quantile(wood_water_cap, c(.025,.25,.5,.75,.975))

myfit_leaf <- fitdist(vals = qs_leaf, probs =c(.025,.25,.5,.75,.975))
variable_id.in <- 1000000287
phylogeny.in <- "plants"
distn.in <- myfit_leaf$ssq %>% dplyr::select(one_of("Normal", "Gamma", "Log normal", "Beta")) %>%
  which.min %>% names
input_prior(bety, myfit_leaf, variable_id.in, phylogeny.in, distn.in)

myfit_wood <- fitdist(vals = qs_wood, probs =c(.025,.25,.5,.75,.975))
variable_id.in <- 1000000288
phylogeny.in <- "plants"
distn.in <- myfit_wood$ssq %>% dplyr::select(one_of("Normal", "Gamma", "Log normal", "Beta")) %>%
  which.min %>% names
input_prior(bety, myfit_wood, variable_id.in, phylogeny.in, distn.in)

################################################################################
## Calculate psi_min capacity by hand


leaf_elastic_mod_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000294) %>% collect()
leaf_elastic_mod <- rgamma(1000000, shape = leaf_elastic_mod_fit$parama, rate = leaf_elastic_mod_fit$paramb)

leaf_water_sat_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000285) %>% collect()
leaf_water_sat <- rnorm(1000000, mean = leaf_water_sat_fit$parama, sd = leaf_water_sat_fit$paramb)

leaf_water_cap_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000287) %>% collect()
leaf_water_cap <- rnorm(1000000, mean = leaf_water_cap_fit$parama, sd = leaf_water_cap_fit$paramb)

leaf_rwc_min = 0.01 * leaf_elastic_mod + 0.17
leaf_psi_min = (leaf_rwc_min - 1.) * leaf_water_sat / leaf_water_cap

plot(density(leaf_psi_min))

qs <- quantile(leaf_psi_min, c(.025,.25,.5,.75,.975))
myfit <- fitdist(vals = qs, probs =c(.025,.25,.5,.75,.975))

variable_id.in <- 1000000299
phylogeny.in <- "plants"
distn.in <- myfit$ssq %>% dplyr::select(one_of("Normal", "Gamma", "Log normal", "Beta")) %>%
  which.min %>% names

input_prior(bety, myfit, variable_id.in, phylogeny.in, distn.in)

leaf_psi_min_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000299) %>% collect()
leaf_psi_min <- rnorm(1000000, mean = leaf_psi_min_fit$parama, sd = leaf_psi_min_fit$paramb)

plot(density(leaf_psi_min))

################################################################################
## Calculate leaf_density


leaf_elastic_mod_fit <- tbl(bety, "priors") %>% filter(variable_id == 1000000294) %>% collect()
leaf_elastic_mod <- rgamma(1000000, shape = leaf_elastic_mod_fit$parama, rate = leaf_elastic_mod_fit$paramb)

leaf_density <- (leaf_elastic_mod - 2.03) / 25.4 * 1.e3 # kg m-3

qs <- quantile(leaf_density, c(.025,.25,.5,.75,.975))
myfit <- fitdist(vals = qs, probs =c(.025,.25,.5,.75,.975))

variable_id.in <- 1000000304
phylogeny.in <- "plants"
distn.in <- myfit$ssq %>% dplyr::select(one_of("Normal", "Gamma", "Log normal", "Beta")) %>%
  which.min %>% names

input_prior(bety, myfit, variable_id.in, phylogeny.in, distn.in)
