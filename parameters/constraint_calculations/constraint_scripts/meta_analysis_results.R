library(ED.Hydro.Helpers)
library(grid)
traitnames <- read.csv("/fs/data3/ecowdery/ED.Hydro/run_results/pft/ED_tropical_hydro/prior.distns.csv", stringsAsFactors = FALSE) %>% pull(X)

load("/fs/data3/ecowdery/ED.Hydro/run_results/pft/ED_tropical_hydro/madata.Rdata")
load("/fs/data3/ecowdery/ED.Hydro/run_results/pft/ED_tropical_hydro/prior.distns.Rdata")
prior.distns$traits <- traitnames
load("/fs/data3/ecowdery/ED.Hydro/run_results/pft/ED_tropical_hydro/post.distns.Rdata")
post.distns$traits <- traitnames


for(i in seq_along(traitnames)){
  print(i)
  
  pr <- prior.distns %>% filter(traits == traitnames[i])
  po <- post.distns %>% filter(traits == traitnames[i])
  
  same <- 
    pr$distn == po$distn &
    pr$parama == po$parama &
    pr$paramb == po$paramb
  
  if(!same){
    dat <- data.frame(prior = rdistn(pr), 
                      post = rdistn(po) )
    dat2 <- gather(dat)
    
    ED_name1 <- traitnames[i]
    if(traitnames[i] == "fineroot2leaf") ED_name1 <- "q"
    if(traitnames[i] == "Vcmax") ED_name1 <- "Vm0"
    
    ED_name2 <- traitnames[i]
    if(traitnames[i] == "fineroot2leaf") ED_name2 <- "q"
    
    plot_default <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/parameters/pft3_defaults_history.xml", ED_name1)
    plot_default <- ifelse(ED_name1 =="wood_density", 1, plot_default)
    new_config <- get_ED_default("/fs/data3/ecowdery/ED.Hydro/run_results/run/ENS-00001-1000005005/config.xml", ED_name2)
    
    q <- c(0,0.97)
    plot_range <- quantile(dat2$value, q)
    
    p <- ggplot(dat2) + 
      geom_vline(aes(xintercept = plot_default, color = "old"), size = 1) + 
      geom_vline(aes(xintercept = new_config, color = "new"), linetype = 1, size = 1) + 
      geom_density(aes(x = value, fill = key), alpha = .5)
    
    points <- data.frame(x = madata[[traitnames[i]]]$Y, y = 0)
    
    mainplot <- p + 
      ggtitle(traitnames[i]) + 
      geom_point(data = points, aes(x = x, y = y))
      xlim(plot_range)
    
    big <- max(plot_default, new_config)
    small <- min(plot_default, new_config)
    buffer <- (big-small)/10
    
    subplot <- p + 
      xlim(small - buffer, big + buffer)
    
    vp <- viewport(width = 0.4, height = 0.4, x = .9,
                   y = .9, just = c("right", "top"))
    
    full <- function() {
      print(mainplot)
      theme_set(theme_bw(base_size = 8))
      print(subplot, vp = vp)
      theme_set(theme_bw())
    }
    
    png(filename = paste0("/fs/data3/ecowdery/ED.Hydro/run_results/pft/ED_tropical_hydro/ma_",traitnames[i],"_BC.png"), width = 1200, height = 600)
    full()
    dev.off()
    
  }
}
