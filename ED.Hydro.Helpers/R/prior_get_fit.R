#' Get a prior fit, specifically from a priors dataframe
#'
#' @param prior_in one row of the priors dataframe
#' @param accepted_dists accepted dists
#' @param plot option to plot the fits
#' @return fit object
#' @export

prior_get_fit <- function(prior_in, accepted_dists, plot = FALSE){

  quants <- data.frame(
    name = c("theor.min", "low.025", "low.25", "mean", "upp.75", "upp.975", "theor.max"),
    quant = c(0, .025, .25, .5, .75, .975, 1),
    stringsAsFactors = FALSE)

  sub <- prior_in %>% select(one_of(quants$name)) %>% gather(key = name) %>% na.omit()

  #If there aren't any parameters, skip the variable
  if(dim(sub)[2] == 0){
    print("skipping")
    next
  }

  sub$value <- as.numeric(sub$value)
  sub2 <- left_join(sub, quants, by = "name")

  fitout <- suppressMessages(rriskDistributions::rriskFitdist.perc(p = sub2$quant, q = sub2$value,
                                                                    show.output = FALSE))

  results <- fitout$results %>%
    rename(Data = Quantiles) %>%
    mutate(Name = row.names(.)) %>%
    dplyr::select_if(~!all(is.na(.))) %>%
    select(one_of("Name", "Data", intersect(names(.), accepted_dists)))

  dists <- results  %>%
    na.omit() %>%
    filter(!Name == 100,!Name == 0)
  # print(dists)

  score <- apply(dists %>% select(-one_of("Name", "Data")), 2,
                function(x) sqrt(mean((dists$Data - x)^2))) %>%
    sort(decreasing = TRUE) %>%
    as.list %>% data.frame %>%
    gather(key = "dist", value = "RMSE")
  # print(score)

  dist_params <- results %>%
    select(-one_of("Data")) %>%
    filter(Name %in% c("Para1", "Para2", "Para3", "Para4"))
  # print(dist_params)

  if(plot){

    xval = seq(min(sub2$value),max(sub2$value),length=1000)

    pdists <- pdistn(xval = xval, dist_params = dist_params)

    pdists_plot <- pdists %>%
      gather(key = "dist", value = "value", -xval) %>%
      right_join(score, by = "dist")
    pdists_plot$dist <- factor(pdists_plot$dist, levels = c(score$dist))

    c_values <- score$RMSE / max(score$RMSE)
    c_select <- viridis::viridis_pal()(100)
    cols <- scales::gradient_n_pal(c_select)(c_values)
    names(cols) <- score$dist
    # scales::show_col(cols)
    colScale <-  scale_color_manual(name = "dist", values = cols)

    p <- ggplot(pdists_plot) +
      geom_hline(data = sub2, aes(yintercept = quant), linetype = "dashed") +
      geom_line(aes(x = xval, y = value, color = dist), size = 1.5) +
      geom_point(data = sub2, aes(x = value, y = quant), size = 3) +
      ylab("Quantile") +
      xlab("Value") +
      colScale +
      theme_classic() +
      annotation_custom(
        gridExtra::tableGrob(score, theme = gridExtra::ttheme_default(base_size = 10), rows=NULL),
        xmin = (2/3) * (max(xval)-min(xval)), xmax = max(xval),
        ymin=0, ymax=.5)

    plot(p)
  }

  myfit <- list(dists = dist_params, score = score)
  return(myfit)
}


# sub3 <- sub2 %>% filter(!value == 1, !value == 0)
#
# args <- list(
#   vals = sub3$value,
#   probs = sub3$quant
# )
#
# lower_test <- sub %>% filter(name == "theor.min")
# lower <- ifelse(nrow(lower_test), lower_test$value, NA)
# upper_test <- sub %>% filter(name == "theor.max")
# upper <- ifelse(nrow(upper_test), upper_test$value, NA)
#
# if(!is.na(lower)) args = append(args, list(lower = lower))
# if(!is.na(upper)) args = append(args, list(upper = upper))

###############################################
## Calculate fit and insert in to the database
## Supported distributions: Normal, Beta, Log Normal, Beta
## Distributions we want: chisq, exp, f, unif, weibull

# myfit <- do.call(SHELF::fitdist, args)
# sort(myfit$ssq)
# if(plot){
#   print("plotting")
#   lwd = 5*min(myfit$ssq,na.rm = TRUE)/myfit$ssq
#   xval = seq(min(sub2$value),max(sub2$value),length=1000)
#   plot(sub2$value, sub2$quant, main = prior_in$ED_name[i])
#
#   if(!is.na(myfit$Normal[1])){
#     lines(xval,pnorm(xval,myfit$Normal$mean,myfit$Normal$sd),col=2,lwd=lwd[1])
#   }
#   if(!is.na(myfit$Gamma[1])){
#     lines(xval,pgamma(xval,myfit$Gamma$shape,myfit$Gamma$rate),col=3,lwd=lwd[3])
#   }
#   if(!is.na(myfit$Log.normal[1])){
#     lines(xval,plnorm(xval,myfit$Log.normal$mean.log.X,myfit$Log.normal$sd.log.X),col=4,lwd=lwd[4])
#   }
#   if(!is.na(myfit$Beta[1])){
#     lines(xval,pbeta(xval,myfit$Beta$shape1,myfit$Beta$shape2),col=5,lwd=lwd[5])
#   }
#   # if(!is.na(myfit$Student.t[1])){
#   #   lines(xval,pt(xval,myfit$Student.t$location,myfit$Student.t$scale,myfit$Student.t$df),col=6,lwd=lwd[6])
#   # }
#   # if(!is.na(myfit$Log.Student.t[1])){
#   #   lines(xval,pt(xval,myfit$Log.Student.t$location.log.X,myfit$Log.Student.t$scale.log.X,myfit$Log.Student.t$df.log.X, log.p = TRUE),col=7,lwd=lwd[7])
#   # }
#   legend("topleft",legend=c("Normal","Gamma","Log.normal","Beta", "Student t", "Log Student t"),col=2:7,lwd=2)
# }
