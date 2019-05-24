#' Plot that prior!
#'
#' @param prior
#' @param q
#' @param plot_default
#' @param title
#' @param type
#' @return ggplot object
#' @export



prior_plot <- function(prior, q = NA, plot_default = NA, title = "", type = "prior"){

  plot_df <- data.frame(var = prior, type = type)

  Set2colors <- RColorBrewer::brewer.pal(8, "Set1")
  names(Set2colors) <- c("red", "blue", "green", "purple", "orange", "yellow", "brown", "pink")

  dist_colors        <- c(Set2colors[c("green", "blue", "orange", "purple")], "gray")
  names(dist_colors) <- c("elic", "equation", "mixed", "obs", "prior")
  dist_colors

  line_colors <- Set2colors["blue"]
  names(line_colors) <- c("PFT3")

  dist_labels <- c("Based on elicitation", "Based on equations","Based on elicitation and equations", "Observed data", "Prior")
  names(dist_labels) <- c("elic", "equation", "mixed", "obs", "prior")

  p <- ggplot(plot_df) +
    geom_density(aes(x = var, fill = type), alpha = .3, color = NA) +
    ggtitle(title) +
    theme_classic() +
    scale_fill_manual(name = "",
                      values = dist_colors,
                      labels = dist_labels) +
    scale_color_manual(name = "",
                       values = line_colors ,
                       labels = "ED PFT3 Default")

  if(!all(is.na(q))){
    plot_range <- quantile(prior, q)
    p <- p + xlim(plot_range)
  }

  if(!is.na(plot_default)){
    p <- p + geom_vline(aes(xintercept = plot_default, color = "PFT3"), size = 1)
  }

  return(p)
}
