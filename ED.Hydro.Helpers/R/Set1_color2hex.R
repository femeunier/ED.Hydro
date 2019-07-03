#' Simple function to get the hex code for my favorite colors
#'
#' @param color_in
#' @export

Set1_color2hex <- function(color_in){

  Set1 <- as.data.frame(rbind(
    c("red" , "#e01d1b"),
    c("blue", "#3073af"),
    c("green", "#43a541"),
    c("purple", "#8d4499"),
    c("orange", "#ff7304"),
    c("yellow", "#fffa2d"),
    c("brown", "#9c4c24"),
    c("pink", "#f676b7"),
    c("grey", "#8e8e8e")),
    stringsAsFactors = FALSE)
  names(Set1) <- c("color", "hex")

  hex_out <- Set1 %>% filter(color == color_in) %>% pull(hex)
  return(hex_out)

}


