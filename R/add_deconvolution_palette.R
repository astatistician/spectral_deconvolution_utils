#' Supply distinctive colours to a graph
#' @param gg_plot A ggplot graph.
#' @param return_palette Logical, TRUE will return the palette only, FALSE (the default) will return the modified graph.
#' @importFrom RColorBrewer brewer.pal
add_deconvolution_palette <- function(gg_plot, return_palette = FALSE) {
  my_pal <- c("#000000", brewer.pal(8, "Dark2"))
  if (return_palette) {
    return(my_pal)
  } else {
    gg_plot <- gg_plot + scale_colour_manual(values = my_pal)
    return(gg_plot)
  }
}
