#' MA plot of estimates vs ground truth
#'
#' @param data data Data in longer format containing the mapping variables.
#' @param x Bare variable name of the ground truth, i.e. spiked-in proportion.
#' @param y Bare variable name of the estimated proportions.
#' @param ... Pass additional mappings to \code{ggplot} like colour, symbol, size etc. using bare variable names.
#' @param extra_ellipsis expression(s) (don't use ” or "", just quote or rlang::expr) that
#'   will be evaluated in order to modify the plot (e.g. add new geom, change
#'   axis labels etc.).
#'
#' @export
#'
#' @examples
#' if (!require(tidyverse)) {
#'   library(tidyverse)
#' }
#' dat <- results_MS_spectrum %>%
#'   filter(spectrum_type == "imp") %>%
#'   distinct(z, ground_truth, scaling_factor)
#' plot_ma(dat, x = ground_truth, y = scaling_factor, colour = factor(z))
plot_ma <- function(data, x, y, ..., extra_ellipsis = NULL) {
  gg_dat <- data %>%
    tidyr::drop_na() %>%
    filter({{x}} > 0 & {{y}} > 0) %>%
    mutate(`log2Ratio` = log2({{y}} / {{x}}),
           `log2Avg` = .5 * (log2({{x}}) + log2({{y}})))
  p <- gg_dat %>%
    ggplot(aes(x = `log2Avg`, y = `log2Ratio`, ...)) +
    geom_point() +
    geom_hline(yintercept = 0.0, color = "black") +
    geom_hline(yintercept = 1.0, color = "black", linetype = "dotted") +
    geom_hline(yintercept = -1.0, color = "black", linetype = "dotted") +
    scale_x_continuous(breaks = pretty_breaks(n = 8)) +
    scale_y_continuous(breaks = pretty_breaks(n = 8)) +
    theme_bw()
  if (!is.null(extra_ellipsis)) {
    for (i in seq_along(extra_ellipsis)) {
      p <- p + eval(extra_ellipsis[[i]])
    }
  }
  p
}
