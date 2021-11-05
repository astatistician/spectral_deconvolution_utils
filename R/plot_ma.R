#' MA plot of estimates vs ground truth
#'
#' @param x numeric, the ground truth (spiked) proportion/ratio values.
#' @param y numeric, estimated mixing proportions/ratios.
#' @param ... expression(s) (don't use ” or "", just quote or rlang::expr) that
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
#'   filter(z == 9 & spectrum_type == "imp") %>%
#'   distinct(ground_truth, scaling_factor)
#' plot_ma(x = dat$ground_truth, y = dat$scaling_factor)
plot_ma <- function(x, y, ...) {
  gg_dat <- bind_cols(x = x, y = y) %>%
    tidyr::drop_na() %>%
    filter(x > 0 & y > 0) %>%
    mutate(`log2Ratio` = log2(y / x), `log2Avg` = .5 * (log2(x) + log2(y)))
  p <- gg_dat %>%
    ggplot(aes(x = `log2Avg`, y = `log2Ratio`)) +
    geom_point() +
    geom_hline(yintercept = 0.0, color = "black") +
    geom_hline(yintercept = 1.0, color = "black", linetype = "dotted") +
    geom_hline(yintercept = -1.0, color = "black", linetype = "dotted") +
    scale_x_continuous(breaks = pretty_breaks(n = 8)) +
    scale_y_continuous(breaks = pretty_breaks(n = 8)) +
    theme_bw()
  add_options <- list(...)
  for (i in seq_along(add_options)) {
    p <- p + eval(add_options[[i]])
  }
  p
}
