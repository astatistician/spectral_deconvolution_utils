#' Visualise Bias of Estimates
#'
#' Create scatterplot presenting difference between estimates and ground truth,
#' on absolute, relative, or multiplicatve scale.
#'
#' @param x numeric, the ground truth (spiked) proportion/ratio values.
#' @param y vector or matrix with estimates (columns).
#' @param error_type character, \code{"diff"} shows y-x, \code{"relative diff"}
#'   100*(y-x)/x, \code{"multiplicative err"} y/x
#' @param ... to be documented
#' @param replicates logical, set to TRUE if there are replicate spectra per
#'   given spiked-in proportion; as a result, points will be drawn instead of
#'   lines.
#' @param smooth logical, add smoothed line using \code{ggplot2::geom_smooth}?
#'
#' @export
#'
#' @examples
#' #' if (!require(tidyverse)) {
#'   library(tidyverse)
#' }
#' # prepare data
#' dat <- results_MS_spectrum %>%
#'   filter(spectrum_type == "imp")
#' ground_truth <- dat %>% distinct(sample, ground_truth) %>% pull(ground_truth)
#' dat <- dat %>%
#'   distinct(sample, z, ground_truth, scaling_factor) %>%
#'   pivot_wider(id = sample, names_from = "z", values_from = scaling_factor, names_prefix = "z") %>%
#'   select(-sample)
#'
#' # "absolute error", replicated concentrations, smoother line
#' plot_estimate_bias(x = ground_truth, y = dat, error_type = "diff", replicates = TRUE, smooth = TRUE)

plot_estimate_bias <- function(x, y, error_type, ..., replicates = FALSE, smooth = FALSE) {
  y_org <- y
  y <- as_tibble(y)
  err_fun <- function(x,y, type){
    switch(type,
           "diff" = y-x,
           "relative diff" = ifelse(x != 0, 100*(y-x)/x, NaN),
           "multiplicative err" = ifelse(x != 0, y/x, NaN))
  }
  error_match <- match.arg(error_type, c("diff", "relative diff", "multiplicative err"))
  line_y_intercept <- ifelse(error_match == "multiplicative err", 1, 0)
  gg_dat <- bind_cols(x = x, y) %>%  pivot_longer(cols = -x, names_to = "group", values_to = "y") %>%
    mutate(err = err_fun(x, y, error_match), group = as_factor(group))
  p <- gg_dat %>% ggplot(aes(x = x, y = err)) +
    geom_point(aes(colour = group), size = 3)
  if (!replicates) p <- p + geom_line(aes(colour = group))
  p <- p +
    geom_hline(yintercept = line_y_intercept, linetype="dashed") +
    theme_bw() +
    scale_x_continuous(breaks = pretty_breaks(n = 8)) +
    scale_y_continuous(breaks = pretty_breaks(n = 8)) +
    labs(x = "true proportion", y = error_match)
  if (length(unique(gg_dat$group)) <=9) p <- add_deconvolution_palette(p)
  if (length(y) == 1) p <- p + theme(legend.position = "none")
  if (smooth) p <- p + geom_smooth(size = 1.2, se = FALSE)
  add_options <- list(...)
  for (i in seq_along(add_options)) {
    p <- p + eval(add_options[[i]])
  }
  p
}
