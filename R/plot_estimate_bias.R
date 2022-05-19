#' Visualise Bias of Estimates
#'
#' Create scatterplot presenting difference between estimates and ground truth,
#' on absolute, relative, or multiplicatve scale.
#'
#' @param data Data in longer format containing the mapping variables.
#' @param x Bare variable name of the ground truth, i.e. spiked-in proportion.
#' @param y Bare variable name of the estimated proportions.
#' @param error_type character, \code{"diff"} shows y-x, \code{"relative diff"}
#'   100*(y-x)/x, \code{"multiplicative err"} y/x
#' @param ... Pass additional mappings to \code{ggplot} like colour, symbol, size etc. using bare
#'   variable names.
#' @param replicates logical, set to TRUE if there are replicate spectra per
#'   given spiked-in proportion; as a result, points will be drawn instead of
#'   lines.
#' @param smooth logical, add smoothed line using \code{ggplot2::geom_smooth}?
#' @param extra_ellipsis expression(s) (don't use ” or "", just quote or
#'   rlang::expr) that will be evaluated in order to modify the plot (e.g. add
#'   new geom, change axis labels etc.).
#' @export
#'
#' @examples
#' if (!require(tidyverse)) {
#'   library(tidyverse)
#' }
#' dat <- results_MS_spectrum %>%
#'   filter(spectrum_type == "imp") %>%
#'   distinct(sample, z, ground_truth, scaling_factor) %>%
#'   mutate(z = as.factor(z))
#' # "absolute error", replicated concentrations, smoother line
#' plot_estimate_bias(data = dat, x = ground_truth, y = scaling_factor, error_type = "diff", colour = z, replicates = TRUE, smooth = TRUE)

plot_estimate_bias <- function(data, x, y, error_type, ..., replicates = FALSE, smooth = FALSE, extra_ellipsis = NULL) {
  error_match <- match.arg(error_type, c("diff", "relative diff", "multiplicative err"))
  if (error_match == "diff") {
    eval_expr <- expr({{y}}-{{x}})
  } else if (error_match == "relative diff") {
    eval_expr <- expr(ifelse({{x}} != 0, 100*({{y}}-{{x}})/{{x}}, NaN))
  } else if (error_match == "multiplicative err") {
    eval_expr <- expr(ifelse({{x}} != 0, {{y}}/{{x}}, NaN))
  }
  data["err"] <- rlang::eval_tidy(eval_expr, data)
  line_y_intercept <- ifelse(error_match == "multiplicative err", 1, 0)
  p <- data %>% ggplot(aes(x = {{x}}, y = err, ...)) +
    geom_point(size = 3)
  if (!replicates) p <- p + geom_line()
  p <- p +
    geom_hline(yintercept = line_y_intercept, linetype="dashed") +
    theme_bw() +
    scale_x_continuous(breaks = pretty_breaks(n = 8)) +
    scale_y_continuous(breaks = pretty_breaks(n = 8)) +
    labs(x = "true proportion", y = error_match)
  if (!is.null(pluck(p$mapping, "colour"))) {
    colour_var <- rlang::quo_get_expr(p$mapping["colour"][[1]]) %>% rlang::sym() %>% rlang::as_string()
    if (length(unique(pull(data, colour_var))) <=9 & class(pull(data, colour_var)) %in% c("character", "factor"))
      p <- add_deconvolution_palette(p)
  }
  if (smooth) p <- p + geom_smooth(aes(x = {{x}}, y = err, colour = NULL), size = 1.2, se = FALSE)
  if (!is.null(extra_ellipsis)) {
    for (i in seq_along(extra_ellipsis)) {
      p <- p + eval(extra_ellipsis[[i]])
    }
  }
  p
}
