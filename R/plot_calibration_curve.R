#' Plot calibration curve
#'
#' @param x numeric, the ground truth (spiked) proportion/ratio values.
#' @param y numeric, estimated mixing proportions/ratios.
#' @param scale_in,scale_out character, possible values: \code{"proportion"} or
#'   \code{"ratio"}. Decide whether input should be transformed from proportions
#'   to ratios and vice versa.
#' @param ... expression(s) (don't use ” or "", just quote or rlang::expr) that
#'   will be evaluated in order to modify the plot (e.g. add new geom, change
#'   axis labels etc.).
#' @param intercept logical, should the fitted calibration line include an
#'   intercept?
#' @importFrom stats coef lm
#' @export
#'
#' @examples
#' if (!require(dplyr)) {
#'   library(dplyr)
#' }
#' dat <- results_MS_spectrum %>%
#'   filter(z == 9 & spectrum_type == "imp") %>%
#'   distinct(ground_truth, scaling_factor)
#' plot_calibration_curve(x = dat$ground_truth, y = dat$scaling_factor, scale_in = "proportion",
#' scale_out = "proportion")
plot_calibration_curve <- function(x, y, scale_in, scale_out, ..., intercept = TRUE) {
  if (length(y) != length(x)) stop("x and y vectors are not equally sized")
  if (scale_in == "proportion" & scale_out == "ratio") {
    signal_est <- y / (1 - y)
    signal_true <- x / (1 - x)
  } else if (scale_in == "ratio" & scale_out == "proportion") {
    signal_est <- 1 + 1 / y
    signal_true <- 1 + 1 / x
  } else {
    signal_est <- y
    signal_true <- x
  }
  x_lab <- str_c("true", scale_out, sep = " ")
  y_lab <- str_c("estimated", scale_out, sep = " ")
  gg_dat <- bind_cols(signal_est, signal_true)
  p <- gg_dat %>% ggplot(aes(x = signal_true, y = signal_est)) +
    geom_point() +
    theme_bw() +
    scale_x_continuous(breaks = pretty_breaks(n = 8)) +
    scale_y_continuous(breaks = pretty_breaks(n = 8)) +
    labs(x = x_lab, y = y_lab)
  if (intercept) mod <- lm(signal_est ~ signal_true) else mod <- lm(signal_est ~ -1 + signal_true)
  cf <- coef(mod)
  summ <- summary(mod)
  legend_txt1 <- paste(paste0(quote("R^2="), round(summ$r.squared, 4)), "\n",
    paste0(quote("sigma="), round(summ$sigma, 5)),
    collapse = " "
  )
  cf_sign <- ifelse(sign(cf) < 0, "-", "+")
  cf_abs <- abs(cf)
  legend_txt2 <- paste0("y = ", ifelse(intercept,
    paste0(round(coef(mod)[2], 3), "x", " ", cf_sign[1], " ", round(cf_abs[1], 3)),
    paste0(round(coef(mod)[1], 3), "x")
  ))
  range_x <- range(signal_true)
  range_y <- range(signal_est)
  p <- p + geom_text(x = range_x[1] + diff(range_x) * 0.15, y = range_y[2] - diff(range_y) * 0.15, label = legend_txt2) +
    geom_text(x = range_x[2] - diff(range_x) * 0.15, y = range_y[1] + diff(range_y) * 0.15, label = legend_txt1) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

  add_options <- list(...)
  for (i in seq_along(add_options)) {
    p <- p + eval(add_options[[i]])
  }
  p
}
