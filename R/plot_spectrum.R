#' Flexible spectrum plot
#'
#' A flexible wrapper on ggplot2 for plotting spectra that allows for: \itemize{
#' \item presenting single/multiple spectra in stick or profile mode \item
#' static or interactive display \item rescaling isotope distributions according
#' to the estimated mixing proportions \item stacking on each other selected
#' spectra \item facetting }
#'
#' @param data Data in longer format containing the mapping variables.
#' @param ... Pass graph's mapping to ggplot: x, y, colour, etc. The x, y,
#'   colour are \bold{required mappings}.
#' @param mode A character string, "profile" (default) or "stick".
#' @param interactive logical, TRUE will generate plotly-based graph, FALSE (the
#'   default) results in static visualisation.
#' @param jitter logical, TRUE separates visually different traces by a adding
#'   small, fixed number.
#' @param which_stacked if not NULL (the default), providing character vector
#'   like c("template1", "template2") stacks these spectra on each other.
#' @param residuals_as_points logical, TRUE will present the "residual" trace of
#'   the colour variable as red dots.
#' @param rev_xaxis logical, FALSE (the default) will plot x-axis as-is, TRUE
#'   will reverse it.
#' @param extra_ellipsis A list of expressions (don't use '' or "", just quote
#'   or rlang::expr) that will be evaluated in order to modify the plot (e.g.
#'   add new geom, change axis labels etc.).
#' @param scaling_factor logical, if TRUE you first need to run
#'   add_scaling_column() on the input data. Selected spectra will be rescaled
#'   appropriately.
#'
#' @export
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import stringi
#' @import stringr
#' @importFrom scales pretty_breaks
#' @importFrom plotly ggplotly
#' @examples
#' # select a single isotope distribution
#' dat <- subset(results_MS_spectrum, sample == "prop19.6_id3" & z == 9)
#'
#' # interactive plot
#' plot_spectrum(dat,
#'   x = mz, y = intensity, colour = spectrum_type,
#'   mode = "stick", interactive = TRUE, residuals_as_points = TRUE,
#'   rev_xaxis = FALSE
#' )
#'
#' # now with jitter
#' plot_spectrum(dat,
#'   x = mz, y = intensity, colour = spectrum_type,
#'   mode = "stick", interactive = TRUE, residuals_as_points = TRUE, jitter = TRUE,
#'   rev_xaxis = FALSE
#' )
#'
#' # add scaling_factor (remember that the \code{add_scaling_column} function has been run on
#' # the input data beforehand)
#' plot_spectrum(dat,
#'   x = mz, y = intensity, colour = spectrum_type,
#'   mode = "stick", interactive = TRUE, residuals_as_points = TRUE, jitter = TRUE,
#'   rev_xaxis = FALSE, scaling_factor = TRUE
#' )
#'
#' # add stacking
#' plot_spectrum(dat,
#'   x = mz, y = intensity, colour = spectrum_type,
#'   mode = "stick", interactive = TRUE, residuals_as_points = TRUE, jitter = TRUE,
#'   rev_xaxis = FALSE, scaling_factor = TRUE, which_stacked = c("flp", "imp")
#' )
#'
#' # add facetting via the extra_ellipsis argument
#' dat2 <- subset(results_MS_spectrum, z == 9)
#' plot_spectrum(dat2,
#'   x = mz, y = intensity, colour = spectrum_type,
#'   mode = "stick", interactive = TRUE, residuals_as_points = TRUE, jitter = TRUE,
#'   rev_xaxis = FALSE, scaling_factor = TRUE, which_stacked = c("flp", "imp"),
#'   extra_ellipsis = list(quote(facet_wrap(vars(sample))))
#' )
plot_spectrum <- function(data, ..., mode = "profile", interactive = FALSE, jitter = FALSE, which_stacked = NULL, residuals_as_points = FALSE, rev_xaxis = TRUE, extra_ellipsis = NULL, scaling_factor = FALSE) {
  # pass ... to aes to throw an error if the user provided incorrect mapping
  p_fake <- data %>% ggplot(aes(...))
  aes_vars_sym <- map(p_fake$mapping, ~ rlang::quo_get_expr(.x) %>% rlang::sym())
  aes_vars_char <- map_chr(aes_vars_sym, rlang::as_string)
  if (scaling_factor) {
    data <- data %>% mutate(!!aes_vars_sym$y := !!aes_vars_sym$y * scaling_factor)
  }
  colour_var_values <- unique(data[[aes_vars_char["colour"]]])
  resid_match <- pmatch("residuals", colour_var_values)
  colour_var_values_out <- ifelse(is.na(resid_match), "", colour_var_values[resid_match])
  if (residuals_as_points) {
    resids <- data %>% filter(!!aes_vars_sym$colour == colour_var_values_out)
    data <- data %>% filter(!!aes_vars_sym$colour != colour_var_values_out)
  }
  data_dodge <- data %>%
    filter(!(!!aes_vars_sym$colour %in% which_stacked)) %>%
    mutate(ymin = 0, ymax = !!aes_vars_sym$y)
  data_stacked <- data %>%
    filter(!!aes_vars_sym$colour %in% which_stacked) %>%
    group_by(!!aes_vars_sym$x) %>%
    mutate(ymax = cumsum(!!aes_vars_sym$y), ymin = lag(ymax, default = 0))
  data <- bind_rows(data_dodge, data_stacked)
  if (jitter) {
    x_vals <- data[[aes_vars_char["x"]]]
    x_vals_d <- diff(x_vals)
    x_delta <- min(abs(x_vals_d[abs(x_vals_d) > 0]))
    data[[str_c(aes_vars_char["colour"], "_tmp")]] <- ifelse(data[[aes_vars_char["colour"]]] %in% which_stacked, "stacked", data[[aes_vars_char["colour"]]])
    colour_var_values <- unique(data[[str_c(aes_vars_char["colour"], "_tmp")]])
    if (residuals_as_points | is.na(resid_match)) n_jitter_groups <- length(colour_var_values) else n_jitter_groups <- length(colour_var_values) - 1
    x_delta <- 0.8 * x_delta / n_jitter_groups
    # treat the spectra specified by "which_stacked" as one spectrum
    # the first spectrum, and residual spectrum won't be shifted
    tmp_df <- tibble(
      type = setdiff(colour_var_values, colour_var_values_out),
      jitter_value = x_delta * (0:(n_jitter_groups - 1) / n_jitter_groups)
    )
    data <- data %>%
      left_join(tmp_df, by = set_names("type", str_c(aes_vars_char["colour"], "_tmp"))) %>%
      mutate(jitter_value = ifelse(is.na(jitter_value), 0, jitter_value))
    data <- data %>% mutate(!!aes_vars_sym$x := !!aes_vars_sym$x + jitter_value)
    resid_jitter <- mean(unique(data$jitter_value))
    if (residuals_as_points) {
      resids <- resids %>% mutate(!!aes_vars_sym$x := !!aes_vars_sym$x + resid_jitter)
    } else {
      data <- data %>% mutate(!!aes_vars_sym$x := case_when(
        !!aes_vars_sym$colour == "residuals" ~ !!aes_vars_sym$x + resid_jitter,
        TRUE ~ !!aes_vars_sym$x
      ))
    }
  }
  # plotting
  p <- data %>% ggplot(aes(...))
  if (!is.null(aes_vars_char["colour"])) {
    n_colour <- length(unique(data[[aes_vars_char["colour"]]]))
    if (n_colour >= 1 & n_colour <= 9) p <- add_deconvolution_palette(p)
  }
  mode_arg <- match.arg(mode, c("profile", "stick"))
  if (mode_arg == "profile" & !is.null(which_stacked)) warning("Stacking data not possible in profile mode; change to the stick mode.")
  if (mode_arg == "profile") {
    p <- p + geom_line()
  } else {
    p <- p + geom_segment(aes(xend = .data[[aes_vars_char["x"]]], y = ymin, yend = ymax))
  }
  if (residuals_as_points) {
    p <- p + geom_point(data = resids, col = "red")
  }
  p <- p + theme_bw() +
    scale_y_continuous(breaks = pretty_breaks(n = 8))
  if (rev_xaxis) {
    p <- p + scale_x_reverse(breaks = pretty_breaks(n = 8))
  } else {
    p <- p + scale_x_continuous(breaks = pretty_breaks(n = 8))
  }
  if (!is.null(extra_ellipsis)) {
    for (i in seq_along(extra_ellipsis)) {
      p <- p + eval(extra_ellipsis[[i]])
    }
  }
  if (!interactive) p else ggplotly(p)
}
