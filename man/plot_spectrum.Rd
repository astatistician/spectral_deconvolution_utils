% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_spectrum.R
\name{plot_spectrum}
\alias{plot_spectrum}
\title{Flexible spectrum plot}
\usage{
plot_spectrum(
  data,
  ...,
  mode = "profile",
  interactive = FALSE,
  jitter = FALSE,
  which_stacked = NULL,
  residuals_as_points = FALSE,
  rev_xaxis = TRUE,
  extra_ellipsis = NULL,
  scaling_factor = FALSE
)
}
\arguments{
\item{data}{Data in longer format containing the mapping variables.}

\item{...}{Pass graph's mapping to ggplot: x, y, colour, etc. The x, y,
colour are \bold{required mappings}.}

\item{mode}{A character string, "profile" (default) or "stick".}

\item{interactive}{logical, TRUE will generate plotly-based graph, FALSE (the
default) results in static visualisation.}

\item{jitter}{logical, TRUE separates visually different traces by a adding
small, fixed number.}

\item{which_stacked}{if not NULL (the default), providing character vector
like c("template1", "template2") stacks these spectra on each other.}

\item{residuals_as_points}{logical, TRUE will present the "residual" trace of
the colour variable as red dots.}

\item{rev_xaxis}{logical, FALSE (the default) will plot x-axis as-is, TRUE
will reverse it.}

\item{extra_ellipsis}{A list of expressions (don't use '' or "", just quote
or rlang::expr) that will be evaluated in order to modify the plot (e.g.
add new geom, change axis labels etc.).}

\item{scaling_factor}{logical, if TRUE you first need to run
add_scaling_column() on the input data. Selected spectra will be rescaled
appropriately.}
}
\description{
A flexible wrapper on ggplot2 for plotting spectra that allows for: \itemize{
\item presenting single/multiple spectra in stick or profile mode \item
static or interactive display \item rescaling isotope distributions according
to the estimated mixing proportions \item stacking on each other selected
spectra \item facetting }
}
\examples{
# select a single isotope distribution
dat <- subset(results_MS_spectrum, sample == "prop19.6_id3" & z == 9)

# interactive plot
plot_spectrum(dat,
  x = mz, y = intensity, colour = spectrum_type,
  mode = "stick", interactive = TRUE, residuals_as_points = TRUE,
  rev_xaxis = FALSE
)

# now with jitter
plot_spectrum(dat,
  x = mz, y = intensity, colour = spectrum_type,
  mode = "stick", interactive = TRUE, residuals_as_points = TRUE, jitter = TRUE,
  rev_xaxis = FALSE
)

# add scaling_factor (remember that the \code{add_scaling_column} function has been run on
# the input data beforehand)
plot_spectrum(dat,
  x = mz, y = intensity, colour = spectrum_type,
  mode = "stick", interactive = TRUE, residuals_as_points = TRUE, jitter = TRUE,
  rev_xaxis = FALSE, scaling_factor = TRUE
)

# add stacking
plot_spectrum(dat,
  x = mz, y = intensity, colour = spectrum_type,
  mode = "stick", interactive = TRUE, residuals_as_points = TRUE, jitter = TRUE,
  rev_xaxis = FALSE, scaling_factor = TRUE, which_stacked = c("flp", "imp")
)

# add facetting via the extra_ellipsis argument
dat2 <- subset(results_MS_spectrum, z == 9)
plot_spectrum(dat2,
  x = mz, y = intensity, colour = spectrum_type,
  mode = "stick", interactive = TRUE, residuals_as_points = TRUE, jitter = TRUE,
  rev_xaxis = FALSE, scaling_factor = TRUE, which_stacked = c("flp", "imp"),
  extra_ellipsis = list(quote(facet_wrap(vars(sample))))
)
}
