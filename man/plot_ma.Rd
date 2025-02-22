% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ma.R
\name{plot_ma}
\alias{plot_ma}
\title{MA plot of estimates vs ground truth}
\usage{
plot_ma(data, x, y, ..., extra_ellipsis = NULL)
}
\arguments{
\item{data}{data Data in longer format containing the mapping variables.}

\item{x}{Bare variable name of the ground truth, i.e. spiked-in proportion.}

\item{y}{Bare variable name of the estimated proportions.}

\item{...}{Pass additional mappings to \code{ggplot} like colour, symbol, size etc. using bare variable names.}

\item{extra_ellipsis}{expression(s) (don't use ” or "", just quote or rlang::expr) that
will be evaluated in order to modify the plot (e.g. add new geom, change
axis labels etc.).}
}
\description{
MA plot of estimates vs ground truth
}
\examples{
if (!require(tidyverse)) {
  library(tidyverse)
}
dat <- results_MS_spectrum \%>\%
  filter(spectrum_type == "imp") \%>\%
  distinct(z, ground_truth, scaling_factor)
plot_ma(dat, x = ground_truth, y = scaling_factor, colour = factor(z))
}
