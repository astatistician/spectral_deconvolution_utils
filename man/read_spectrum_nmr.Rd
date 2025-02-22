% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_spectrum_nmr.R
\name{read_spectrum_nmr}
\alias{read_spectrum_nmr}
\title{Read in NMR spectrum}
\usage{
read_spectrum_nmr(path, type)
}
\arguments{
\item{path}{If type = "spectrum", path should point at the folder with "1r"
and "1i" files. If type = "fid", the path should be the folder with the
"fid" file. type = "text" can not be handled currently.}

\item{type}{A character vector controlling how the data is loaded, see the
\emph{path} argument.}
}
\value{
List with two elements \itemize{ \item \emph{data}: a tibble in long
format with three variables: signal - signal id, x - ppm value, y - complex
signal. \item \emph{info}: matrix with acquisition information (signal id,
OFFSET, SW_p, SF, SI, BYTORDP, NC_proc, FTSIZE). One spectrum per row. }
}
\description{
Vectorized function to read in multiple NMR spectra (acquired using Bruker's
equipment) at once.
}
\examples{
path_spectrum <- system.file("extdata", "raw_NMR_spectrum/20/pdata/2", package = "deconvolutionutils")
head(read_spectrum_nmr(path_spectrum, type = "spectrum"))

path_fid <- system.file("extdata", "raw_NMR_spectrum/20", package = "deconvolutionutils")
head(read_spectrum_nmr(path_fid, type = "fid"))
}
