#' Read in MS spectrum
#'
#' Read in single MS spectrum from a .csv (stick data) or .mzXML (profile data) file.
#'
#' @param path A character string of length 1 containing input file path.
#' @param mode "stick" or "profile" depending on the input data.
#' @param do_norm If TRUE, sum-to-one normalization will be applied.
#'
#' @return Returns a tibble with two columns: mz and intensity.
#' @export
#' @importFrom magrittr `%>%`
#' @examples path <- system.file("extdata", "raw_MS_spectrum.csv", package = "deconvolutionutils")
#' head(read_spectrum_ms(path, mode = "stick"))
read_spectrum_ms <- function(path, mode, do_norm = FALSE) {
  arg <- match.arg(mode, c("stick", "profile"))
  if (arg == "stick") {
    data <- utils::read.csv(path)
  } else if (arg == "profile") {
    data <- utils::read.table(path, header = T)
  }
  data <- data %>% dplyr::rename(mz = m.z)
  if (do_norm) data$intensity <- data$intensity / sum(data$intensity, na.rm = TRUE)
  return(data)
}
