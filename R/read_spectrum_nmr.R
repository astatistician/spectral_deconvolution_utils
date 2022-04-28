#' Read in NMR spectrum
#'
#' Vectorized function to read in multiple NMR spectra (acquired using Bruker's
#' equipment) at once.
#'
#' @param path If type = "spectrum", path should point at the folder with "1r"
#'   and "1i" files. If type = "fid", the path should be the folder with the
#'   "fid" file. type = "text" can not be handled currently.
#' @param type A character vector controlling how the data is loaded, see the
#'   \emph{path} argument.
#'
#' @return List with two elements \itemize{ \item \emph{data}: a tibble in long
#'   format with three variables: signal - signal id, x - ppm value, y - complex
#'   signal. \item \emph{info}: matrix with acquisition information (signal id,
#'   OFFSET, SW_p, SF, SI, BYTORDP, NC_proc, FTSIZE). One spectrum per row. }
#' @export
#'
#' @examples
#' path_spectrum <- system.file("extdata", "raw_NMR_spectrum/20/pdata/2", package = "deconvolutionutils")
#' head(read_spectrum_nmr(path_spectrum, type = "spectrum"))
#'
#' path_fid <- system.file("extdata", "raw_NMR_spectrum/20", package = "deconvolutionutils")
#' head(read_spectrum_nmr(path_fid, type = "fid"))
read_spectrum_nmr <- function(path, type) {
  type <- match.arg(type, c("spectrum", "fid", "text"))

  n_files <- length(path)
  info_out <- NULL
  data_out <- vector("list", n_files)

  if (type == "fid") {
    params <- c("TD", "BYTORDA", "DIGMOD", "DECIM", "DSPFVS", "SW_h", "SW", "O1", "GRPDLY")
    info_out <- matrix(nrow = n_files, ncol = length(params) + 1)
    colnames(info_out) <- c(params, "DW")

    for (i in 1:n_files) {
      x1 <- readLines(paste0(path[i], "/acqus"))
      x2 <- strsplit(x1, "=")
      x3 <- lapply(x2, function(x) {
        tmp <- x %>%
          stri_replace_all("", regex = "#*\\$*") %>%
          stri_trim_both()
      })

      params_all <- unlist(lapply(x3, function(x) x[1]))
      values_all <- unlist(lapply(x3, function(x) x[2]))
      params_ind <- match(params, params_all)
      values <- as.numeric(values_all[params_ind])
      values <- c(values, 1 / (2 * values[params == "SW_h"]))

      info_out[i, ] <- values
      endianness <- ifelse(info_out[, "BYTORDA"] != 0, "big", "little")

      fid <- readBin(paste0(path[i], "/fid"),
        what = "int", n = info_out[, "TD"],
        size = 4L, endian = endianness
      )

      fid <- complex(
        real = fid[seq(from = 1, to = info_out[i, "TD"], by = 2)],
        imaginary = fid[seq(from = 2, to = info_out[i, "TD"], by = 2)]
      )
      time_vec <- seq(0, (length(fid) - 1) * info_out[i, "DW"], by = info_out[i, "DW"])
      data_out[[i]] <- tibble(x = time_vec, y = fid)
    }
  } else if (type == "spectrum") {
    params <- c("OFFSET", "SW_p", "SF", "SI", "BYTORDP", "NC_proc", "FTSIZE")
    info_out <- matrix(nrow = n_files, ncol = length(params))
    colnames(info_out) <- c(params)

    for (i in 1:n_files) {
      x1 <- readLines(paste0(path[i], "/procs"))
      x2 <- strsplit(x1, "=")
      x3 <- lapply(x2, function(x) {
        tmp <- x %>%
          stri_replace_all("", regex = "#*\\$*") %>%
          stri_trim_both()
      })

      params_all <- unlist(lapply(x3, function(x) x[1]))
      values_all <- unlist(lapply(x3, function(x) x[2]))
      params_ind <- match(params, params_all)
      info_out[i, ] <- as.numeric(values_all[params_ind])

      nspec <- info_out[i, "SI"]
      swp <- info_out[i, "SW_p"] / info_out[i, "SF"]
      dppm <- swp / nspec
      ppm <- seq(info_out[i, "OFFSET"], (info_out[i, "OFFSET"] - swp), by = -dppm)
      # the ppm of last point may not coincide with the sequence right limit
      ppm <- ppm[1 : nspec]

      spec_r <- readBin(paste0(path[i], "/1r"),
        what = "int", n = nspec,
        size = 4L, endian = "little"
      )
      spec_i <- readBin(paste0(path[i], "/1i"),
        what = "int", n = nspec,
        size = 4L, endian = "little"
      )
      spec <- complex(real = spec_r, imaginary = spec_i)
      data_out[[i]] <- tibble(x = ppm, y = spec)
    }
  } else if (type == "text") {
    for (i in 1:n_files) {
      tmp_spec <- utils::read.table(path[i], sep = " ", header = FALSE)
      data_out[[i]] <- tibble(x = tmp_spec[, 1], y = tmp_spec[, 2])
    }
  }
  data_out <- set_names(data_out, paste0("signal", 1 : n_files))
  return(list(data = bind_rows(data_out, .id = "signal"),
              info = bind_cols(signal = paste0("signal", 1:n_files), info_out)))
}
