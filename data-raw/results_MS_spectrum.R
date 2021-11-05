## code to prepare `results_MS_spectrum` dataset goes here
library(tidyverse)
results_MS_spectrum <- read.csv(here::here("inst/extdata/results_MS_spectrum.csv")) %>%
  select(-X) %>%
  as.data.frame()
usethis::use_data(results_MS_spectrum, overwrite = TRUE)
