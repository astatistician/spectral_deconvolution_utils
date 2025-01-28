
# deconvolutionutils

A Toolbox for NMR and MS Spectra Visualization

## Overview

deconvolutionutils is an R package designed to simplify the visualization of NMR (Nuclear Magnetic Resonance) and MS (Mass Spectrometry) data analysis. It provides a suite of convenience functions for reading in the data and plotting the molecules' estimated concentrations versus the expected proportions.

## Installation

To install the package from GitHub, use the following commands in R:

```R
# Install the devtools package if not already installed
install.packages("devtools")

# Install deconvolutionutils from GitHub
devtools::install_github("astatistician/deconvolutionutils")
```

## Functions

The package includes the following functions:

1. **read_spectrum_ms**  
   Read a single MS spectrum from a `.csv` (stick data) or `.mzXML` (profile data) file.

2. **read_spectrum_nmr**  
   Vectorized function to read multiple NMR spectra (acquired using Bruker's equipment) at once.

3. **results_MS_spectrum**  
   A deconvolution results dataset created to showcase how to call various plotting functions.

4. **plot_spectrum**  
   A flexible wrapper on ggplot2 for plotting spectra, allowing for:
   - Presenting single/multiple spectra in stick or profile mode.
   - Static or interactive display.
   - Rescaling isotope distributions according to estimated mixing proportions.
   - Stacking selected spectra on each other.
   - Faceting.
   
5. **plot_ma**  
   Generate an MA plot of estimates vs. ground truth.

6. **plot_calibration_curve**  
   Plot a calibration curve.

7. **plot_estimate_bias**  
   Create a scatterplot showing the difference between estimates and ground truth on absolute, relative, or multiplicative scales.

8. **add_deconvolution_palette**  
   Supply distinctive colors to a graph with overlapping signals.

## Usage

Refer to the package documentation for examples and further details on how to use each function effectively.

---
