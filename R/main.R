# ------------------------------------------------------------------------------
# -----------------    Main script prediction bands    -------------------------
#
# Author: Daniel Koska
# December 2021
#
# This script accompanies the publication: DOI
#
# Please cite: Koska, D., Oriwol, D. & Maiwald, C. (2022) Methodische Aspekte der
# Konstruktion von Pradiktionsbändern in Methodenvergleich
# ------------------------------------------------------------------------------

rm(list = ls())

library(ggplot2)

dir.script <- "~/floa/R"
dir.data <- "~/floa/R/examples"

setwd(dir.script)

source("example_data.R")
source("fdaDelta.R")
source("pick_subwise_curves.R")
source("floa_boot.R")
source("floa_point.R")
source("floa_roislien.R")
source("plot_loa.R")
source("get_coverage_fraction.R")
source("get_coverage_singlecurve.R")
source("get_coverage_singlecurve_fraction.R")
source("singlecurve_coverage.R")
source("singlecurve_coverage_fraction.R")
source("estimate_uncertainty_loa.R")

# Data sets --------------------------------------------------------------------

# Wrapper function for example data sets. Function arguments:
#
# * Real world empirical validation data: "imu_mc"
# * Smooth wave data (normal error, no trend): "smooth_realistic"
# * Heteroscedastic errors (no trend): "heteroscedastic"
# * Data with non-gaussian (Weibull distributed) error (no trend): "non_gaussian"
# * Phase shifted data (x-axis direction): "shift"
data <- example_data(dat = "smooth_realistic", dir.data)


# Prediction bands -------------------------------------------------------------

n.boot <- 400

floa.point     <- floa_point(data)
floa.roislien  <- floa_roislien(data)
floa.boot      <- floa_boot(data,
                            k.coef = 50,
                            n.boot = n.boot,
                            band = "prediction",
                            cp.begin = 0,
                            alpha = 0.05,
                            iid = FALSE)

plot_loa(data, floa.point, floa.roislien, floa.boot, ylim = c(-5, 5))


# Coverage ---------------------------------------------------------------------

# 1. Percentage of fully covered curves
cover.cross.singlecurve <- singlecurve_coverage(data, n.boot)

# Calculate percentage from counts
calculate_percent <- function(data) {
  percentage <- (sum(data) / length(data))
  percentage <- round(percentage, digits = 2)
}

# Order of output: POINT, ROISLIEN, BOOT
covered.curves.percent <- apply(cover.cross.singlecurve, 2, calculate_percent)

# 2. Percentage of covered points
n.strides <- length(unique(data$strideID))

cover.cross.fraction.singlecurve <- singlecurve_coverage_fraction(data, n.boot)
covered.curves.fraction.percent <- apply(cover.cross.singlecurve, 2, calculate_percent)


# Uncertainty estimation -------------------------------------------------------

n.rep <- 100

# Order of output: POINT, ROISLIEN, BOOT.all, BOOT.iid
uncertainty <- estimate_uncertainty_loa(data, n.rep, n.boot)


