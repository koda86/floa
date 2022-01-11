# ******************************************************************************
# *****************    Main script prediction bands    *************************
#
# Author: Daniel Koska
# Date: December 2021
# R version: 4.0.5
# Platform: x86_64-pc-linux-gnu
#
# This script accompanies the publication:

# Please cite: Koska, D., Oriwol, D. & Maiwald, C. (2022) Methodological aspects
# for the construction of continuous prediction intervals in method comparisons.
# DOI: ...
#
# Two important variables should be adjusted before running the script:
# 1. The data set (in example_data()
# 2. The number of bootstrap iterations (n.boot)
# ******************************************************************************

rm(list = ls())

library(ggplot2)

dir.script <- "~/floa/R"
dir.data <- "~/floa/R/examples"

setwd(dir.script)

source("example_data.R")
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

# Data sets are created in 'simulate_data.R'
#
# Wrapper function for example data sets. Function arguments:
#
# - Smooth wave data (normal error): "smooth_realistic"
# - Data with non-gaussian (Weibull distributed) error: "non_gaussian"
# - Phase shifted data (x-axis direction): "shift"
# - Real world empirical validation data: "imu_mc"
data <- example_data(dat = "smooth_realistic", dir.data)



# Prediction bands -------------------------------------------------------------

n.boot <- 400

floa.point     <- floa_point(data)
floa.roislien  <- floa_roislien(data)
floa.boot.rep  <- floa_boot(data,
                            k.coef = 50,
                            n.boot = n.boot,
                            band = "prediction",
                            cp.begin = 0,
                            alpha = 0.05,
                            iid = FALSE) # Draw all curves
floa.boot.iid  <- floa_boot(data,
                            k.coef = 50,
                            n.boot = n.boot,
                            band = "prediction",
                            cp.begin = 0,
                            alpha = 0.05,
                            iid = TRUE) # Draw only a single curve per subject

plot_loa(data, floa.point, floa.roislien, floa.boot.rep, floa.boot.iid, ylim = c(-5, 5))



# Coverage ---------------------------------------------------------------------

#### 1. Percentage of fully covered curves
cover.cross.singlecurve <- singlecurve_coverage(data, n.boot)

calculate_percent <- function(data) {
  # Calculate percentage from counts
  percentage <- (sum(data) / length(data))
  percentage <- round(percentage, digits = 2)
}

# Order of output: POINT, ROISLIEN, BOOTrep, BOOTiid
covered.curves.percent <- apply(cover.cross.singlecurve, 2, calculate_percent)

#### 2. Percentage of covered points
n.strides <- length(unique(data$strideID))

cover.cross.fraction.singlecurve <- singlecurve_coverage_fraction(data, n.boot)

# Get percentile to summarize the distribution
apply(cover.cross.fraction.singlecurve, 2, quantile, probs = c(0, 0.025, 0.1, 0.25, 0.5))



# Uncertainty estimation -------------------------------------------------------

n.rep <- 100

# Order of output: POINT, ROISLIEN, BOOT.all, BOOT.iid
uncertainty <- estimate_uncertainty_loa(data, n.rep, n.boot)


