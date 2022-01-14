# ******************************************************************************
# *****************    Main script prediction bands    *************************
# ------------------------------------------------------------------------------
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
# Variables that should be adjusted before running the script:
# - The data and script directories ('dir.data', 'dir.script')
# - The data set (in example_data())
# - The number of bootstrap iterations (n.boot)
# - The number of repeated calculations for the uncertainty estimation (n.rep)
# ******************************************************************************

rm(list = ls())

dir.script <- "~/floa/R"
dir.data <- "~/floa/R/examples"

setwd(dir.script)

library(ggplot2)

source("example_data.R")
source("pick_subwise_curves.R")
source("floa_boot.R")
source("floa_point.R")
source("floa_roislien.R")
source("plot_loa.R")
source("points_within_limits.R")
source("coverage_loocv.R")
source("coverage_curves.R")
source("estimate_uncertainty_loa.R")


# ------------------------------------------------------------------------------
# Data sets
# ------------------------------------------------------------------------------

# Data sets originally created in 'simulate_data.R'
#
# Wrapper function for example data sets. Function arguments:
#
# - Smooth wave data (normal error): "smooth_realistic"
# - Data with non-gaussian (Weibull distributed) error: "non_gaussian"
# - Phase shifted data (x-axis direction): "shift"
# - Real world empirical validation data: "imu_mc"
data <- example_data(dat = "imu_mc", dir.data)


# ------------------------------------------------------------------------------
# Plot prediction bands from four methods
# ------------------------------------------------------------------------------

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



# ------------------------------------------------------------------------------
# Leave-one out cross validation to estimate coverage propoerties
# ------------------------------------------------------------------------------

# Percentage of covered points
# 100% means that curve is fully contained within the band limits
n.strides <- length(unique(data$strideID))
# Order of output: POINT, ROISLIEN, BOOTrep, BOOTiid
cover.cross.fraction.singlecurve <- coverage_loocv(data, n.boot)

# Proportion of bands that contain p.cover percent of the T=101 curve points
apply(cover.cross.fraction.singlecurve, 2, coverage_curves, p.cover = 1)
apply(cover.cross.fraction.singlecurve, 2, coverage_curves, p.cover = 0.95)
apply(cover.cross.fraction.singlecurve, 2, coverage_curves, p.cover = 0.9)
apply(cover.cross.fraction.singlecurve, 2, coverage_curves, p.cover = 0.5)

# Get percentile to summarize the distribution
# apply(cover.cross.fraction.singlecurve, 2, quantile, probs = c(0, 0.025, 0.1, 0.25, 0.5))



# ------------------------------------------------------------------------------
# Uncertainty estimation
# ------------------------------------------------------------------------------

n.rep <- 100

# Order of output: POINT, ROISLIEN, BOOT.all, BOOT.iid
uncertainty.area <- estimate_uncertainty_loa(data, n.rep, n.boot)


