# Main script Functional Limits of Agreement (FLoA)
# ------------------------------------------------------------------------------
#
# FLoA derived by different methods are compared
#   * Randomized Cluster Bootstrap      (floa.rcb)
#     * Different strategies
#   * Point-by-point Gaussian intervals (floa.point)
#
#
# For demonstration, different (synthetic) data sets can be chosen
# (see subsection data sets)
#
# TODO:
#   + Wie lassen sich zu weite LoA effektiv beschreiben / in einen Parameter giessen? Abstand zur aeussersten Kurve (als MSE)?
#   + Weitere edge case Datensaetze a la shift
#   + range-Angabe unertainty intervals!
#   + Wie lassen sich (potenziell) zu weite LoA quantifizieren (Roislien!?)? Estimation uncertainty in Roislien is probably a lot higher
#   + mean or median for floa.rcb?
#   + 95% coverage implementieren
#   + (Implementieren FDA)
#   + Preallocation in floa_rcb()
#   + Implement balanced data in floa_rcb.R
#
# TODO READ:
#   + B&A, 2007
#   + https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-020-01022-x
#   + Ratkowsky - Handbook of nonlinear regression models
#   + Robinson et al. (2021)
#   + https://cran.r-project.org/web/packages/smooth/vignettes/simulate.html
#
# TODO STYLE:
#   + Add namespaces (package names ::)
#   + Vectorize wherever possible
#   + Make variable names more universal (e.g. "device.1" instead "mc")
#   + Rename n.strides to n.curves()
#   + No line between header and body in loops
# ******************************************************************************
rm(list = ls())

library(ggplot2)

dir.script <- "~/floa/R"
dir.data <- "~/floa/R/examples"

setwd(dir.script)

source("example_data.R")
source("fdaDelta.R")
source("pick_subwise_curves.R")
source("draw_clusters.R")
source("floa_rcb.R")
source("lenhoff_doris.R")
source("floa_point.R")
source("floa_roislien.R")
source("plot_loa.R")
source("get_coverage.R")
source("get_coverage_fraction.R")
source("get_coverage_singlecurve.R")
source("get_coverage_singlecurve_fraction.R")
source("crossval_coverage.R")
source("crossval_coverage_fraction.R")
source("singlecurve_coverage.R")
source("singlecurve_coverage_fraction.R")
source("plot_cov_ver.R")
source("distance_2_floa.R")
source("estimate_uncertainty_loa.R")

# ********************************* Data sets **********************************
# Wrapper function for example data sets. Function arguments:
#
# * Real world empirical validation data: "imu_mc"
# * Realistic looking, smooth, wave data (normal error, no trend): "smooth_realistic"
# * Smooth, wave data (normal error, no trend): "smooth"
# * Smooth wave data with nonlinear trend (constant variance): "smooth_trend"
# * Heteroscedastic errors (no trend): "heteroscedastic"
# * Data with non-gaussian (Weibull distributed) error (no trend): "non_gaussian"
# * Data with shock peaks (no bias, no trend): "shock"
# * Phase shifted data (x-axis direction): "shift"
data <- example_data(dat = "smooth_realistic", dir.data)

# Plot data --------------------------------------------------------------------
# uncommment when subject differences need to be plotted
# data$subjectID <- as.factor(data$subjectID)
data.single.mc <- subset(data, device == "MC")
data.single.imu <- subset(data, device == "IMU")

PLOT <- ggplot(data = data.single.mc, aes(x = frame, y = value, group = strideID)) + # , colour = subjectID
  geom_line(alpha = 0.7) +
  geom_line(data = data.single.imu, aes(x = frame, y = value, group = strideID, col = "red"), alpha = 0.3) + #, colour = subjectID
  labs(x = "Time-normalized signal [%]", y = "Difference") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        legend.position = "none")

PLOT


# ****************************** Calculate FLoA ********************************
n.boot <- 400

# Randomized Cluster Bootstrap -------------------------------------------------
#
# * In the first stage, n=length(subjects) random strides are sampled
# from all strides (with replacement). Strides are selected from the entire set
# of curves (NOT! one curve per subject).
# * The process is repeated n.boot times.
# From the resulting distribution, percentiles (2.5%, 50%, 97.5%) are calculated.
#
# In current implementation: Specify version number (ver):
# v1 : n = length(subjects) random strides from all strides
# v2 : One random stride per subject
# v3 : Fetch a SINGLE random stride from all strides
# v4 : Roislien approach (Get one random stride from each subject ONCE and boot-
#      strap the resulting sample (of length (n=length(subjects))
# floa <- floa_rcb(data, n.boot, ver = "v2")

floa.point <- floa_point(data)
floa.roislien <- floa_roislien(data)
floa.lenhoff <- floa_lenhoff(data, k_reihe = 50, n.boot = n.boot, cp.begin = 0, alpha = 0.05)

plot_loa(data, floa.point, floa.roislien, floa.lenhoff, ylim = c(-5, 5))


# ********************************* Coverage ***********************************
# Calculate coverage (entire curves within the limits of agreement)
coverage <- get_coverage(data, floa.roislien) # Select floa method: floa or floa.point
print(coverage)


# ***************************** Cross validation *******************************
# Currently, two different methods for estimating the uncertainty in the achieved
# coverage are implemented:
# 1. Leave all curves of a single subject out
# 2. Leave-one single curve out
#
# Currently, different versions of the sampling process in draw_clusters() (nested
# in floa_rcb()) are implemented:
#
# v1 : n = length(subjects) random strides from all strides
# v2 : One random stride per subject
# v3 : Fetch a SINGLE random stride from all strides
# v4 : Roislien approach (Get one random stride from each subject ONCE and boot-
#      strap the resulting sample (of length (n=length(subjects))
# v5 : Pointwise B & A Limits of Agreement
#
# Output:
#   * Coverage levels [%] across n=length(subjectID) iterations
# ******************************************************************************
# 1. Leave-one subject out
cover.cross.subject <- crossval_coverage(data, n.boot)

# Display cross validation coverages (entire curve covered) across methods
plot_cov_ver(cover.cross.subject)

# Mean fraction of the curve that is covered) across methods
cover.cross.fraction.subject <- crossval_coverage_fraction(data, n.boot)

plot_cov_ver(cover.cross.fraction.subject)

# 2. Leave-one curve out
cover.cross.singlecurve <- singlecurve_coverage(data, n.boot)

calculate_percent <- function(data) {
  # Calculate percent from counts
  percentage <- (sum(data) / length(data))
  percentage <- round(percentage, digits = 2)
}
covered.curves.percent <- apply(cover.cross.singlecurve, 2, calculate_percent)

cover.cross.fraction.singlecurve <- singlecurve_coverage_fraction(data, n.boot)
plot_cov_ver(cover.cross.fraction.singlecurve)



# Estimate the uncertainty across several iterations
# Returns results for all implemented methods
estimate_uncertainty_loa(data, n.boot)


# Average distance to the outer point of the curve set
dist.all <- data.frame(distance_2_floa(data, floa)$upr.dist,
                       distance_2_floa(data, floa)$lwr.dist,
                       distance_2_floa(data, floa.point)$upr.dist,
                       distance_2_floa(data, floa.point)$lwr.dist,
                       distance_2_floa(data, floa.roislien)$upr.dist,
                       distance_2_floa(data, floa.roislien)$lwr.dist
                       )

colnames(dist.all) <- c("floa.up", "floa.lw", "point.up", "point.lw", "roislien.up", "roislien.lw")

boxplot(dist.all, ylab = "floa boundary - curve maximum")

