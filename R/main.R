# Main script Functional Limits of Agreement (FLoA)
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
#   + Implementieren FDA
#   + Quantile: Über die gesamte Verteilung oder die "Ausreisser-Quantile" einzelner/extremer Probanden
#     + quantile() function: Bias correction useful/necessary?
#   + Konvergenzanalyse --> ja, aber "nur" intern
#   + Implement balanced data in floa_rcb.R
#
# TODO READ:
#   + Ratkowsky - Handbook of nonlinear regression models
#   + https://cran.r-project.org/web/packages/smooth/vignettes/simulate.html
#
# TODO STYLE:
#   + Make variable names more universal (e.g. "device.1" instead "mc")
#   + Add namespaces (package names ::)
#   + No line between header and body in loops
# ******************************************************************************

# library(fda)
# library(funData)

rm(list = ls())

library(ggplot2)

dir.script <- "C:/Users/Daniel/Desktop/tmp/floa/R"
dir.data <- "C:/Users/Daniel/Desktop/tmp/floa/R/examples"

setwd(dir.script)
source("example_data.R")
source("fdaDelta.R")
source("draw_clusters.R")
source("floa_rcb.R")
source("floa_point.R")
source("plot_loa.R")
source("get_coverage.R")
source("crossval_coverage.R")
source("plot_cov_ver.R")


# ********************************* Data sets **********************************

# Wrapper function for example data sets. Function arguments:
#
# (* Empirical validation data: "imu_mc")
# * Smooth, wave data (normal error, constant variance, no trend): "smooth"
# * Smooth wave data with nonlinear trend (constant variance): "smooth_trend"
# * Data with non-gaussian (Weibull distributed) error (no trend): "non_gaussian"
# * Data with shock peaks (no bias, no trend): "shock"

data <- example_data(dat = "smooth", dir.data)

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


# ************* Approximate time series using Fourier functions ****************

fd.basis <- 1 # Number of basic (Fourier) functions

tmp <- fdaDelta(data, fd.basis)


# ****************************** Calculate FLoA ********************************

n.boot <- 100

# Randomized Cluster Bootstrap -------------------------------------------------
#
# * In the first stage, n=length(subjects) random strides are sampled
# from all strides (with replacement). Strides are selected from the entire set
# of curves (NOT! one curve per subject).
# * The process is repeated n.boot times.
# From the resulting distribution, percentiles (2.5%, 50%, 97.5%) are calculated.
#
# In current implementation: Specify version number (ver):
# v1   : n = length(subjects) random strides from all strides
# v1.1 : Functional data version of v1
# v2   : One random stride per subject
# v3   : Fetch a SINGLE random stride from all strides
# ------------------------------------------------------------------------------

floa <- floa_rcb(data, n.boot, ver = "v2")

# Pointwise LoA ----------------------------------------------------------------

# Mean and SD are calculated across all strides (and subjects).
# No bootstrap or other resampling strategies are applied.
floa.point <- floa_point(data)


# ********************************* Plot data **********************************

# Select limits of agreement method and central tendency parameter
plot_loa(data, floa, central.tendency = "mean")

# floa.point <- data.frame(t(floa.point))


# ********************************* Coverage ***********************************

# Calculate coverage (entire curves within the limits of agreement)
coverage <- get_coverage(data, floa.point) # Select floa method: floa or floa.point

print(coverage)


# ***************************** Cross validation *******************************

# Leave-one (subject) out method to estimate the achieved coverage
# See e. g. Lenhoff et al. (1999)
#
# Specify method calculated in crossval_coverage()
#   * method = "all"
#   * method = "floa.rcb"
#   * method = "floa.point"
#
# Select a version for the calculation of floa.rcb (see above)

# Currently, different version of the sampling process in draw_clusters() (nested)
# in floa_rcb()) are implemented (specified by the function argument ver).

# Select a version:
# v1  : n=length(subjects) random strides from all strides
# v2  : Functional data version of v1
# v3  : Fetch a single stride only from all strides
#
# Output:()
#   * Coverage levels [%] across n=length(subjectID) iterations
# ------------------------------------------------------------------------------

cover.cross.v1 <- crossval_coverage(data, floa, floa.point, method = "all", ver = "v1")
cover.cross.v2 <- crossval_coverage(data, floa, floa.point, method = "all", ver = "v2")
cover.cross.v3 <- crossval_coverage(data, floa, floa.point, method = "all", ver = "v3")


plot_cov_ver(cover.cross.v1)
plot_cov_ver(cover.cross.v2)
plot_cov_ver(cover.cross.v3)


