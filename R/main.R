################################################################################
# Main script Functional Limits of Agreement (FLoA)
#
# FLoA derived by different methods are compared
#   * Randomized Cluster Bootstrap      (floa.rcb)
#     * Different strategies
#   * Point-by-point Gaussian intervals (floa.point)
#   (* Method Lenhoff et al. (1999)      (curently not implemented))
#   (* Method Roislien et al. (2012)     (curently not implemented))
#
# Thus far, a transformation of time series data to functional data (Fourier,
# Splines etc.) is not implemented.
#
# For demonstration, different (synthetic) data sets can be chosen
# (see subsection data sets)
#
# TODO:
#   * Implementieren FDA!?
#   * Nur synthetische Daten verwenden? (Leave out real world example?)
#   * Welche weiteren (simulierten) Daten?
#   * FLoA_RCB: mean oder median as estimator?
#   * Umbennung in CLoA (Continuous LoA)?
#   * quantile() function: Bias correction useful/necessary?
#   * Konvergenzanalyse --> "[...] the  achieved  level  of  bootstrap bands  is
#   roughly  equal  to  the nominal  level  with as  few  as  25  or  so  curves.
#   * Implement balanced data in floa_rcb.R
################################################################################

# library(fda)
# library(funData)

rm(list = ls())

library(ggplot2)

dir.script <- "C:/Users/Daniel/Desktop/tmp/floa/R"
dir.data <- "C:/Users/Daniel/Desktop/tmp/floa/R/examples"

setwd(dir.script)
source("example_data.R")
source("draw_clusters.R")
source("floa_rcb.R")
source("floa_point.R")
source("plot_loa.R")
source("get_coverage.R")
source("crossval_coverage.R")
source("plot_cov_ver.R")
# source("fdaDelta.R")


################################### Data sets ##################################

# Wrapper function for example data sets.
#
# Function arguments:
#
# * Empirical validation data: "imu_mc"
# * Smooth, wave data (normal error, constant variance, no trend): "smooth"
# * Biased data (constant variance, no trend): "bias"
# * Non-stationary data (trend, no bias) data:"non_stationary"
# * Data with shock peaks (no bias, no trend): "shock"

data <- example_data(dat = "non_const_var", dir.data)


################################ Calculate FLoA ################################

n.boot <- 100

# Randomized Cluster Bootstrap
# ------------------------------------------------------------------------------

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
ver = "v2"

floa.boot.percentiles.intrp <- floa_rcb(data, n.boot, ver)

# Pointwise LoA
# ------------------------------------------------------------------------------

# Mean and SD are calculated across all strides (and subjects).
# No bootstrap or other resampling strategies are applied.
floa.point <- floa_point(data)

# # Method Lenhoff et al. (1999)
# # ------------------------------------------------------------------------------

# # Method Roislien et al. (2012)
# # ------------------------------------------------------------------------------


################################### Plot data ##################################

floa.rcb <- data.frame(t(floa.boot.percentiles.intrp))
plot_loa(data, floa.rcb) # Select floa method

floa.point <- data.frame(t(floa.point))


################################### Coverage ###################################

# Calculate coverage (entire curves within the percentile boundaries)
coverage <- get_coverage(data, t(floa.point)) # Select floa method

print(coverage)


############################### Cross validation ###############################

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
# v3  : Fetch a single stride only form all strides
#
# Output:
#   * Coverage levels [%] across n=length(subjectID) iterations
# ------------------------------------------------------------------------------

system.time(
  cover.cross.v1 <- crossval_coverage(data, n.boot, method = "all", ver = "v1")
  )
cover.cross.v2 <- crossval_coverage(data, n.boot, method = "all", ver = "v2")
cover.cross.v3 <- crossval_coverage(data, n.boot, method = "all", ver = "v3")


plot_cov_ver(cover.cross.v1)
plot_cov_ver(cover.cross.v2)
plot_cov_ver(cover.cross.v3)


############################# Convergence analysis #############################


