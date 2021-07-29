################################################################################
# Main script Functional Limits of Agreement (FLoA)
#
# FLoA derived by different methods are compared
#   * Randomized Cluster Bootstrap      (floa.rcb)
#   * Point-by-point Gaussian intervals (floa.point)
#   * Method Lenhoff et al. (1999)      (curently not implemented)
#   * Method Roislien et al. (2012)     (curently not implemented)
#
# Thus far, a transformation of time series data to functional data (Fourier,
# Splines etc.) is not implemented.
#
# For demonstration, different (synthetic) data sets can be chosen
# (see subsection data sets)
#
# TODO:
#   * Compare v1 (Chris) and v3 (Doris) floa methods
#   * Implement balanced data in floa_rcb.R
#   * Simulierte Daten in einer Funktion zusammenfassen
#   * mean oder median as estimator?
#   * Method comparisons: Against FDA (Lenhoff/Roislien) and pointwise!?
#   * Use only synthetic data? (Leave out real world example?)
#   * Umbennung in CLoA (Continuous LoA)?
#   * quantile() function: Bias correction useful/necessary?
#   * Konvergenzanalyse --> "[...] the  achieved  level  of  bootstrap bands  is
# roughly  equal  to  the nominal  level  with as  few  as  25  or  so  curves.

# Further reading:
#   * (Appendix) Lenhoff et al. (1999)
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
# source("fdaDelta.R")


################################### Data sets ##################################

# Wrapper function for example data sets.
#
# Function arguments:
#
# * Empirical validation data: "imu_mc"
# * Smooth, wave data (normal error, constant variance, no trend): "smooth"
# * Biased data (constant variance, no trend): "bias"
# * Non-constant variance data (normal error, no trend): "non_const_var"
# * Non-stationary data (trend, no bias) data:"non_stationary"
# * Log-normal error data (no bias,  constant variance, no trend): "log_normal"
# * Data with shock peaks (no bias, no trend): "shock"

data <- example_data(dat = "smooth", dir.data)


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
# v1  : n=length(subjects) random strides from all strides
# v2  : Functional data version of v1
# v3  : Fetch a single stride only form all strides
# vall: Returns all implemented versions
# ------------------------------------------------------------------------------
ver = "v3"

floa.boot.percentiles.intrp <- floa_rcb(data, n.boot, ver)

# Pointwise LoA
# ------------------------------------------------------------------------------
floa.point <- floa_point(data)

# # Method Lenhoff et al. (1999)
# # ------------------------------------------------------------------------------

# # Method Roislien et al. (2012)
# # ------------------------------------------------------------------------------


################################### Plot data ##################################

floa.rcb <- data.frame(t(floa.boot.percentiles.intrp))
floa.point <- data.frame(t(floa.point))

plot_loa(data, floa.rcb) # Select floa method


################################### Coverage ###################################

# Calculate coverage (entire curves within the percentile boundaries)
coverage <- get_coverage(data, t(floa.rcb)) # Select floa method

print(coverage)


############################### Cross validation ###############################

# Leave-one (subject) out method to estimate the achieved coverage
# See Lenhoff et al. (1999)
#
# Specify method calculated in crossval_coverage()
#   * method = "all"
#   * method = "floa.rcb"
#   * method = "floa.point"
#
# Output:
#   * Coverage levels [%] and SEM across n=length(subjectID) iterations
#   * Standard error of estimate
# ------------------------------------------------------------------------------

cover.cross <- crossval_coverage(data, n.boot, method = "all")

# Prepare data for (gg)plotting
cover.cross.df <- data.frame(cover.cross)
cover.cross.long <- reshape(cover.cross.df, direction = "long", varying = list(names(cover.cross.df)))[, 1:2]
cover.cross.long$time <- as.factor(cover.cross.long$time)
names(cover.cross.long)[names(cover.cross.long) != 'time'] <- 'value'
names(cover.cross.long)[names(cover.cross.long) == 'time'] <- 'method'

cover.PLOT <- ggplot(cover.cross.long, aes(x = method, y = value)) + geom_boxplot()
cover.PLOT

