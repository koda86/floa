################################################################################
# Main script Functional Limits of Agreement (FLoA)
#
# FLoA are calculated using a randomized cluster bootstrap
#
# Thus far, a transformation of time series data to functional data (Fourier,
# Splines etc.) is not implemented.
#
# TODO:
#     * Implement balanced data in floa_rcb.R
#     * Simulierte Daten in einer Funktion zusammenfassen
#     * Cross validation
#     * mean oder median as estimator?
#     * Method comparisons: Against FDA (Lenhoff/Roislien) and/or pointwise!?
#     * See appendix Lenhoff et al.
#     * Use only synthetic data? (Leave out real world example?)
#     * Umbennung in CLoA (Continuous LoA)?
#     * quantile() function: Bias correction useful/necessary?
################################################################################

# library(fda)
# library(funData)

rm(list = ls())

dir.script <- "C:/Users/Daniel/Desktop/tmp/floa/R"
dir.data <- "C:/Users/Daniel/Desktop/tmp/floa/R/examples"

setwd(dir.script)
source("example_data.R")
source("draw_clusters.R")
source("floa_rcb.R")
source("plot_loa.R")
source("get_coverage.R")
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


###################################### FLoA #####################################

n.boot <- 100

# Randomized Cluster Bootstrap -------------------------------------------------
#
# * In the first stage, n=length(subjects) random strides are sampled
# from all strides (with replacement). Strides are selected from the entire set
# of curves (NOT! one curve per subject).
# * The process is repeated n.boot times.
# From the resulting distribution, percentiles (2.5%, 50%, 97.5%) are calculated.

# Function returns percentiles (2.5%, 50%, 97.5%)
floa.boot.percentiles.intrp <- floa_rcb(data, n.boot, plt = TRUE)

# Pointwise LoA ----------------------------------------------------------------


# Method Roislien et al. -------------------------------------------------------

# Method Lenhoff ---------------------------------------------------------------


################################### Plot data ##################################

floa <- data.frame(t(floa.boot.percentiles.intrp))

plot_loa(data, floa)

#################################### Coverage ##################################

# Calculate coverage (entire curves within the percentile boundaries) ----------
coverage <- get_coverage(floa.boot.percentiles.intrp)

print(coverage)


############################### Cross validation ###############################

# Leave-one (subject) out method to estimate the achieved coverage
# See Lenhoff
#
# Output:
#     * Coverage level (%)
#     * Standard error of estimate
#     * "[...] the  achieved  level  of  bootstrap bands  is  roughly  equal  to  the
# nominal  level  with as  few  as  25  or  so  curves. --> Konvergenzanalyse



