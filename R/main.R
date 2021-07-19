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
#     * Data:
#         - Nichtnormalverteilte Daten
#         - Eines der beiden Messsysteme ist verrauscht, eins smooth
#         - Daten mit smoothen und nicht smoothem Signalanteil
#     * Simulierte Daten in einer Funktion zusammenfassen
#     * Cross validation
#     * mean oder median as estimator?
#     * Method comparisons: Against FDA (Lenhoff/Roislien) and/or pointwise!?
#     * See appendix Lenhoff et al.
#     * Use only synthetic data? (Leave out real world example?)
#     * Umbennung in CLoA (Continuous LoA)?
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
# source("fdaDelta.R")


################################### Data sets ##################################

# Wrapper function for example data sets.
# Function arguments:
# 1. Empirical validation data: "imu_mc"
# 3. Biased data (constant variance, no trend): "bias"
# 3. Non-constant variance data (normal error, no trend): "non_const_var"
# 4. Non-stationary data (trend, no bias) data:"non_stationary"
# 2. Log-normal error data (no bias,  constant variance, no trend): "log_normal"
# 5. Data with shock peaks (no bias, no trend): "shock"

data <- example_data(dat = "log_normal", dir.data)


###################################### FLoA #####################################

n.boot <- 100

# Randomized Cluster Bootstrap -------------------------------------------------
#
# Function returns percentiles (2.5%, 50%, 97.5%)
FLOArcb <- floa_rcb(data, n.boot, plt = TRUE)

# Method Roislien et al. -------------------------------------------------------

# Method Lenhoff ---------------------------------------------------------------

# Pointwise LoA ----------------------------------------------------------------


############################### Cross validation ###############################

# Leave-one out method to estimate the achieved coverage
# See Lenhoff
#
# Output:
#     * Coverage level (%)
#     * Standard error of estimate
#     * "[...] the  achieved  level  of  bootstrap bands  is  roughly  equal  to  the
# nominal  level  with as  few  as  25  or  so  curves. --> Konvergenzanalyse


