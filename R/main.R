################################################################################
# Main script Functional Limits of Agreement (FLoA)
#
# FLoA are calculated using a randomized cluster bootstrap
#
# Thus far, a transformation of time series data to functional data (Fourier,
# Splines etc.) is not implemented.
#
# TODO:
#     * Data:
#         - Drift nur in einem der Messsysteme (IMU)
#         - Eines der beiden Messsysteme ist verrauscht, eins smooth
#         - Daten mit smoothen und nicht smoothen Signalanteil
#     * Cross validation
#     * Compare against FDA (Lenhoff/Roislien) and/or pointwise
#     * Use only synthetic data? (Leave out real world example?)
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
# 2. ARIMA: "arima"
# 3. Non-stationar:"non_stationary"
# 4. shock: "shock"

data <- example_data(dat = "shock", dir.data)


################################ Calculate FLoA ################################

n.boot <- 100

# Randomized Cluster Bootstrap -------------------------------------------------
#
# Function returns percentiles (2.5%, 50%, 97.5%)
FLOArcb <- floa_rcb(data, n.boot, plt = TRUE)

# Version with functional data
# FLOArcb <- floa_rcb(data, fd.basis, n.boot, plt = FALSE)


############################### Cross validation ###############################

# Leave-one out method to estimate the achieved coverage
# See Lenhoff
#
# Output:
#     * Coverage level (%)
#     * Standard error of estimate
#     * "[...] the  achieved  level  of  bootstrap bands  is  roughly  equal  to  the
# nominal  level  with as  few  as  25  or  so  curves. --> Konvergenzanalyse


