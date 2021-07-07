################################################################################
# Main script Functional Limits of Agreement (FLoA)
#
# FLoA are calculated using a randomized cluster bootstrap
#
# Thus far, a transformation of time series data to functional data (Fourier,
# Splines etc.) is not implemented.
#
# Example data sets can be called from the function example data().
################################################################################

# library(fda)
# library(funData)

rm(list=ls())

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
# 3. Intrainvidual differences:
# 4. Non-stationar:"non_stationary"
# 5. shock: "shock"

data <- example_data(dat = "imu_mc", dir.data)

################################ Calculate FLoA ################################

n.boot <- 100

# Randomized Cluster Bootstrap -------------------------------------------------
#
# Function returns percentiles (2.5%, 50%, 97.5%)
FLOArcb <- floa_rcb(data, n.boot, plt = TRUE)

# Version with functional data
# FLOArcb <- floa_rcb(data, fd.basis, n.boot, plt = FALSE)



