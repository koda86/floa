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

library(fda)
library(funData)

rm(list=ls())

dir.script <- "C:/Users/Daniel/Desktop/tmp/floa/R"
dir.data <- "C:/Users/Daniel/Desktop/tmp/floa/R/examples"

setwd(dir.script)
source("example_data.R")
source("draw_clusters.R")
source("floa_rcb.R")
source("fdaDelta.R")

################################### Data sets ##################################
################################################################################

# Wrapper function to call available data sets. Function arguments:
# 1. Empirical validation data: "imu_mc"
# 2. ARIMA: ...
# 3. Fourier based surrogate data: ...

data <- example_data(dat = "imu_mc", dir.data)


################################ Calculate FLoA ################################
################################################################################

n.boot <- 100

# Randomized Cluster Bootstrap (FLoAboot_RCB) -------------------------------
#
# Function returns difference curves and percentiles (2.5%, 50%, 97.5%)
FLOArcb <- floa_rcb(data, plt = FALSE)
# FLOArcb <- floa_rcb(data, fd.basis, n.boot, plt = FALSE) # Version with functional data




# # FLoA_2SD ------------------------------------------------------------------
# floa.boot.2SD.fdata <- FLOAboot_2SD(fda.delta, n.boot)
#
# # Convert class 'fdata' to class 'funData' to prepare data for ggploting
# floa.boot.2SD.fd <- lapply(floa.boot.2SD.fdata, fdata2fd)
# floa.boot.2SD.funData <- new("multiFunData", list(fd2funData(floa.boot.2SD.fd[[1]], argvals=seq(0, 1, 0.01)),
#                                                   fd2funData(floa.boot.2SD.fd[[2]], argvals=seq(0, 1, 0.01))))



