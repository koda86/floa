# Main script Functional Limits of Agreement (FLoA)

library(fda)

rm(list=ls())

# Set working directories
dir.data <- "C:/Users/Daniel/Desktop/tmp/floa/R"
dir.script <- "C:/Users/Daniel/Desktop/tmp/floa/R/examples"

# Long format data consisting of device, subjectID, and strideID
data <- readRDS(paste0(dir.script, "/", "data.rds"))

# Convert discrete time series to functional data (class fd (package fda))
# ------------------------------------------------------------------------------
# Function fit to empirical curves appears to plateau around 50 basis vectors
fd.basis <- create.fourier.basis(nbasis=50)

# 1. Bootstrapped functional B & A Limits (Roislien et al., 2012) (FLoA_2SD)
# ------------------------------------------------------------------------------
# Returns 12 elements list with class fdata elements
floa.boot.2SD.fdata <- FLOAboot_2SD(fd.fda.jointwise, n.boot)

# Convert class 'fdata' to class 'funData' to prepare data for ggploting
floa.boot.2SD.funData <- Conv2funData(floa.boot.2SD.fdata)


# 2. Randomized Cluster Bootstrap (FLoAboot_RCB)
# ------------------------------------------------------------------------------
n.boot <- 5

source("draw_clusters.R")
source("floa_rcb.R")

floa.rcb.percentiles.intrp <- floa_rcb(data, fd.basis, n.boot)
