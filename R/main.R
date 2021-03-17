################################################################################
# Main script Functional Limits of Agreement (FLoA)
#
# Calculate different FLoA methods
#
# 1. Bootstrapped functional B & A Limits (Roislien et al., 2012) (FLoA_2SD)
# 2. Randomized Cluster Bootstrap (FLoAboot_RCB)
# 3. Pointwise (Bland & Altman) (FLoA_Point)
####################

library(fda)

rm(list=ls())

source("FLoAboot_2SD.R")
source("draw_clusters.R")
source("floa_rcb.R")
source("fdaDelta.R")
source("FLoAboot_2SD.R")

# Set working directories
dir.data <- "C:/Users/Daniel/Desktop/tmp/floa/R"
dir.script <- "C:/Users/Daniel/Desktop/tmp/floa/R/examples"

# Long format data consisting of device, subjectID, and strideID
data <- readRDS(paste0(dir.script, "/", "data.rds"))

# Convert discrete time series to functional data (class fd, package fda)
# ------------------------------------------------------------------------------
# Function fit to empirical curves appears to plateau around 50 basis vectors
fd.basis <- create.fourier.basis(nbasis=50)

# Approximate empirical time series data using Fourier series (functions)
fda.delta <- fdaDelta(data, fd.basis) # Returns delta curves, mean and sd


######################################
########### Calculate FLoA ###########
######################################

n.boot <- 5

# 1. Bootstrapped functional B & A Limits (FLoA_2SD)
# ------------------------------------------------------------------------------
floa.boot.2SD.fdata <- FLOAboot_2SD(fda.delta, n.boot)

# Convert class 'fdata' to class 'funData' to prepare data for ggploting
floa.boot.2SD.funData <- Conv2funData(floa.boot.2SD.fdata)


# 2. Randomized Cluster Bootstrap (FLoAboot_RCB)
# ------------------------------------------------------------------------------
floa.rcb.percentiles.intrp <- floa_rcb(data, fd.basis, n.boot)


# 3. Pointwise (Bland & Altman) (FLoA_Point)
# ------------------------------------------------------------------------------


