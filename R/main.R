# Main script Functional Limits of Agreement (FLoA)

library(fda)

rm(list=ls())

# Set wokring directories
dir.data <- "C:/Users/Daniel/Desktop/tmp/floa/R"
dir.script <- "C:/Users/Daniel/Desktop/tmp/floa/R/examples"

# Long format data consisting of device, subjectID, and strideID
data <- readRDS(paste0(dir.script, "/", "data.rds"))

# Convert discrete time series to functional data (class fd (package fda))
# ------------------------------------------------------------------------------
fd.basis <- create.fourier.basis(nbasis=50) # Function fit to empirical curves appears to plateau around 50 basis vectors

# Randomized Cluster Bootstrap (FLoAboot_RCB)
# ------------------------------------------------------------------------------
n.boot <- 5

source("draw_clusters.R")
source("floa_rcb.R")

floa.rcb.percentiles.intrp <- floa_rcb(data, fd.basis, n.boot)
