# Main script Functional Limits of Agreement (FLoA)

library(fda)

rm(list=ls())

# Long format data consisting of device, subjectID, and strideID
# data <- readRDS("data.rds")

# Convert discrete time series to functional data (class fd (package fda))
# ------------------------------------------------------------------------------
fd.basis <- create.fourier.basis(nbasis=50) # Function fit to empirical curves appears to plateau around 50 basis vectors

# Randomized Cluster Bootstrap (FLoAboot_RCB)
# ------------------------------------------------------------------------------
floa_rcb <- floa_rcb(data, fd.basis, n.boot, n.subj)
