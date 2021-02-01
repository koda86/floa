# Main script Functional Limits of Agremment (FLoA)

library(fda)

rm(list=ls())

# Convert discrete time series to functional data (class fd (package fda))
# ------------------------------------------------------------------------------
fd.basis <- create.fourier.basis(nbasis=50) # Function fit to empirical curves appears to plateau around 50 basis vectors

# data.long <- readRDS("data.long.rds")
fd.fda.jointwise <- fdaData(data.long, fd.basis) # Returns delta curves, mean and sd for each condition

# Randomized Cluster Bootstrap (FLoAboot_RCB)
# ------------------------------------------------------------------------------
floa_rcb <- floa_rcb(data.long, fd.basis, n.boot)
