# Main script Functional Limits of Agremment (FLoA)

library(fda)

rm(list=ls())

# Convert discrete time series to functional data (class fd (package fda))
# ------------------------------------------------------------------------------
# data.long <- readRDS("data.long.rds")

fd.basis <- create.fourier.basis(nbasis=50) # Function fit to empirical curves appears to plateau around 50 basis vectors

# Returns delta curves, mean and sd for each joint condition
# 6 joints, left / right == 18 list elements
fd.fda.jointwise <- fdaData(data.long, fd.basis)
