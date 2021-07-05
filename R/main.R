################################################################################
# Main script Functional Limits of Agreement (FLoA)
#
# Calculate different FLoA methods
#
# 1. Bootstrapped functional B & A Limits (Roislien et al., 2012) (FLoA_2SD)
# 2. Randomized Cluster Bootstrap (FLoAboot_RCB)
# 3. Pointwise (Bland & Altman) (FLoA_Point)
################################################################################

library(fda)
library(funData)

rm(list=ls())

dir.script <- "C:/Users/Daniel/Desktop/tmp/floa/R"
dir.data <- "C:/Users/Daniel/Desktop/tmp/floa/R/examples"

setwd(dir.script)
# source("FLoAboot_2SD.R")
source("draw_clusters.R")
source("floa_rcb.R")
source("fdaDelta.R")

# Long format data consisting of device, subjectID, and strideID
data <- readRDS(paste0(dir.data, "/", "data.rds"))

# Intermediate step: Fit functions to discrete time series data ----------------
fd.basis <- create.fourier.basis(nbasis=50) # Plateau around 50 basis vectors
# Fit Fourier
fda.delta <- fdaDelta(data, fd.basis) # Returns delta curves, mean and sd


################################################################################
################################ Calculate FLoA ################################
################################################################################

n.boot <- 5

# # 1. FLoA_2SD ------------------------------------------------------------------
# floa.boot.2SD.fdata <- FLOAboot_2SD(fda.delta, n.boot)
#
# # Convert class 'fdata' to class 'funData' to prepare data for ggploting
# floa.boot.2SD.fd <- lapply(floa.boot.2SD.fdata, fdata2fd)
# floa.boot.2SD.funData <- new("multiFunData", list(fd2funData(floa.boot.2SD.fd[[1]], argvals=seq(0, 1, 0.01)),
#                                                   fd2funData(floa.boot.2SD.fd[[2]], argvals=seq(0, 1, 0.01))))


# 2. Randomized Cluster Bootstrap (FLoAboot_RCB) -------------------------------
floa.rcb.percentiles.intrp <- floa_rcb(data, fd.basis, n.boot)


# 3. Pointwise (Bland & Altman) (FLoA_Point) -----------------------------------



