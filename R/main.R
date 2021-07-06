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
# Fit Fourier. Returns delta curves, mean and sd.
fda.delta <- fdaDelta(data, fd.basis)


################################################################################
################################ Calculate FLoA ################################
################################################################################

n.boot <- 10

# # 1. FLoA_2SD ------------------------------------------------------------------
# floa.boot.2SD.fdata <- FLOAboot_2SD(fda.delta, n.boot)
#
# # Convert class 'fdata' to class 'funData' to prepare data for ggploting
# floa.boot.2SD.fd <- lapply(floa.boot.2SD.fdata, fdata2fd)
# floa.boot.2SD.funData <- new("multiFunData", list(fd2funData(floa.boot.2SD.fd[[1]], argvals=seq(0, 1, 0.01)),
#                                                   fd2funData(floa.boot.2SD.fd[[2]], argvals=seq(0, 1, 0.01))))


# 2. Randomized Cluster Bootstrap (FLoAboot_RCB) -------------------------------
#
# Function returns difference curves and percentiles (2.5%, 50%, 97.5%)
FLOArcb <- floa_rcb(data, fd.basis, n.boot)

plot(FLOArcb[[1]][1, ],
     type = "l",
     ylim = c(-20, 20),
     ylab = "Diff [deg]")
apply(FLOArcb[[1]], 1, lines)
apply(FLOArcb[[2]], 1, lines, col = "red", lwd = 5)



# 3. Pointwise (Bland & Altman) (FLoA_Point) -----------------------------------



