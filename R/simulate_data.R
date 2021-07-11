# Simulate data
################################################################################

# ------------------------------------------------------------------------------
# Non-stationary ---------------------------------------------------------------
#
# https://stats.stackexchange.com/questions/330199/simulating-drift-in-the-data
# ------------------------------------------------------------------------------

n.subj <- 11
n.strides <- 100
n.devices <- 2
n.frames <- 101

alpha <- 2 # Constant term

device <- c()
value <- c()
subjectID <- c()

for (subj.idx in 1:n.subj) {

  value.subj <- c()
  device.subj <- c()
  subjectID.subj <- c()

  # set.seed(subj.idx)

  for (stride.idx in 1:(n.strides)) {

    # AR coefficients
    phi1 <- c(0.80, 0.15)
    phi2 <- runif(1, 0.2, 0.3)

    # Trend coefficient
    scale1 <- 0.1
    scale2 <- 0.05

    imu <- c(rep(0, n.frames))
    mc <- c(rep(0, n.frames))

    # Error terms
    w <- rnorm(n.frames, mean = 0, sd = 1)
    v <- rnorm(n.frames, mean = 0, sd = 5)

    for (t in 3:n.frames) {
      # AR(1) with constant and trend (no shocks)
      imu[t] <- alpha + phi1[1] * imu[t-1] + v[t] + scale1*t # imu
      mc[t] <- alpha + phi2[1] * mc[t-1] + v[t] + scale2*t   # mc
    }

    value.subj <- c(value.subj, c(imu, mc))

    device.imu <- rep("IMU", n.frames)
    device.mc <- rep("MC", n.frames)
    device.subj <- c(device.subj, c(device.imu, device.mc))

    subjectID.subj <- c(subjectID.subj, rep(subj.idx, 2 * n.frames))
  }

  device <- c(device, device.subj)
  value <- c(value, value.subj)
  subjectID <- c(subjectID, subjectID.subj)
}

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames) # seq(1, n.subj * n.strides)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "data_nonstat.rds"))
