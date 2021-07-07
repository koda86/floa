# Simulate data
################################################################################

# ------------------------------------------------------------------------------
# Non-stationary ---------------------------------------------------------------
#
# https://stats.stackexchange.com/questions/330199/simulating-drift-in-the-data
# ------------------------------------------------------------------------------

n.subj <- 11
n.strides <- 1100
n.devices <- 2
n.frames <- 101

alpha <- 2 # Constant term

value <- c()
device <- c()

for (i in 1:(n.strides * n.subj)) {

  # AR coefficients
  phi1 <- c(0.80, 0.15)
  phi2 <- runif(1, 0.2, 0.3)

  n <- 101 # Number of periods

  # Trend coefficient
  scale1 <- 0.1
  scale2 <- 0.05

  # Initial data values
  imu <- c(rep(0,n))
  mc <- c(rep(0,n))

  # Error terms
  w <- rnorm(n, mean=0, sd=1)
  v <- rnorm(n, mean=0, sd=5)

  for (t in 3:n) {
    # AR(1) with constant and trend (no shocks)
    imu[t] <- alpha + phi1[1] * imu[t-1] + v[t] + scale1*t # imu
    mc[t] <- alpha + phi2[1] * mc[t-1] + v[t] + scale2*t   # mc
  }

  # plot(imu, type = "l")
  # plot(mc, type = "l")

  imu <- round(imu, 2)
  mc <- round(mc, 2)

  value <- c(value, c(imu, mc))

  device.imu <- rep("IMU", n)
  device.mc <- rep("MC", n)
  device <- c(device, c(device.imu, device.mc))
}

subjectID <- rep(1:n.subj, each = n.devices * n.strides * n.frames)
strideID <- rep(1:n.strides, each = n.devices * n.subj * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "data_nonstat.rds"))
