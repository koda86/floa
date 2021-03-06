################################################################################
# Simulate data
################################################################################
#
# Inspired by ...
# https://stats.stackexchange.com/questions/330199/simulating-drift-in-the-data
#
# 1. Non-stationary (trend) data
# 2. Shock (spike) data
################################################################################


# ------------------------------------------------------------------------------
# Smooth, wave data (normal error, constant variance, no trend) ----------------
# ------------------------------------------------------------------------------

n.subj <- 11
n.strides <- 100
n.devices <- 2
n.frames <- 101

device <- c()
value <- c()
subjectID <- c()

for (subj.idx in 1:n.subj) {

  value.subj <- c()
  device.subj <- c()
  subjectID.subj <- c()

  t <- seq(0, 100)

  # Subjectwise wave parameters
  # a1.mean <- 3
  offset.mean <- runif(1, min = -0.5, max = 0.5)

  for (stride.idx in 1:(n.strides)) {

    a1.1 <- rnorm(1, 3, .1) # mc
    a1.2 <- rnorm(1, 3, .1) # imu
    a2.1 <- 0.08 # rnorm(1, 0.1, .01)
    a2.2 <- 0.08 # rnorm(1, 0.2, .01)
    b1.1 <- rnorm(1, 0.06, .001)
    b1.2 <- rnorm(1, 0.06, .001)
    b2.1 <- rnorm(1, 0.58, .001)
    b2.2 <- rnorm(1, 0.58, .001)
    c <- 2

    offset <- rnorm(1, offset.mean, 0.05) # 0.1

    mc <- a1.1 * sin(b1.1 * t) ^ (c + 3) + a2.1 * sin(b2.1 * t)
    imu <- offset + a1.2 * sin(b1.2 * t) ^ (c + 3) + a2.2 * sin(b2.1 * t)

    # plot(mc, type = "l")
    # lines(imu, col = "red")

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

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "smooth.rds"))


# ------------------------------------------------------------------------------
# Biased data (normal error, constant variance, no trend) ----------------------
# ------------------------------------------------------------------------------

offset <- 5

device <- c()
value <- c()
subjectID <- c()

for (subj.idx in 1:n.subj) {

  value.subj <- c()
  device.subj <- c()
  subjectID.subj <- c()

  # Sample subjectwise parameters
  subj.mean <- 5 * rnorm(1)
  subj.sd.1 <- 2 * runif(1)
  subj.sd.2 <- 5 * runif(1)
  trend.1 <- abs(rnorm(1, 0.1, sd = 0.1))
  trend.2 <- 0

  for (stride.idx in 1:(n.strides)) {

    mc <- rnorm(n.frames, mean = subj.mean, sd = subj.sd.2)
    imu <- offset + rnorm(n.frames, mean = subj.mean, sd = subj.sd.1)

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

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "bias.rds"))


# ------------------------------------------------------------------------------
# Non-constant variance data (normal error, no trend) --------------------------
# ------------------------------------------------------------------------------

device <- c()
value <- c()
subjectID <- c()

for (subj.idx in 1:n.subj) {

  value.subj <- c()
  device.subj <- c()
  subjectID.subj <- c()

  # Sample subjectwise parameters
  subj.mean <- rnorm(1)
  subj.sd.1 <- runif(1)
  # Dummy vector to allocate regions (within) one stride with similar smoothness
  # Serves as input (mean) to rnorm in the next step
  subj.sd.2 <- runif(n.frames, min = 0, max = 5)

  for (stride.idx in 1:(n.strides)) {

    mc <- rep(NA, n.frames)
    imu <- rep(NA, n.frames)

    for (frame.idx in 1:n.frames) {

      mc[frame.idx] <- rnorm(1, mean = subj.mean, sd = subj.sd.1)
      imu[frame.idx] <- rnorm(1, mean = subj.mean, sd = subj.sd.2[frame.idx])
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

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "non_const_var.rds"))


# ------------------------------------------------------------------------------
# Non-stationary data (normal error, no bias, constant variance) ---------------
# ------------------------------------------------------------------------------

device <- c()
value <- c()
subjectID <- c()

for (subj.idx in 1:n.subj) {

  value.subj <- c()
  device.subj <- c()
  subjectID.subj <- c()

  # Sample subjectwise parameters
  subj.mean <- 5 * rnorm(1)
  subj.sd.1 <- 2 * runif(1)
  subj.sd.2 <- 5 * runif(1)
  trend.1 <- abs(rnorm(1, 0.1, sd = 0.1))
  trend.2 <- 0

  for (stride.idx in 1:(n.strides)) {

    # AR coefficients
    phi.1 <- c(0.80, 0.15)
    phi.2 <- runif(1, 0.2, 0.3)

    # Trend coefficient
    trend.1 <- 0.1
    trend.2 <- 0.05

    imu <- c(rep(0, n.frames))
    mc <- c(rep(0, n.frames))

    # Error terms
    w <- rnorm(n.frames, mean = subj.mean, sd = subj.sd.1)
    v <- rnorm(n.frames, mean = subj.mean, sd = subj.sd.2)

    # Autoregressive signal with trend
    for (t in 2:n.frames) {

      if (t == 1) {

        mc[t] <- rnorm(1, mean = subj.mean, sd = subj.sd.2)
        imu[t] <- rnorm(1, mean = subj.mean, sd = subj.sd.1)
      }

      # mc[t] <- phi.2[1] * mc[t-1] + v[t] + trend.2*t
      mc[t] <- v[t]
      imu[t] <- phi.1[1] * imu[t-1] + w[t] + trend.1*t # offset

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

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "data_nonstat.rds"))


# ------------------------------------------------------------------------------
# Log-normal error data (no bias, constant variance, no trend) -----------------
# ------------------------------------------------------------------------------

device <- c()
value <- c()
subjectID <- c()

for (subj.idx in 1:n.subj) {

  value.subj <- c()
  device.subj <- c()
  subjectID.subj <- c()

  # Sample subjectwise parameters
  subj.mean <- rnorm(1)
  subj.sd.1 <- runif(1)
  subj.sd.2 <- runif(1)

  for (stride.idx in 1:(n.strides)) {

    mc <- rlnorm(n.frames, meanlog = subj.mean, sdlog = subj.sd.2) # rnorm(n.frames, mean = subj.mean, sd = subj.sd.2)
    imu <- rnorm(n.frames, mean = subj.mean, sd = subj.sd.1)

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

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "log_normal.rds"))


# ------------------------------------------------------------------------------
# Shock (spike) data (normal error) -----------------------------------------
# ------------------------------------------------------------------------------

device <- c()
value <- c()
subjectID <- c()

for (subj.idx in 1:n.subj) {

  value.subj <- c()
  device.subj <- c()
  subjectID.subj <- c()

  # Sample subjectwise parameters
  subj.sd <- 0.05 # runif(1, 0.1, 0.2)

  for (stride.idx in 1:(n.strides)) {

    # Error terms
    w <- rnorm(n.frames, mean = 0, sd = subj.sd)
    v <- rnorm(n.frames, mean = 0, sd = subj.sd)

    t <- seq(0, 2*pi, 0.0625)

    mc <- sin(t) + rnorm(n.frames, sd = subj.sd)
    imu <- 2 * sin(t) + rnorm(n.frames, sd = subj.sd)

    # Add shock to the curves of a subject
    if (subj.idx == 3) { # | subj.idx == 5

      tmp <- round(runif(1, min = 5, max = 7))
      imu[tmp] <- imu[tmp] * rnorm(1, mean = 5)
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

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "data_shock.rds"))

