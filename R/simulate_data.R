################################################################################
# Simulate data
################################################################################
#
# Inspired by ...
# https://ademos.people.uic.edu/Chapter23.html
# https://stats.stackexchange.com/questions/330199/simulating-drift-in-the-data
#
# Simulated data sets:
#
# 1. Smooth, wave data (normal error, constant variance, no trend): "smooth"
# 2. Biased data (constant variance, no trend): "bias"
# 3. Non-stationary data (trend, no bias) data:"non_stationary"
# 4. Data with shock peaks (no bias, no trend): "shock"
################################################################################

n.subj <- 11
n.strides <- 100
n.devices <- 2
n.frames <- 101

# Smooth wave data (constant variance, no trend) -------------------------------
device <- c()
value <- c()
subjectID <- c()

for (subj.idx in 1:n.subj) {

  value.subj <- c()
  device.subj <- c()
  subjectID.subj <- c()

  t <- seq(0, 100)

  # Subjectwise wave parameters
  offset.mean <- runif(1, min = -0.5, max = 0.5)

  a.sd <- runif(1, min = 0.05, max = 0.15) # .1
  b.sd <- runif(1, min = 0.0001, max = 0.002) # .001

  for (stride.idx in 1:(n.strides)) {

    a1.1 <- rnorm(1, mean = 3, sd = a.sd) # .1
    a1.2 <- rnorm(1, mean = 3, sd = a.sd) # .1
    a2.1 <- 0.08
    a2.2 <- 0.08
    b1.1 <- rnorm(1, mean = 0.06, sd = b.sd) # 0.001
    b1.2 <- rnorm(1, mean = 0.06, sd = b.sd) # 0.001
    b2.1 <- rnorm(1, mean = 0.58, sd = b.sd) # 0.001
    b2.2 <- rnorm(1, mean = 0.58, sd = b.sd) # 0.001
    c <- 2

    sine.1 <- a1.1 * sin(b1.1 * t) ^ (c + 3)
    sine.2 <- a2.1 * sin(b2.1 * t)
    sine.3 <- a1.2 * sin(b1.2 * t) ^ (c + 3)
    sine.4 <- a2.2 * sin(b2.1 * t)

    offset <- rnorm(1, offset.mean, 0.05)

    mc <- sine.1 + sine.2
    imu <- offset + sine.3 + sine.4

    # plot(mc, type = "l")
    # lines(imu, col = "red")

    value.subj <- c(value.subj, c(imu, mc))

    device.imu <- rep("IMU", n.frames)
    device.mc <- rep("MC", n.frames)
    device.subj <- c(device.subj, c(device.imu, device.mc))

    subjectID.subj <- c(subjectID.subj, rep(subj.idx, 2 * n.frames))
  }

  # Data is stored for long format output
  device <- c(device, device.subj)
  value <- c(value, value.subj)
  subjectID <- c(subjectID, subjectID.subj)
}

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "smooth.rds"))


# Smooth wave data with nonlinear trend ----------------------------------------
device <- c()
value <- c()
subjectID <- c()

for (subj.idx in 1:n.subj) {

  value.subj <- c()
  device.subj <- c()
  subjectID.subj <- c()

  t <- seq(0, 100)

  # Subjectwise wave parameters
  offset.mean <- runif(1, min = -0.5, max = 0.5)

  a.sd <- runif(1, min = 0.05, max = 0.15) # .1
  b.sd <- runif(1, min = 0.0001, max = 0.002) # .001

  for (stride.idx in 1:(n.strides)) {

    a1.1 <- rnorm(1, mean = 3, sd = a.sd)
    a1.2 <- rnorm(1, mean = 3, sd = a.sd)
    a2.1 <- 0.08
    a2.2 <- 0.08
    b1.1 <- rnorm(1, mean = 0.06, sd = b.sd)
    b1.2 <- rnorm(1, mean = 0.06, sd = b.sd)
    b2.1 <- rnorm(1, mean = 0.58, sd = b.sd)
    b2.2 <- rnorm(1, mean = 0.58, sd =b.sd)
    c <- 2

    offset <- rnorm(1, offset.mean, 0.05)

    sine.1 <- a1.1 * sin(b1.1 * t) ^ (c + 3)
    sine.2 <- a2.1 * sin(b2.1 * t)
    sine.3 <- a1.2 * sin(b1.2 * t) ^ (c + 3)
    sine.4 <- a2.2 * sin(b2.1 * t)

    # Create nonlinear trend
    trend <- (1 / 100000) * seq(0.5, 50.5, 0.5)^3

    mc <- sine.1 + sine.2
    imu <- offset + sine.3 + sine.4 + trend

    # plot(mc, type = "l")
    # lines(imu, col = "red")

    value.subj <- c(value.subj, c(imu, mc))

    device.imu <- rep("IMU", n.frames)
    device.mc <- rep("MC", n.frames)
    device.subj <- c(device.subj, c(device.imu, device.mc))

    subjectID.subj <- c(subjectID.subj, rep(subj.idx, 2 * n.frames))
  }

  # Data is stored for long format output
  device <- c(device, device.subj)
  value <- c(value, value.subj)
  subjectID <- c(subjectID, subjectID.subj)
}

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "smooth_trend.rds"))


# Non-gaussian (Weibull distributed) error (no trend) --------------------------
device <- c()
value <- c()
subjectID <- c()

for (subj.idx in 1:n.subj) {

  value.subj <- c()
  device.subj <- c()
  subjectID.subj <- c()

  t <- seq(0, 100)

  # Subjectwise wave parameters
  offset.mean <- runif(1, min = -0.5, max = 0.5)

  a.sd <- runif(1, min = 0.05, max = 0.15) # .1

  b.sd <- runif(1, min = 0.0001, max = 0.002)

  for (stride.idx in 1:(n.strides)) {

    a1.1 <- rnorm(1, mean = 3, sd = a.sd)
    a1.2 <- rnorm(n = 1,
                  mean = rweibull(1, shape = 1.5, scale=1) - factorial(1/1.5), # factorial() used to center around 0
                  sd = a.sd)
    a2.1 <- 0.08
    a2.2 <- 0.08
    b1.1 <- rnorm(1, mean = 0.06, sd = b.sd)
    b1.2 <- rnorm(1, mean = 0.06, sd = b.sd)
    b2.1 <- rnorm(1, mean = 0.58, sd = b.sd)
    b2.2 <- rnorm(1, mean = 0.58, sd = b.sd)
    c <- 2

    sine.1 <- a1.1 * sin(b1.1 * t) ^ (c + 3)
    sine.2 <- a2.1 * sin(b2.1 * t)
    sine.3 <- a1.2 * sin(b1.2 * t) ^ (c + 3)
    sine.4 <- a2.2 * sin(b2.1 * t)

    offset <- rnorm(1, offset.mean, 0.05)

    mc <- sine.1 + sine.2
    imu <- offset + sine.3 + sine.4

    # plot(mc, type = "l")
    # lines(imu, col = "red")

    value.subj <- c(value.subj, c(imu, mc))

    device.imu <- rep("IMU", n.frames)
    device.mc <- rep("MC", n.frames)
    device.subj <- c(device.subj, c(device.imu, device.mc))

    subjectID.subj <- c(subjectID.subj, rep(subj.idx, 2 * n.frames))
  }

  # Data is stored for long format output
  device <- c(device, device.subj)
  value <- c(value, value.subj)
  subjectID <- c(subjectID, subjectID.subj)
}

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "non_gaussian.rds"))






# # ------------------------------------------------------------------------------
# # Smooth wave data with nonlinear trend (no bias, constant variance) (2) -------
# # ------------------------------------------------------------------------------
#
# device <- c()
# value <- c()
# subjectID <- c()
#
# for (subj.idx in 1:n.subj) {
#
#   t <- seq(n.strides * n.frames)
#
#   # Subjectwise wave parameters
#   offset.mean <- runif(1, min = -0.5, max = 0.5)
#
#   # Trend across all strides of a subject
#   a1.1 <- rnorm(1, 3, .1)
#   a1.2 <- rnorm(1, 3, .1)
#   a2.1 <- 0.08
#   a2.2 <- 0.08
#   b1.1 <- rnorm(1, 0.06, .001)
#   b1.2 <- rnorm(1, 0.06, .001)
#   b2.1 <- rnorm(1, 0.58, .001)
#   b2.2 <- rnorm(1, 0.58, .001)
#   c <- 2
#
#   offset <- rnorm(1, offset.mean, 0.05)
#
#   sine.1 <- a1.1 * sin(b1.1 * t) ^ (c + 3)
#   sine.2 <- a2.1 * sin(b2.1 * t)
#   sine.3 <- a1.2 * sin(b1.2 * t) ^ (c + 3)
#   sine.4 <- a2.2 * sin(b2.1 * t)
#
#   # Create nonlinear trend
#   trend <- (3/5000000) * seq(0.5, 50.5, 0.5)^3
#   # plot(trend, type = "l")
#
#   mc.n.strides <- sine.1 + sine.2
#   imu.n.strides <- sine.3 + sine.4 # + trend # offset
#
#   plot(mc.n.strides[1:1000], type = "l")
#   lines(imu.n.strides[1:1000], col = "red")
#
#   # Sequence longer sequences  of n.strides into single strides to retain the established structure
#   stride.seq <- seq(1, length(mc.n.strides), by = n.frames)
#
#   plot(mc.n.strides[1:1500], type = "l")
#   # plot(imu.n.strides[1:1000], type = "l")
#   abline(v = stride.seq, col = "red")
#
#   value.subj <- c()
#   device.subj <- c()
#   subjectID.subj <- c()
#
#   for (stride.idx in 1:n.strides) {
#
#     if (i == 1) {
#       start <- stride.seq[stride.idx]
#       end <- stride.seq[stride.idx] + 100
#     } else {
#       start <- stride.seq[stride.idx]
#       end <- stride.seq[stride.idx] + 100
#     }
#
#     start <- stride.seq[stride.idx]
#     end <- stride.seq[stride.idx] + 100
#
#     mc <- mc.n.strides[start:end]
#     imu <- imu.n.strides[start:end]
#
#     value.subj <- c(value.subj, c(imu, mc))
#
#     device.imu <- rep("IMU", n.frames)
#     device.mc <- rep("MC", n.frames)
#     device.subj <- c(device.subj, c(device.imu, device.mc))
#
#     subjectID.subj <- c(subjectID.subj, rep(subj.idx, 2 * n.frames))
#   }
#
#   # Data is stored for long format output
#   device <- c(device, device.subj)
#   value <- c(value, value.subj)
#   subjectID <- c(subjectID, subjectID.subj)
# }
#
# strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
# frame <- rep(0:100, times = n.strides * n.devices * n.subj)
#
# data <- data.frame(device, subjectID, strideID, value, frame)
#
# saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "smooth_trend.rds"))


# ------------------------------------------------------------------------------
# Noisy wave data (normal error, constant variance, no trend) ------------------
# ------------------------------------------------------------------------------

device <- c()
value <- c()
subjectID <- c()

for (subj.idx in 1:n.subj) {

  value.subj <- c()
  device.subj <- c()
  subjectID.subj <- c()

  t <- seq(0, 100)

  # Subjectwise wave parameters
  offset.mean <- runif(1, min = -0.5, max = 0.5)
  # Subject-wise for additive errors
  subj.mean <- rnorm(1)
  subj.sd.1 <- runif(1, min = 0.04, max = 0.06)
  subj.sd.2 <- runif(1, min = 0.1, max = 0.2)

  for (stride.idx in 1:(n.strides)) {

    a1.1 <- rnorm(1, 3, .1)
    a1.2 <- rnorm(1, 3, .1)
    a2.1 <- 0.08
    a2.2 <- 0.08
    b1.1 <- rnorm(1, 0.06, .001)
    b1.2 <- rnorm(1, 0.06, .001)
    b2.1 <- rnorm(1, 0.58, .001)
    b2.2 <- rnorm(1, 0.58, .001)
    c <- 2

    # Error terms
    error.norm.1 <- rnorm(n.frames, mean = subj.mean, sd = subj.sd.1)
    error.norm.2 <- rnorm(n.frames, mean = subj.mean, sd = subj.sd.2)

    offset <- rnorm(1, offset.mean, 0.05)

    mc <- a1.1 * sin(b1.1 * t) ^ (c + 3) + a2.1 * sin(b2.1 * t) + error.norm.1
    imu <- offset + a1.2 * sin(b1.2 * t) ^ (c + 3) + a2.2 * sin(b2.1 * t) + error.norm.2

    # plot(mc, type = "l")
    # lines(imu, col = "red")

    value.subj <- c(value.subj, c(imu, mc))

    device.imu <- rep("IMU", n.frames)
    device.mc <- rep("MC", n.frames)
    device.subj <- c(device.subj, c(device.imu, device.mc))

    subjectID.subj <- c(subjectID.subj, rep(subj.idx, 2 * n.frames))
  }

  # Data is stored for long format output
  device <- c(device, device.subj)
  value <- c(value, value.subj)
  subjectID <- c(subjectID, subjectID.subj)
}

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "smooth_noise.rds"))


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

    offset <- 10

    # Autoregressive signal with trend
    for (t in 1:n.frames) {

      if (t < offset) {
        mc[t] <- v[t]
        imu[t] <- w[t]
      } else {
        mc[t] <- v[t]
        imu[t] <- phi.1[1] * imu[t-(offset-1)] + w[t] + trend.1*t
      }
    }

    value.subj <- c(value.subj, c(imu, mc))

    device.imu <- rep("IMU", n.frames)
    device.mc <- rep("MC", n.frames)
    device.subj <- c(device.subj, c(device.imu, device.mc))

    subjectID.subj <- c(subjectID.subj, rep(subj.idx, 2 * n.frames))
  }

  # Data is stored for long format output
  device <- c(device, device.subj)
  value <- c(value, value.subj)
  subjectID <- c(subjectID, subjectID.subj)
}

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "data_nonstat.rds"))


# # ------------------------------------------------------------------------------
# # Nonlinear function (Ratkowsky) -----------------------------------------------
# # ------------------------------------------------------------------------------
#
# # https://robjhyndman.com/hyndsight/simulating-from-a-specified-seasonal-arima-model/
# # library(forecast)
# model <- Arima(ts(rnorm(100), freq = 10),
#                order = c(1, 1, 1),
#                seasonal = c(1, 1, 1),
#                fixed = c(phi = 0.2, theta = -0.2, Phi = 0.3, Theta = -0.2)) # c(phi = 0.5, theta = -0.4, Phi = 0.3, Theta = -0.2)
#
# value.subj <- as.numeric(simulate(model, nsim = 202))
#
# plot(value.subj, type = "l")
#
# device <- c()
# value <- c()
# subjectID <- c()
#
# for (subj.idx in 1:n.subj) {
#
#   value.subj <- c()
#   device.subj <- c()
#   subjectID.subj <- c()
#
#   for (stride.idx in 1:(n.strides)) {
#
#     mc <- as.numeric(simulate(model, nsim = 101))
#
#     value.subj <- as.numeric(simulate(model, nsim = 202))
#
#     plot(value.subj, type = "l")
#
#     device.imu <- rep("IMU", n.frames)
#     device.mc <- rep("MC", n.frames)
#     device.subj <- c(device.subj, c(device.imu, device.mc))
#
#     subjectID.subj <- c(subjectID.subj, rep(subj.idx, 2 * n.frames))
#   }
#
#   # Data is stored for long format output
#   device <- c(device, device.subj)
#   value <- c(value, value.subj)
#   subjectID <- c(subjectID, subjectID.subj)
# }
#
# strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
# frame <- rep(0:100, times = n.strides * n.devices * n.subj)
#
# data <- data.frame(device, subjectID, strideID, value, frame)
#
# saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "data_nonstat.rds"))


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

  # Data is stored for long format output
  device <- c(device, device.subj)
  value <- c(value, value.subj)
  subjectID <- c(subjectID, subjectID.subj)
}

strideID <- rep(1:(n.strides * n.subj), each = n.devices * n.frames)
frame <- rep(0:100, times = n.strides * n.devices * n.subj)

data <- data.frame(device, subjectID, strideID, value, frame)

saveRDS(data, file = paste0("C:/Users/Daniel/Desktop/tmp/floa/R/examples/", "data_shock.rds"))

