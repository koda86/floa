example_data <- function(dat, dir.data) {

  if (dat == "imu_mc") {

    # IMU vs. MC validation data ---------------------------------------------------
    data <- readRDS(paste0(dir.data, "/", "data.rds"))

    # # Intermediate step: Fit functions to discrete time series data ----------------
    # # Thus far, a transformation of time series data to functional data (Fourier,
    # # Splines etc.) is not implemented.
    # fd.basis <- create.fourier.basis(nbasis=50) # Plateau around 50 basis vectors
    # fda.delta <- fdaDelta(data, fd.basis) # Fit Fourier, returns delta curves (mean, sd)

  } else if (dat == "arima") {

    # Simulated data ARIMA ---------------------------------------------------------

    set.seed(123)
    t <- 1:100
    alpha <- 6 # mean
    beta <- 0 # deterministic time-dependent trend
    theta <- 0.8
    mc <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))

    set.seed(123)
    alpha <- 6 # mean
    beta <- 0 # deterministic time-dependent trend
    theta <- 0.8
    imu <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))

    n.subj <- 11
    n.strides <- 768
    n.devices <- 2
    n.frames <- 100

    subjectID <- rep(1:n.subj, n.strides * n.devices * n.frames)
    strideID <- 1
    frames <- rep(0:n.frames, times = n.strides * n.devices)

    data <- cbind(1, 1, 1, 1, frames)

    plot(mc)

    print("ARIMA")

  } else if (dat == "fft") {

    # Fourier transform based

    print("fft")
  }

}
