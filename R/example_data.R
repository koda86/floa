example_data <- function(dat, dir.data) {

  if (dat == "imu_mc") {

    # IMU vs. MC validation data ---------------------------------------------------
    data <- readRDS(paste0(dir.data, "/", "data.rds"))

    # # Intermediate step: Fit functions to discrete time series data ----------------
    # # Thus far, a transformation of time series data to functional data (Fourier,
    # # Splines etc.) is not implemented.
    # fd.basis <- create.fourier.basis(nbasis=50) # Plateau around 50 basis vectors
    # fda.delta <- fdaDelta(data, fd.basis) # Fit Fourier, returns delta curves (mean, sd)
  }



  # # Surrogate data ---------------------------------------------------------------
  #
  # # ARIMA
  # set.seed(100000)
  # t <- 1:500
  # alpha <- 1
  # beta <- 0
  # theta <- 0.8
  # ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t)
  #
  # # Fourier transform based


}
