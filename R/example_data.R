example_data <- function(dat, dir.data) {

  if (dat == "imu_mc") {

    # IMU vs. MC validation data ---------------------------------------------------
    data <- readRDS(paste0(dir.data, "/", "data.rds"))

    # # Intermediate step: Fit functions to discrete time series data ----------------
    # # Thus far, a transformation of time series data to functional data (Fourier,
    # # Splines etc.) is not implemented.
    # fd.basis <- create.fourier.basis(nbasis=50) # Plateau around 50 basis vectors
    # fda.delta <- fdaDelta(data, fd.basis) # Fit Fourier, returns delta curves (mean, sd)

  } else if (dat == "log_normal") {

    data <- readRDS(paste0(dir.data, "/", "log_normal.rds"))

  } else if (dat == "bias") {

    data <- readRDS(paste0(dir.data, "/", "bias.rds"))

  } else if (dat == "non_stationary") {

    # Simulation outsourced to script 'simulate_data'
    data <- readRDS(paste0(dir.data, "/", "data_nonstat.rds"))

  } else if (dat == "shock") {

    # Simulation outsourced to script 'simulate_data'
    data <- readRDS(paste0(dir.data, "/", "data_shock.rds"))
  }

  return(data)

}
