example_data <- function(dat, dir.data) {

  # Simulation outsourced to script 'simulate_data'

  if (dat == "imu_mc") {

    # IMU vs. MC validation data ---------------------------------------------------
    data <- readRDS(paste0(dir.data, "/", "imu_mc.rds")) # data.rds contains the unbalanced data

    # # Intermediate step: Fit functions to discrete time series data ----------------
    # # Thus far, a transformation of time series data to functional data (Fourier,
    # # Splines etc.) is not implemented.
    # fd.basis <- create.fourier.basis(nbasis=50) # Plateau around 50 basis vectors
    # fda.delta <- fdaDelta(data, fd.basis) # Fit Fourier, returns delta curves (mean, sd)

  } else if (dat == "smooth_realistic") {

    data <- readRDS(paste0(dir.data, "/", "smooth_realistic.rds"))

  } else if (dat == "smooth") {

    data <- readRDS(paste0(dir.data, "/", "smooth.rds"))

  } else if (dat == "smooth_trend") {

    data <- readRDS(paste0(dir.data, "/", "smooth_trend.rds"))

  } else if (dat == "smooth_biased") {

    data <- readRDS(paste0(dir.data, "/", "smooth_biased.rds"))

  } else if (dat == "non_gaussian") {

    data <- readRDS(paste0(dir.data, "/", "non_gaussian.rds"))

  } else if (dat == "shock") {

    data <- readRDS(paste0(dir.data, "/", "shock.rds"))

  } else if (dat == "shift") {

    data <- readRDS(paste0(dir.data, "/", "shift.rds"))
  }

  # # Convert continuous strideID to identical strideID's for repeated measures across subjects
  # # Needed for floa_point()
  # n.subj <- length(unique(data$subjectID))
  # n.devices <- length(unique(data$device))
  # strides.per.subject <- length(unique(data$strideID)) / length(unique(data$subjectID))
  # n.frames <- length(unique(data$frame))
  # data$strideID.rep <- rep(1:strides.per.subject, each = n.frames * n.devices, times = n.subj)

  return(data)

}
