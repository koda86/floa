example_data <- function(dat, dir.data) {

  # ****************************************************************************
  # Select one of the data sets prepared in 'simulate_data.R'
  # ****************************************************************************

  if (dat == "imu_mc") {

    data <- readRDS(paste0(dir.data, "/", "imu_mc.rds"))

  } else if (dat == "smooth_realistic") {

    data <- readRDS(paste0(dir.data, "/", "smooth_realistic.rds"))

  } else if (dat == "non_gaussian") {

    data <- readRDS(paste0(dir.data, "/", "non_gaussian.rds"))

  } else if (dat == "shock") {

    data <- readRDS(paste0(dir.data, "/", "shock.rds"))

  } else if (dat == "shift") {

    data <- readRDS(paste0(dir.data, "/", "shift.rds"))
  }
}
