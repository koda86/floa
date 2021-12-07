example_data <- function(dat, dir.data) {

  # Simulation outsourced to script 'simulate_data'

  if (dat == "imu_mc") {

    # IMU vs. MC validation data
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

  return(data)

}
