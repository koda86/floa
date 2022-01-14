coverage_loocv <- function (data, n.boot) {

  # ****************************************************************************
  # Leave-one (curve) out method to estimate the uncertainty across methods
  # ****************************************************************************

  # Convert continuous strideID to identical strideID's for repeated measures
  # across subjects
  n.subj <- length(unique(data$subjectID))
  n.devices <- length(unique(data$device))
  strides.per.subject <- length(unique(data$strideID)) / length(unique(data$subjectID))
  n.frames <- length(unique(data$frame))
  data$strideID.rep <- rep(1:strides.per.subject,
                           each = n.frames * n.devices,
                           times = n.subj)

  n.curves <- unique(data$strideID)

  # Calculate LoA with one curve left out ------------------------------
  coverage.point    <- vector(mode = "list", length = length(n.curves))
  coverage.roislien <- vector(mode = "list", length = length(n.curves))
  coverage.boot.rep <- vector(mode = "list", length = length(n.curves))
  coverage.boot.iid <- vector(mode = "list", length = length(n.curves))

  for (curve.idx in n.curves) {

    data.one.out <- subset(data, strideID != curve.idx)

    floa.point    <- floa_point(data.one.out)
    floa.roislien <- floa_roislien(data.one.out)
    floa.boot.rep <- floa_boot(data,
                               k.coef = 50,
                               n.boot = n.boot,
                               band = "prediction",
                               cp.begin = 0,
                               alpha = 0.05,
                               iid = FALSE)
    floa.boot.iid <- floa_boot(data,
                               k.coef = 50,
                               n.boot = n.boot,
                               band = "prediction",
                               cp.begin = 0,
                               alpha = 0.05,
                               iid = TRUE)

    # Get coverage for the left out (difference) curve -------------------
    data.subset <- subset(data, strideID == curve.idx)

    device1     <- data.frame(subset(data.subset, device == "IMU"))
    device2     <- data.frame(subset(data.subset, device == "MC"))
    device.diff <- device1$value - device2$value

    coverage.point[curve.idx]      <- points_within_limits(device.diff, floa.point)
    coverage.roislien[curve.idx]   <- points_within_limits(device.diff, floa.roislien)
    coverage.boot.rep[curve.idx]   <- points_within_limits(device.diff, floa.boot.rep)
    coverage.boot.iid[curve.idx]   <- points_within_limits(device.diff, floa.boot.iid)
  }

  coverage <- cbind(
    unlist(coverage.point),
    unlist(coverage.roislien),
    unlist(coverage.boot.rep),
    unlist(coverage.boot.iid)
    )

  return(coverage)
}
