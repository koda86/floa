coverage_loocv <- function (data, n.boot) { # singlecurve_coverage_fraction

  # ****************************************************************************
  # Leave-one (curve) out method to estimate the uncertainty in the achieved
  # coverage (see Lenhoff et al. (1999))
  # ****************************************************************************

  # Convert continuous strideID to identical strideID's for repeated measures
  # across subjects
  n.subj <- length(unique(data$subjectID))
  n.devices <- length(unique(data$device))
  strides.per.subject <- length(unique(data$strideID)) / length(unique(data$subjectID))
  n.frames <- length(unique(data$frame))
  data$strideID.rep <- rep(1:strides.per.subject, each = n.frames * n.devices, times = n.subj)

  n.curves <- unique(data$strideID)

  cover.cross.fraction.point    <- vector(mode = "list", length = length(n.curves))
  cover.cross.fraction.roislien <- vector(mode = "list", length = length(n.curves))
  cover.cross.fraction.boot.rep <- vector(mode = "list", length = length(n.curves))
  cover.cross.fraction.boot.iid <- vector(mode = "list", length = length(n.curves))
  # Calculate LoA with one curve left out ------------------------------
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


    # Plot left out curve vs. various methods ----------------------------
    data.subset <- subset(data, strideID == curve.idx)

    device1     <- data.frame(subset(data.subset, device == "IMU"))
    device2     <- data.frame(subset(data.subset, device == "MC"))
    device.diff <- device1$value - device2$value


    # Get coverage for the left out (difference) curve -------------------
    cover.cross.fraction.point[curve.idx]      <- get_coverage_singlecurve_fraction(device.diff, floa.point)
    cover.cross.fraction.roislien[curve.idx]   <- get_coverage_singlecurve_fraction(device.diff, floa.roislien)
    cover.cross.fraction.boot.rep[curve.idx]   <- get_coverage_singlecurve_fraction(device.diff, floa.boot.rep)
    cover.cross.fraction.boot.iid[curve.idx]   <- get_coverage_singlecurve_fraction(device.diff, floa.boot.iid)
  }

  cover.cross.fraction <- cbind(
    unlist(cover.cross.fraction.point),
    unlist(cover.cross.fraction.roislien),
    unlist(cover.cross.fraction.boot.rep),
    unlist(cover.cross.fraction.boot.iid)
    )

  return(cover.cross.fraction)
}
