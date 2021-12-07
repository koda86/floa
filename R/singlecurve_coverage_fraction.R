singlecurve_coverage_fraction <- function (data, n.boot) {

  # --------------------------------------------------------------------
  # Leave-one (curve) out method to estimate the uncertainty in the
  # achieved coverage (see Lenhoff et al. (1999))
  # --------------------------------------------------------------------

  # Convert continuous strideID to identical strideID's for repeated measures across subjects
  n.subj <- length(unique(data$subjectID))
  n.devices <- length(unique(data$device))
  strides.per.subject <- length(unique(data$strideID)) / length(unique(data$subjectID))
  n.frames <- length(unique(data$frame))
  data$strideID.rep <- rep(1:strides.per.subject, each = n.frames * n.devices, times = n.subj)

  n.curves <- unique(data$strideID)

  cover.cross.point     <- vector(mode = "list", length = length(n.curves))
  cover.cross.roislien  <- vector(mode = "list", length = length(n.curves))
  cover.cross.boot      <- vector(mode = "list", length = length(n.curves))

  for (curve.idx in n.curves) {
    # Calculate LoA with one curve left out ------------------------------
    data.one.out <- subset(data, strideID != curve.idx)

    floa.point    <- floa_point(data.one.out)
    floa.roislien <- floa_roislien(data.one.out)
    floa.boot     <- floa_boot(data,
                               k_reihe = 50,
                               n.boot = n.boot,
                               band = "prediction",
                               cp.begin = 0,
                               alpha = 0.05)

    # Plot left out curve vs. various methods ----------------------------
    data.subset <- subset(data, strideID == curve.idx)

    device1     <- data.frame(subset(data.subset, device == "IMU"))
    device2     <- data.frame(subset(data.subset, device == "MC"))
    device.diff <- device1$value - device2$value

    # Get coverage for the left out (difference) curve
    # --------------------------------------------------------------------
    cover.cross.point[curve.idx]    <- get_coverage_singlecurve_fraction(device.diff, floa.point)
    cover.cross.roislien[curve.idx] <- get_coverage_singlecurve_fraction(device.diff, floa.roislien)
    cover.cross.boot[curve.idx]     <- get_coverage_singlecurve_fraction(device.diff, floa.boot)
  }

  cover.cross <- cbind(
    unlist(cover.cross.point),
    unlist(cover.cross.roislien),
    unlist(cover.cross.boot)
    )

  return(cover.cross)
}
