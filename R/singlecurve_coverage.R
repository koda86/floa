singlecurve_coverage <- function (data, n.boot) {
  # ####################################################################
  # Leave-one (curve) out method to estimate the uncertainty in the
  # achieved coverage (see Lenhoff et al. (1999))
  # --------------------------------------------------------------------
  #
  # Currently, different versions of the sampling process in draw_clusters()
  # (nested in floa_rcb()) are implemented:
  #
  # v1 : n = length(subjects) random strides from all strides
  # v2 : One random stride per subject
  # v3 : Fetch a SINGLE random stride from all strides
  # v4 : Roislien approach (Get one random stride from each subject ONCE
  #      and bootstrap the resulting sample
  # v5 : Pointwise B&A limits
  # ####################################################################
  n.curves <- unique(data$strideID)

  cover.cross.point    <- vector(mode = "list", length = length(n.curves))
  cover.cross.roislien <- vector(mode = "list", length = length(n.curves))
  cover.cross.lenhoff <- vector(mode = "list", length = length(n.curves))

  for (curve.idx in n.curves) {
    # Calculate FLoA with one curve left out -----------------------------
    data.one.out <- subset(data, strideID != curve.idx)

    floa.point    <- floa_point(data.one.out)
    floa.roislien <- floa_roislien(data.one.out)
    floa.lenhoff  <- floa_lenhoff(data,
                                  k_reihe = 50,
                                  n.boot = n.boot,
                                  band = "prediction",
                                  cp.begin = 0,
                                  alpha = 0.05)

    # Plot left out curve vs. various FLoA methods -----------------------
    data.subset <- subset(data, strideID == curve.idx)

    device1 <- data.frame(subset(data.subset, device == "IMU"))
    device2 <- data.frame(subset(data.subset, device == "MC"))
    device.diff <- device1$value - device2$value

    # Get coverage for the left out (difference) curve -------------------
    cover.cross.point[curve.idx]      <- get_coverage_singlecurve(device.diff, floa.point)
    cover.cross.roislien[curve.idx]   <- get_coverage_singlecurve(device.diff, floa.roislien)
    cover.cross.lenhoff[curve.idx]    <- get_coverage_singlecurve(device.diff, floa.lenhoff)
  }

  cover.cross <- cbind(
                      unlist(cover.cross.point),
                      unlist(cover.cross.roislien),
                      unlist(cover.cross.lenhoff)
                      )

  return(cover.cross)
}
