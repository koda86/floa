singlecurve_coverage_fraction <- function (data, n.boot) {

  # ####################################################################
  # Leave-one (curve) out method to estimate the uncertainty in the
  # achieved coverage (see Lenhoff et al. (1999))
  # ####################################################################
  #
  # Currently, different versions of the sampling process in draw_clusters()
  # (nested in floa_rcb()) are implemented:
  #
  # v1 : n = length(subjects) random strides from all strides
  # v2 : One random stride per subject
  # v3 : Fetch a SINGLE random stride from all strides
  # v4 : Roislien approach (Get one random stride from each subject ONCE and boot-
  #      strap the resulting sample (of length (n=length(subjects))
  # v5 : Pointwise B&A limits (SD from linear mixed effects model)
  # ####################################################################

  # Convert continuous strideID to identical strideID's for repeated measures across subjects
  # Needed for floa_point()
  n.subj <- length(unique(data$subjectID))
  n.devices <- length(unique(data$device))
  strides.per.subject <- length(unique(data$strideID)) / length(unique(data$subjectID))
  n.frames <- length(unique(data$frame))
  data$strideID.rep <- rep(1:strides.per.subject, each = n.frames * n.devices, times = n.subj)

  n.curves <- unique(data$strideID)

  cover.cross.rcb.v1   <- vector(mode = "list", length = length(n.curves))
  cover.cross.rcb.v2   <- vector(mode = "list", length = length(n.curves))
  cover.cross.rcb.v3   <- vector(mode = "list", length = length(n.curves))
  cover.cross.point    <- vector(mode = "list", length = length(n.curves))
  cover.cross.roislien <- vector(mode = "list", length = length(n.curves))
  for (curve.idx in n.curves) {
    # Calculate FLoA with one curve left out -----------------------------
    data.one.out <- subset(data, strideID != curve.idx)

    floa.point    <- floa_point(data.one.out)
    floa.v1       <- floa_rcb(data.one.out, n.boot, ver = "v1")
    floa.v2       <- floa_rcb(data.one.out, n.boot, ver = "v2")
    floa.v3       <- floa_rcb(data.one.out, n.boot, ver = "v3")
    floa.roislien <- floa_roislien(data.one.out)

    # Plot left out curve vs. various FLoA methods -----------------------
    data.subset <- subset(data, strideID == curve.idx)

    device1 <- data.frame(subset(data.subset, device == "IMU"))
    device2 <- data.frame(subset(data.subset, device == "MC"))
    device.diff <- device1$value - device2$value

    # Get coverage for the left out (difference) curve
    # --------------------------------------------------------------------
    cover.cross.rcb.v1[curve.idx]   <- get_coverage_singlecurve_fraction(device.diff, floa.v1)
    cover.cross.rcb.v2[curve.idx]   <- get_coverage_singlecurve_fraction(device.diff, floa.v2)
    cover.cross.rcb.v3[curve.idx]   <- get_coverage_singlecurve_fraction(device.diff, floa.v3)
    cover.cross.roislien[curve.idx] <- get_coverage_singlecurve_fraction(device.diff, floa.roislien)
    cover.cross.point[curve.idx]    <- get_coverage_singlecurve_fraction(device.diff, floa.point)
  }

  cover.cross <- cbind(unlist(cover.cross.rcb.v1),
                       unlist(cover.cross.rcb.v2),
                       unlist(cover.cross.rcb.v3),
                       unlist(cover.cross.roislien),
                       unlist(cover.cross.point)
                       )

  return(cover.cross)
}
