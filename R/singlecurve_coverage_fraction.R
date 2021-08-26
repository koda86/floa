crossval_coverage_fraction <- function (data, n.boot) {

  # ####################################################################
  # Leave-one (subject) out method to estimate the achieved coverage
  # See e. g. Lenhoff et al. (1999)
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
  #
  # Output:
  #   * Coverage levels [%] across n=length(subjectID) iterations
  # ####################################################################

  n.curves <- unique(data$strideID)

  cover.cross.rcb.v1   <- vector(mode = "list", length = length(n.curves))
  cover.cross.rcb.v2   <- vector(mode = "list", length = length(n.curves))
  cover.cross.rcb.v3   <- vector(mode = "list", length = length(n.curves))
  cover.cross.point    <- vector(mode = "list", length = length(n.curves))
  cover.cross.roislien <- vector(mode = "list", length = length(n.curves))

  for (curve.idx in n.curves) {

    # Calculate FLoA with one curve left out
    # --------------------------------------------------------------------
    # Leave curve at curve.idx out
    data.one.out <- subset(data, strideID != curve.idx)

    floa.point    <- floa_point(data.one.out)
    floa.v1       <- floa_rcb(data.one.out, n.boot, ver = "v1")
    floa.v2       <- floa_rcb(data.one.out, n.boot, ver = "v2")
    floa.v3       <- floa_rcb(data.one.out, n.boot, ver = "v3")
    floa.roislien <- floa_roislien(data.one.out, n.boot)

    # Get coverage for the left out curve
    # --------------------------------------------------------------------
    data.subset <- subset(data, strideID == curve.idx)

    cover.cross.rcb.v1[curve.idx] <- get_coverage_fraction(data.subset, floa.v1)

    # plot(floa.v1["mean", ], type = "l", col = "red", ylim = c(-1, 1))
    # lines(floa.v1["upper", ], col = "red")
    # lines(floa.v1["lower", ], col = "red")
    # lines(tmp$value)

    cover.cross.rcb.v2[curve.idx] <- get_coverage_fraction(data.subset, floa.v2)

    cover.cross.rcb.v3[curve.idx] <- get_coverage_fraction(data.subset, floa.v3)

    cover.cross.roislien[curve.idx] <- get_coverage_fraction(data.subset, floa.roislien)

    # plot(floa.roislien["mean", ], type = "l", col = "red", ylim = c(-1, 1))
    # lines(floa.roislien["upper", ], col = "red")
    # lines(floa.roislien["lower", ], col = "red")
    # lines(tmp$value)

    cover.cross.point[curve.idx] <- get_coverage(data.subset, floa.point)
  }

  cover.cross <- cbind(cover.cross.rcb.v1,
                       cover.cross.rcb.v2,
                       cover.cross.rcb.v3,
                       cover.cross.roislien,
                       cover.cross.point)

  return(cover.cross)
}
