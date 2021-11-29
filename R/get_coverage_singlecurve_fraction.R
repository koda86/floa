get_coverage_singlecurve_fraction <- function (curve, floa) {
  # Curve is a numerical vector
  # floa is a "matrix" "array" containing "lower" and "upper" limits

  # Calculate coverage (curve points within the percentile boundaries)
  # ----------------------------------------------------------------------------
  lwr.bnd <- floa[grep("low", rownames(floa)), ]
  upr.bnd <- floa[grep("up", rownames(floa)), ]

  outside <- 0

  # Compare difference curves with upper and lower boundaries
  below.thresh <- curve < lwr.bnd
  above.thresh <- curve > upr.bnd

  points.outside <- sum(above.thresh) + sum(below.thresh)

  # Divide by 101 to adjust for 101 data points
  coverage <- 1 - (points.outside / 101)
  coverage <- round(coverage, digits = 2)

  return(coverage)
}
