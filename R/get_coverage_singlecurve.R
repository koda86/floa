get_coverage_singlecurve <- function (curve, floa) {
  # Curve is a numerical vector
  # floa is a "matrix" "array" containing "lower" and "upper" limits

  # Calculate coverage (entire curves within the percentile boundaries)
  # ----------------------------------------------------------------------------
  lwr.bnd <- floa["lower", ]
  upr.bnd <- floa["upper", ]

  outside <- 0

  # Compare curves with upper and lower boundaries
  below.thresh <- curve < lwr.bnd
  above.thresh <- curve > upr.bnd

  points.outside <- sum(above.thresh) + sum(below.thresh)

  coverage <- ifelse(points.outside > 0, 0, 1)

  return(coverage)
}
