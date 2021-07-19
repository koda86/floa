get_coverage <- function (floa.boot.percentiles.intrp) {

  # Calculate coverage (entire curves within the percentile boundaries)

  lwr.bnd <- floa.boot.percentiles.intrp[1, ]
  upr.bnd <- floa.boot.percentiles.intrp[3, ]

  n.strides <- length(unique(device.diff$strideID))
  outside <- 0

  for (stride.idx in 1:n.strides){

    tmp <- subset(device.diff, strideID == stride.idx)

    # Compare difference curves with upper and lower boundaries
    below.thresh <- tmp$value < lwr.bnd
    above.thresh <- tmp$value > upr.bnd

    points.outside <- sum(above.thresh) + sum(below.thresh)

    if (points.outside > 0) {
      outside <- outside + 1
    }
  }

  coverage <- 1 - (outside / n.strides)

  return(coverage)
}
