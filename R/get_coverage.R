get_coverage <- function (data, floa.boot.percentiles.intrp) {

  # Get difference curves ------------------------------------------------------
  device1 <- data.frame(subset(data, device == "IMU"))
  device2 <- data.frame(subset(data, device == "MC"))

  device.diff <- subset(device1, select = c(subjectID, strideID, frame))
  device.diff$value <- device1$value - device2$value


  # Calculate coverage (entire curves within the percentile boundaries) --------
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
