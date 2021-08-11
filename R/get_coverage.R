get_coverage <- function (data, floa) {

  # Get difference curves
  # ----------------------------------------------------------------------------
  device1 <- data.frame(
    subset(data, device == "IMU")
    )

  device2 <- data.frame(
    subset(data, device == "MC")
    )

  device.diff <- subset(device1,
                        select = c(subjectID, strideID, frame))

  device.diff$value <- device1$value - device2$value


  # Calculate coverage (entire curves within the percentile boundaries)
  # ----------------------------------------------------------------------------
  lwr.bnd <- floa["lower", ]
  upr.bnd <- floa["upper", ]

  n.strides <- length(unique(device.diff$strideID))
  outside <- 0

  # Account for stride indices not starting at 1
  stride.indices <- unique(data$strideID)

  for (stride.idx in stride.indices){

    tmp <- subset(device.diff, strideID == stride.idx)

    # Compare difference curves with upper and lower boundaries
    below.thresh <- tmp$value < lwr.bnd
    above.thresh <- tmp$value > upr.bnd

    points.outside <- sum(above.thresh) + sum(below.thresh)

    # Check if the entire curve is within the limits
    if (points.outside > 0) {
      outside <- outside + 1
    }
  }

  coverage <- 1 - (outside / n.strides)

  return(coverage)
}
