get_coverage_fraction <- function (data, floa) {

  # Get difference curves
  # ----------------------------------------------------------------------------
  device1 <- data.frame(subset(data, device == "IMU"))
  device2 <- data.frame(subset(data, device == "MC"))

  device.diff <- subset(device1, select = c(subjectID, strideID, frame))
  device.diff$value <- device1$value - device2$value


  # Calculate coverage (points within the limits)
  # ----------------------------------------------------------------------------
  lwr.bnd <- floa["lower", ]
  upr.bnd <- floa["upper", ]

  n.strides <- length(unique(device.diff$strideID))
  inside.thresh.perc <- c()
  # Account for stride indices not starting at 1
  stride.indices <- unique(data$strideID)

  for (stride.idx in stride.indices){
    curve <- subset(device.diff, strideID == stride.idx)

    # Compare difference curves with upper and lower boundaries
    below.thresh <- curve$value < lwr.bnd
    above.thresh <- curve$value > upr.bnd

    # Percentage of points outside the limits
    inside <- below.thresh == above.thresh
    inside.thresh.perc <- c(inside.thresh.perc,
                            sum(inside[TRUE]) / length(curve$value))
  }

  # Mean percentage of points outside the limits
  coverage <- round(mean(inside.thresh.perc), 3)

  return(coverage)
}
