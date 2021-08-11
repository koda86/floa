get_coverage_fraction <- function (data, floa) {

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
  inside.thresh.perc <- c()

  # Account for stride indices not starting at 1
  stride.indices <- unique(data$strideID)

  for (stride.idx in stride.indices){

    tmp <- subset(device.diff, strideID == stride.idx)

    # Compare difference curves with upper and lower boundaries
    below.thresh <- tmp$value < lwr.bnd
    above.thresh <- tmp$value > upr.bnd

    # Percentage of points outside limits
    inside <- below.thresh == above.thresh
    inside.thresh.perc <- c(inside.thresh.perc,
                             round(mean(sum(inside[TRUE]) / length(tmp$value)), 2))

    # percent.out <- round(mean(outside.thresh.perc), 2) # Percentage of points outside limits
  }

  coverage <- round(mean(inside.thresh.perc), 2)

  return(coverage)
}
