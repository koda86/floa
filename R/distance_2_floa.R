# Average distance to the outer point of the curve set
distance_2_floa <- function (data, floa) {

  # floa variables need to be arranged in columns
  if (dim(floa)[1] < dim(floa)[2]) {
    floa <- t(floa)
  }

  if (!is.data.frame(floa)) {
    floa <- as.data.frame(floa)
  }

  device1 <- data.frame(subset(data, device == "IMU")$value)
  device2 <- data.frame(subset(data, device == "MC")$value)
  device.diff <- device1 - device2
  colnames(device.diff)[1] <- "value"

  device.diff$frame <- seq(0, 100)
  n.frames <- length(unique(data$frame))
  n.strides <- length(unique(data$strideID))
  n.subjects <- length(unique(data$subjectID))
  strides.per.subject <- length(unique(data$strideID)) / n.subjects
  device.diff$strideID <- as.factor(rep(1:n.strides, each = 101))
  device.diff$subjectID <- as.factor(rep(1:n.subjects, each = strides.per.subject * n.frames))

  upr.dist <- vector(mode = "list", length = n.frames)
  lwr.dist <- vector(mode = "list", length = n.frames)
  for (frame.idx in seq(0, 100)) {
    data.per.frame <- subset(device.diff, subset = frame == frame.idx)

    curve.extrema <- range(data.per.frame$value)
    curve.max <- max(curve.extrema)
    curve.min <- min(curve.extrema)

    # Account for the loop index starting at 0
    frame.idx.correct <- ifelse(frame.idx == 0, frame.idx + 1, frame.idx)

    floa.upr <- floa$upper[frame.idx + 1]
    floa.lwr <- floa$lower[frame.idx + 1]

    upr.dist[[frame.idx.correct]] <- floa.upr - curve.max
    lwr.dist[[frame.idx.correct]] <- floa.lwr - curve.min
  }

  distance <- data.frame(unlist(upr.dist),
                         unlist(lwr.dist))

  colnames(distance) <- c("upr.dist", "lwr.dist")

  return(distance)
}
