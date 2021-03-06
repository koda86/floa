plot_loa <- function (data, floa) {

  # Prepare data for ggploting -------------------------------------------------

  # "imu_mc" NEEDS BALANCED DATA!!

  device1 <- data.frame(subset(data, device == "IMU")$value)
  device2 <- data.frame(subset(data, device == "MC")$value)

  device.diff <- device1 - device2

  colnames(device.diff)[1] <- "value"

  device.diff$frame <- seq(0, 100)
  n.frames <- length(unique(data$frame))
  n.strides <- length(unique(data$strideID))
  n.subjects <- length(unique(data$subjectID))
  strides.per.subject <- length(unique(data$strideID)) / n.subjects
  device.diff$strideID <- as.factor(rep(1:n.strides, each = 101)) # as.factor(seq(0, n.strides, by = n.frames)) # as.factor(rep(seq(1, strides.per.subject), each = 101))
  device.diff$subjectID <- as.factor(rep(1:n.subjects, each = strides.per.subject * n.frames))

  # For line graphs, the data points must be grouped so that it knows which points to connect.
  # In this case, it is simple -- all points should be connected, so group=1.
  # When more variables are used and multiple lines are drawn, the grouping for lines is usually done by variable.
  PLOT.DIFF <- ggplot(data = device.diff, aes(x = frame, y = value, color = subjectID, group = strideID)) +
    geom_line() +
    scale_color_grey(start = 0.8, end = 0.2) +
    geom_line(data = floa,
              aes(x = seq (0,100), y = X1, col = "red", group = 1),
              linetype = "solid",
              size = 3,
              colour = "red") +
    geom_line(data = floa,
              aes(x = seq (0,100), y = X2, col = "red", group = 1),
              linetype = "dotted",
              size = 3,
              colour = "red") +
    geom_line(data = floa,
              aes(x = seq (0,100), y = X3, col = "red", group = 1),
              linetype = "solid",
              size = 3,
              colour = "red") +
    scale_y_continuous(limits = c(min(device.diff$value), max(device.diff$value))) +
    labs(x = "Time-normalized signal [%]", y = "Difference") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 20),
          axis.title.x = element_text(size = 22),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 22),
          legend.position = "none")

  PLOT.DIFF

}
