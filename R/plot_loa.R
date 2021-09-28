plot_loa <- function (data, floa.point, floa.roislien, floa.lenhoff) {
  # plot_loa() function arguments:
  # 1. floa: Limits of agreement
  # + a. floa (rcb method)
  # + b. floa.point # requires: data.frame(t(floa.point))
  # 2. central.tendency: Central tendency parameter
  # + a. mean
  # + b. median

  # # floa variables need to be arranged in columns
  # if (dim(floa)[1] < dim(floa)[2]) {
  #   floa <- t(floa)
  # }
  #
  # if (!is.data.frame(floa)) {
  #   floa <- as.data.frame(floa)
  # }

  floa.point <- data.frame(t(floa.point))
  floa.roislien <- data.frame(t(floa.roislien))
  floa.lenhoff <- data.frame(t(floa.lenhoff))

  # Prepare data for ggploting -------------------------------------------------
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


  # For line graphs, the data points must be grouped so that it knows which points to connect.
  # In this case, it is simple -- all points should be connected, so group=1.
  # When more variables are used and multiple lines are drawn, the grouping for lines is usually done by variable.
  PLOT.DIFF <- ggplot(data = device.diff, aes(x = frame, y = value)) +
    geom_line(data = device.diff,
              aes(group = strideID),
              alpha = 0.25) +
    scale_color_grey(start = 0.8, end = 0.2) +
    # Plot lower limit of agreement
    geom_line(data = floa.lenhoff,
              aes(x = seq(0, 100), y = upper, col = "red", group = 1),
              linetype = "solid",
              size = 1.5,
              colour = "#D55E00") +
    geom_line(data = floa.lenhoff,
              aes(x = seq(0, 100), y = lower, col = "red", group = 1),
              linetype = "solid",
              size = 1.5,
              colour = "#D55E00") +
    geom_line(data = floa.roislien,
              aes(x = seq(0, 100), y = upper),
              linetype = "solid",
              size = 1.5,
              colour = "#56B4E9") +
    geom_line(data = floa.roislien,
              aes(x = seq(0, 100), y = lower),
              linetype = "solid",
              size = 1.5,
              colour = "#56B4E9") +
    geom_line(data = floa.point,
              aes(x = seq(0, 100), y = upper),
              linetype = "dotted",
              size = 1.5,
              colour = "#009E73") +
    geom_line(data = floa.point,
              aes(x = seq(0, 100), y = lower),
              linetype = "dotted",
              size = 1.5,
              colour = "#009E73") +
    scale_y_continuous(limits = c(-5, 5)) +
    labs(x = "Time-normalized signal [%]", y = "Difference") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 20),
          axis.title.x = element_text(size = 22),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 22),
          legend.position = "none")

  PLOT.DIFF

}
