floa_rcb <- function(data, n.boot, plt) { # , fd.basis

  library(ggplot2)

  # ----------------------------------------------------------------------------
  # Randomized Cluster Bootstrap
  #
  # Add description here
  # draw_clusters(): Draw a single random curve form a single random subject
  # ----------------------------------------------------------------------------

  clust.boot.agg <- c()

  for (boot.idx in 1:n.boot) {

    # draw_clusters returns difference curves (device1 - device2)
    clust.boot.agg[[boot.idx]] <- draw_clusters(data) # , fd.basis
  }

  # Arrange curves in matrix (row-wise) to facilitate computing percentiles
  clust.agg.intrp <- matrix(unlist(clust.boot.agg),
                      ncol  = 101, # length(fd.basis$names)
                      byrow = TRUE)

  # clust.agg.intrp <- t(sapply(apply(t(clust.agg), 2, approx, n = 101), "[[", "y"))

  # Calculate percentiles ------------------------------------------------------
  floa.boot.percentiles <- c()

  for (i in 1:ncol(clust.agg.intrp)) {

    # TODO: Bias correction useful/necessary?
    floa.boot.percentiles <- c(floa.boot.percentiles, quantile(clust.agg.intrp[, i], probs = c(0.025, 0.5, 0.975)))
  }

  perc2.5 <- floa.boot.percentiles[which(names(floa.boot.percentiles) == "2.5%")]
  perc50 <- floa.boot.percentiles[which(names(floa.boot.percentiles) == "50%")]
  perc97.5 <- floa.boot.percentiles[which(names(floa.boot.percentiles) == "97.5%")]

  # Interpolate to 101 data points -------------------------------------------
  floa.boot.percentiles.intrp <- rbind(approx(perc2.5, n = 101)$y, approx(perc50, n = 101)$y, approx(perc97.5, n = 101)$y)

  # Prepare data for ggploting
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


  if (plt) {

    floa <- data.frame(t(floa.boot.percentiles.intrp))

    # For line graphs, the data points must be grouped so that it knows which points to connect.
    # In this case, it is simple -- all points should be connected, so group=1.
    # When more variables are used and multiple lines are drawn, the grouping for lines is usually done by variable.
    PLOT.DIFF <- ggplot(data = device.diff, aes(x = frame, y = value, color = subjectID, group = strideID)) +
      geom_line() +
      geom_line(data = floa, aes(x = seq (0,100), y = X1, col = "red", group = 1), linetype = "dotted", size = 3) +
      geom_line(data = floa, aes(x = seq (0,100), y = X2, col = "red", group = 1), linetype = "longdash", size = 3) +
      geom_line(data = floa, aes(x = seq (0,100), y = X3, col = "red", group = 1), linetype = "dotted", size = 3) +
      scale_color_grey(start = 0.8, end = 0.2) +
      scale_y_continuous(limits = c(min(clust.agg.intrp), max(clust.agg.intrp))) +
      labs(x = "Time-normalized signal duration [%]", y = "Difference") +
      theme(axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 22),
            axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 22),
            legend.position = "none")

    PLOT.DIFF

  } else {

    print("")
  }

  return(floa.boot.percentiles.intrp)
}
