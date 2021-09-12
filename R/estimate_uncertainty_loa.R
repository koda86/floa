# Estimate uncertainty in different LoA approaches (95% percentile intervals for the LoA)
estimate_uncertainty_loa <- function (data, n.boot) {

  floa.v2.upper <- vector(mode = "list", length = n.boot)
  floa.v2.lower <- vector(mode = "list", length = n.boot)
  floa.roislien.upper <- vector(mode = "list", length = n.boot)
  floa.roislien.lower <- vector(mode = "list", length = n.boot)
  floa.point.upper <- vector(mode = "list", length = n.boot)
  floa.point.lower <- vector(mode = "list", length = n.boot)
  for (i in 1:n.boot) {
    floa.point    <- floa_point(data)
    floa.v1       <- floa_rcb(data, n.boot, ver = "v1")
    floa.v2       <- floa_rcb(data, n.boot, ver = "v2")
    floa.v3       <- floa_rcb(data, n.boot, ver = "v3")
    floa.roislien <- floa_roislien(data)

    floa.v2.upper[[i]] <- floa.v2["upper", ]
    floa.v2.lower[[i]] <- floa.v2["lower", ]
    floa.roislien.upper[[i]] <- floa.roislien["upper", ]
    floa.roislien.lower[[i]] <- floa.roislien["lower", ]
    # floa.point.upper[[i]] <- floa.roislien["upper", ]
    # floa.point.lower[[i]] <- floa.roislien["lower", ]
  }

  floa.v2.upper.unlist <- matrix(unlist(floa.v2.upper), 101)
  floa.v2.lower.unlist <- matrix(unlist(floa.v2.lower), 101)
  floa.roislien.upper.unlist <- matrix(unlist(floa.roislien.upper), 101)
  floa.roislien.lower.unlist <- matrix(unlist(floa.roislien.lower), 101)
  # floa.point.upper.unlist <- matrix(unlist(floa.point.upper), 101)
  # floa.point.lower.unlist <- matrix(unlist(floa.point.lower), 101)

  # Pointwise 95% percentile intervals for the LoA
  # floa.v1.pi <- quantile(floa.v1.upper.unlist, probs = c(0.025, 0.975))
  # floa.roislien.pi <- quantile(floa.roislien.upper.unlist, probs = c(0.025, 0.975))
  floa.v2.upper.pi <- apply(floa.v2.upper.unlist, 1, quantile, probs = c(0.025, 0.975))
  floa.v2.lower.pi <- apply(floa.v2.lower.unlist, 1, quantile, probs = c(0.025, 0.975))
  floa.roislien.upper.pi <- apply(floa.roislien.upper.unlist, 1, quantile, probs = c(0.025, 0.975))
  floa.roislien.lower.pi <- apply(floa.roislien.lower.unlist, 1, quantile, probs = c(0.025, 0.975))

  # row.names(pi95) <- c("floa.v2.95.5", "floa.v2.2.5", "floa.roislien.95.5", "floa.roislien.2.5")
  #
  # plot(floa.v2.upper.pi["2.5%", ],
  #      type = "l",
  #      ylim = c(-5, 5),
  #      xlab = "Time-normalized curve length [%]",
  #      ylab = "Limits of Agreement range")
  # lines(floa.v2.upper.pi["97.5%", ])
  # lines(floa.v2.lower.pi["2.5%", ])
  # lines(floa.v2.lower.pi["97.5%", ])
  #
  # lines(floa.roislien.upper.pi["2.5%", ], col = "red")
  # lines(floa.roislien.upper.pi["97.5%", ], col = "red")
  # lines(floa.roislien.lower.pi["2.5%", ], col = "red")
  # lines(floa.roislien.lower.pi["97.5%", ], col = "red")

  pi95 <- rbind(apply(floa.v2.upper.unlist, 1, quantile, probs = c(0.025, 0.975)),
          apply(floa.v2.lower.unlist, 1, quantile, probs = c(0.025, 0.975)),
          apply(floa.roislien.upper.unlist, 1, quantile, probs = c(0.025, 0.975)),
          apply(floa.roislien.lower.unlist, 1, quantile, probs = c(0.025, 0.975))
          )

  pi95 <- data.frame(t(pi95))

  colnames(pi95) <- c("floa.v2.upper.2.5", "floa.v2.upper.97.5", "floa.v2.lower.2.5", "floa.v2.lower.97.5",
                       "floa.roislien.upper.2.5", "floa.roislien.upper.97.5", "floa.roislien.lower.2.5", "floa.roislien.lower.97.5")


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
  device.diff$strideID <- as.factor(rep(1:n.strides, each = 101)) # as.factor(seq(0, n.strides, by = n.frames)) # as.factor(rep(seq(1, strides.per.subject), each = 101))
  device.diff$subjectID <- as.factor(rep(1:n.subjects, each = strides.per.subject * n.frames))
  device.diff$floa.v2.upper.2.5 <- rep(pi95$floa.v2.upper.2.5, length.out = nrow(device.diff))
  device.diff$floa.v2.upper.97.5 <- rep(pi95$floa.v2.upper.97.5, length.out = nrow(device.diff))
  device.diff$floa.v2.lower.2.5 <- rep(pi95$floa.v2.lower.2.5, length.out = nrow(device.diff))
  device.diff$floa.v2.lower.97.5 <- rep(pi95$floa.v2.lower.97.5, length.out = nrow(device.diff))
  device.diff$floa.roislien.upper.2.5 <- rep(pi95$floa.roislien.upper.2.5, length.out = nrow(device.diff))
  device.diff$floa.roislien.upper.97.5 <- rep(pi95$floa.roislien.upper.97.5, length.out = nrow(device.diff))
  device.diff$floa.roislien.lower.2.5 <- rep(pi95$floa.roislien.lower.2.5, length.out = nrow(device.diff))
  device.diff$floa.roislien.lower.97.5 <- rep(pi95$floa.roislien.lower.97.5, length.out = nrow(device.diff))

  # For line graphs, the data points must be grouped so that it knows which points to connect.
  # In this case, it is simple -- all points should be connected, so group=1.
  # When more variables are used and multiple lines are drawn, the grouping for lines is usually done by variable.
  PLOT.DIFF <- ggplot(data = device.diff, aes(x = frame, y = value, color = subjectID, group = strideID)) +
    geom_ribbon(aes(ymin = floa.roislien.upper.2.5, ymax = floa.roislien.upper.97.5), fill = "cyan4", alpha = 0.1) +
    geom_ribbon(aes(ymin = floa.roislien.lower.2.5, ymax = floa.roislien.lower.97.5), fill = "cyan4", alpha = 0.1) +
    geom_ribbon(aes(ymin = floa.v2.upper.2.5, ymax = floa.v2.upper.97.5), fill = "red", alpha = 0.2) +
    geom_ribbon(aes(ymin = floa.v2.lower.2.5, ymax = floa.v2.lower.97.5), fill = "red", alpha = 0.2) +
    geom_line(alpha = .25) +
    scale_color_grey(start = 0.8, end = 0.2) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 20),
          axis.title.x = element_text(size = 22),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 22),
          legend.position = "none") +
    ylim(-5, 5) +
    labs(x = "Time-normalized signal [%]", y = "Difference")

  PLOT.DIFF
}
