# Estimate uncertainty in different LoA approaches (95% percentile intervals for the LoA)
estimate_uncertainty_loa <- function (data, n.boot) {

  floa.roislien.upper <- vector(mode = "list", length = n.boot)
  floa.roislien.lower <- vector(mode = "list", length = n.boot)
  floa.lenhoff.upper <- vector(mode = "list", length = n.boot)
  floa.lenhoff.lower <- vector(mode = "list", length = n.boot)

  for (i in 1:n.boot) {
    floa.roislien <- floa_roislien(data)
    floa.lenhoff  <- floa_lenhoff(data,
                                  k_reihe = 50,
                                  n.boot = n.boot,
                                  band = "prediction",
                                  cp.begin = 0,
                                  alpha = 0.05)

    floa.roislien.upper[[i]] <- floa.roislien["upper", ]
    floa.roislien.lower[[i]] <- floa.roislien["lower", ]
    floa.lenhoff.upper[[i]]  <- floa.lenhoff["upper", ]
    floa.lenhoff.lower[[i]]  <- floa.lenhoff["lower", ]
  }

  # Pointwise LoA return the same estimate every time (all curves are included)
  # (Therefore, they are outside the loop)
  floa.point <- data.frame(t(floa_point(data)))

  floa.roislien.upper.unlist <- matrix(unlist(floa.roislien.upper), 101)
  floa.roislien.lower.unlist <- matrix(unlist(floa.roislien.lower), 101)
  floa.lenhoff.upper.unlist <- matrix(unlist(floa.lenhoff.upper), 101)
  floa.lenhoff.lower.unlist <- matrix(unlist(floa.lenhoff.lower), 101)

  # Pointwise 95% percentile intervals for the LoA
  pi95 <- rbind(
    apply(floa.roislien.upper.unlist, 1, quantile, probs = c(0.025, 0.975)),
    apply(floa.roislien.lower.unlist, 1, quantile, probs = c(0.025, 0.975)),
    apply(floa.lenhoff.upper.unlist, 1, quantile, probs = c(0.025, 0.975)),
    apply(floa.lenhoff.lower.unlist, 1, quantile, probs = c(0.025, 0.975)),
    # Pointwise limits are calculated differently (formula suggested in Bland & Altman, 1986)
    floa.point$stderror.dplus2s.lower,
    floa.point$stderror.dplus2s.upper,
    floa.point$stderror.dminus2s.lower,
    floa.point$stderror.dminus2s.upper
    )

  pi95 <- data.frame(t(pi95))

  colnames(pi95) <- c(
    "floa.roislien.upper.2.5", "floa.roislien.upper.97.5", "floa.roislien.lower.2.5", "floa.roislien.lower.97.5",
    "floa.lenhoff.upper.2.5", "floa.lenhoff.upper.97.5", "floa.lenhoff.lower.2.5", "floa.lenhoff.lower.97.5",
    "floa.point.upper.2.5", "floa.point.upper.97.5", "floa.point.lower.2.5", "floa.point.lower.97.5"
    )

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
  device.diff$floa.roislien.upper.2.5 <- rep(pi95$floa.roislien.upper.2.5, length.out = nrow(device.diff))
  device.diff$floa.roislien.upper.97.5 <- rep(pi95$floa.roislien.upper.97.5, length.out = nrow(device.diff))
  device.diff$floa.roislien.lower.2.5 <- rep(pi95$floa.roislien.lower.2.5, length.out = nrow(device.diff))
  device.diff$floa.roislien.lower.97.5 <- rep(pi95$floa.roislien.lower.97.5, length.out = nrow(device.diff))
  device.diff$floa.lenhoff.upper.2.5 <- rep(pi95$floa.lenhoff.upper.2.5, length.out = nrow(device.diff))
  device.diff$floa.lenhoff.upper.97.5 <- rep(pi95$floa.lenhoff.upper.97.5, length.out = nrow(device.diff))
  device.diff$floa.lenhoff.lower.2.5 <- rep(pi95$floa.lenhoff.lower.2.5, length.out = nrow(device.diff))
  device.diff$floa.lenhoff.lower.97.5 <- rep(pi95$floa.lenhoff.lower.97.5, length.out = nrow(device.diff))
  device.diff$floa.point.upper.2.5 <- rep(pi95$floa.point.upper.2.5, length.out = nrow(device.diff))
  device.diff$floa.point.upper.97.5 <- rep(pi95$floa.point.upper.97.5, length.out = nrow(device.diff))
  device.diff$floa.point.lower.2.5 <- rep(pi95$floa.point.lower.2.5, length.out = nrow(device.diff))
  device.diff$floa.point.lower.97.5 <- rep(pi95$floa.point.lower.97.5, length.out = nrow(device.diff))

  # For line graphs, the data points must be grouped so that it knows which points to connect.
  # In this case, it is simple -- all points should be connected, so group=1.
  # When more variables are used and multiple lines are drawn, the grouping for lines is usually done by variable.
  PLOT.DIFF <- ggplot(data = device.diff, aes(x = frame, y = value)) +
    # geom_line(data = device.diff,
    #           aes(group = strideID),
    #           alpha = 0.1) +
    geom_ribbon(aes(ymin = floa.roislien.upper.2.5,
                    ymax = floa.roislien.upper.97.5),
                fill = "#56B4E9",
                alpha = 0.5) +
    geom_ribbon(aes(ymin = floa.roislien.lower.2.5,
                    ymax = floa.roislien.lower.97.5),
                fill = "#56B4E9",
                alpha = 0.5) +
    geom_ribbon(aes(ymin = floa.lenhoff.upper.2.5,
                  ymax = floa.lenhoff.upper.97.5),
              fill = "#D55E00",
              alpha = 0.5) +
    geom_ribbon(aes(ymin = floa.lenhoff.lower.2.5,
                    ymax = floa.lenhoff.lower.97.5),
                fill = "#D55E00",
                alpha = 0.5) +
    # geom_line(data = floa.point,
    #           aes(x = seq(0, 100), y = upper),
    #           linetype = "dotted",
    #           size = 1.5,
    #           colour = "#009E73") +
    # geom_line(data = floa.point,
    #           aes(x = seq(0, 100), y = lower),
    #           linetype = "dotted",
    #           size = 1.5,
    #           colour = "#009E73") +
    geom_ribbon(aes(ymin = floa.point.upper.2.5,
                    ymax = floa.point.upper.97.5),
                fill = "#009E73",
                alpha = 0.5) +
    geom_ribbon(aes(ymin = floa.point.lower.2.5,
                    ymax = floa.point.lower.97.5),
                fill = "#009E73",
                alpha = 0.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 20),
          axis.title.x = element_text(size = 22),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 22),
          legend.position = "none") +
    ylim(-5, 5) +
    labs(x = "Time-normalized signal [%]", y = "Difference")

  PLOT.DIFF


  # (Uncertainty) Area between 2.5 and 97.5 borders
  uncert.area.limits <- c(sum(pi95$floa.roislien.upper.97.5) - sum(pi95$floa.roislien.upper.2.5),
                          sum(pi95$floa.roislien.lower.97.5) - sum(pi95$floa.roislien.lower.2.5),
                          sum(pi95$floa.lenhoff.upper.97.5) - sum(pi95$floa.lenhoff.upper.2.5),
                          sum(pi95$floa.lenhoff.lower.97.5) - sum(pi95$floa.lenhoff.lower.2.5),
                          sum(pi95$floa.point.upper.97.5) - sum(pi95$floa.point.upper.2.5),
                          sum(pi95$floa.point.lower.97.5) - sum(pi95$floa.point.lower.2.5)
                          )

  names(uncert.area.limits) <- c("au.roislien", "al.roislien", "au.lenhoff", "al.lenhoff", "au.point", "al.point")

  return(uncert.area.limits)
}

