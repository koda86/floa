estimate_uncertainty_loa <- function (data, n.rep, n.boot) {

  # Estimate uncertainty in different methods (95% percentile intervals)
  # ----------------------------------------------------------------------------
  #
  # Function arguments:
  # data     : data
  # n.rep    : Number of repeated calculations
  # n.boot   : Number of bootstrap iterations
  # ----------------------------------------------------------------------------

  floa.roislien.upper <- vector(mode = "list", length = n.boot)
  floa.roislien.lower <- vector(mode = "list", length = n.boot)
  floa.boot.all.upper <- vector(mode = "list", length = n.boot)
  floa.boot.all.lower <- vector(mode = "list", length = n.boot)
  floa.boot.iid.upper <- vector(mode = "list", length = n.boot)
  floa.boot.iid.lower <- vector(mode = "list", length = n.boot)

  for (i in 1:n.rep) {
    floa.roislien <- floa_roislien(data)

    floa.boot.all  <- floa_boot(data,
                                k_reihe = 50,
                                n.boot = n.boot,
                                band = "prediction",
                                cp.begin = 0,
                                alpha = 0.05,
                                iid = FALSE)

    floa.boot.iid  <- floa_boot(data,
                                k_reihe = 50,
                                n.boot = n.boot,
                                band = "prediction",
                                cp.begin = 0,
                                alpha = 0.05,
                                iid = TRUE)

    floa.roislien.upper[[i]] <- floa.roislien["upper.loa", ]
    floa.roislien.lower[[i]] <- floa.roislien["lower.loa", ]
    floa.boot.all.upper[[i]]  <- floa.boot.all["upper.loa", ]
    floa.boot.all.lower[[i]]  <- floa.boot.all["lower.loa", ]
    floa.boot.iid.upper[[i]]  <- floa.boot.iid["upper.loa", ]
    floa.boot.iid.lower[[i]]  <- floa.boot.iid["lower.loa", ]
  }

  floa.roislien.upper.unlist <- matrix(unlist(floa.roislien.upper), 101)
  floa.roislien.lower.unlist <- matrix(unlist(floa.roislien.lower), 101)
  floa.boot.all.upper.unlist <- matrix(unlist(floa.boot.all.upper), 101)
  floa.boot.all.lower.unlist <- matrix(unlist(floa.boot.all.lower), 101)
  floa.boot.iid.upper.unlist <- matrix(unlist(floa.boot.iid.upper), 101)
  floa.boot.iid.lower.unlist <- matrix(unlist(floa.boot.iid.lower), 101)

  # Pointwise LoA return the same estimate every time (all curves are included)
  floa.point <- data.frame(t(floa_point(data)))

  # Pointwise 95% percentile intervals for the LoA
  pi95 <- rbind(
    apply(floa.roislien.upper.unlist, 1, quantile, probs = c(0.025, 0.975)),
    apply(floa.roislien.lower.unlist, 1, quantile, probs = c(0.025, 0.975)),
    apply(floa.boot.all.upper.unlist, 1, quantile, probs = c(0.025, 0.975)),
    apply(floa.boot.all.lower.unlist, 1, quantile, probs = c(0.025, 0.975)),
    apply(floa.boot.iid.upper.unlist, 1, quantile, probs = c(0.025, 0.975)),
    apply(floa.boot.iid.lower.unlist, 1, quantile, probs = c(0.025, 0.975)),
    # Pointwise limits are calculated differently (using CI, see floa_point())
    # but used in the same way here to facilitate plotting (plausibility control)
    floa.point$upper.ci.lower,
    floa.point$upper.ci.upper,
    floa.point$lower.ci.lower,
    floa.point$lower.ci.upper
    )

  pi95 <- data.frame(t(pi95))
  colnames(pi95) <- c(
    "floa.roislien.upper.2.5", "floa.roislien.upper.97.5", "floa.roislien.lower.2.5", "floa.roislien.lower.97.5",
    "floa.boot.all.upper.2.5", "floa.boot.all.upper.97.5", "floa.boot.all.lower.2.5", "floa.all.boot.lower.97.5",
    "floa.boot.iid.upper.2.5", "floa.boot.iid.upper.97.5", "floa.boot.iid.lower.2.5", "floa.iid.boot.lower.97.5",
    "floa.point.upper.2.5", "floa.point.upper.97.5", "floa.point.lower.2.5", "floa.point.lower.97.5"
    )

  # # Prepare data for ggploting -------------------------------------------------
  # device1 <- data.frame(subset(data, device == "IMU")$value)
  # device2 <- data.frame(subset(data, device == "MC")$value)
  # device.diff <- device1 - device2
  #
  # colnames(device.diff)[1] <- "value"
  #
  # device.diff$frame <- seq(0, 100)
  # n.frames <- length(unique(data$frame))
  # n.strides <- length(unique(data$strideID))
  # n.subjects <- length(unique(data$subjectID))
  # strides.per.subject <- length(unique(data$strideID)) / n.subjects
  # device.diff$strideID <- as.factor(rep(1:n.strides, each = 101))
  # device.diff$subjectID <- as.factor(rep(1:n.subjects, each = strides.per.subject * n.frames))
  # device.diff$floa.roislien.upper.2.5 <- rep(pi95$floa.roislien.upper.2.5, length.out = nrow(device.diff))
  # device.diff$floa.roislien.upper.97.5 <- rep(pi95$floa.roislien.upper.97.5, length.out = nrow(device.diff))
  # device.diff$floa.roislien.lower.2.5 <- rep(pi95$floa.roislien.lower.2.5, length.out = nrow(device.diff))
  # device.diff$floa.roislien.lower.97.5 <- rep(pi95$floa.roislien.lower.97.5, length.out = nrow(device.diff))
  # device.diff$floa.boot.upper.2.5 <- rep(pi95$floa.boot.upper.2.5, length.out = nrow(device.diff))
  # device.diff$floa.boot.upper.97.5 <- rep(pi95$floa.boot.upper.97.5, length.out = nrow(device.diff))
  # device.diff$floa.boot.lower.2.5 <- rep(pi95$floa.boot.lower.2.5, length.out = nrow(device.diff))
  # device.diff$floa.boot.lower.97.5 <- rep(pi95$floa.boot.lower.97.5, length.out = nrow(device.diff))
  # device.diff$floa.point.upper.2.5 <- rep(pi95$floa.point.upper.2.5, length.out = nrow(device.diff))
  # device.diff$floa.point.upper.97.5 <- rep(pi95$floa.point.upper.97.5, length.out = nrow(device.diff))
  # device.diff$floa.point.lower.2.5 <- rep(pi95$floa.point.lower.2.5, length.out = nrow(device.diff))
  # device.diff$floa.point.lower.97.5 <- rep(pi95$floa.point.lower.97.5, length.out = nrow(device.diff))
  #
  # # For line graphs, the data points must be grouped so that it knows which points to connect.
  # # In this case, it is simple -- all points should be connected, so group=1.
  # # When more variables are used and multiple lines are drawn, the grouping for lines is usually done by variable.
  # PLOT.DIFF <- ggplot(data = device.diff, aes(x = frame, y = value)) +
  #   # geom_line(data = device.diff,
  #   #           aes(group = strideID),
  #   #           alpha = 0.1) +
  #   geom_ribbon(aes(ymin = floa.roislien.upper.2.5,
  #                   ymax = floa.roislien.upper.97.5),
  #               fill = "#56B4E9",
  #               alpha = 0.5) +
  #   geom_ribbon(aes(ymin = floa.roislien.lower.2.5,
  #                   ymax = floa.roislien.lower.97.5),
  #               fill = "#56B4E9",
  #               alpha = 0.5) +
  #   geom_ribbon(aes(ymin = floa.boot.upper.2.5,
  #                 ymax = floa.boot.upper.97.5),
  #             fill = "#D55E00",
  #             alpha = 0.5) +
  #   geom_ribbon(aes(ymin = floa.boot.lower.2.5,
  #                   ymax = floa.boot.lower.97.5),
  #               fill = "#D55E00",
  #               alpha = 0.5) +
  #   # geom_line(data = floa.point,
  #   #           aes(x = seq(0, 100), y = upper),
  #   #           linetype = "dotted",
  #   #           size = 1.5,
  #   #           colour = "#009E73") +
  #   # geom_line(data = floa.point,
  #   #           aes(x = seq(0, 100), y = lower),
  #   #           linetype = "dotted",
  #   #           size = 1.5,
  #   #           colour = "#009E73") +
  #   geom_ribbon(aes(ymin = floa.point.upper.2.5,
  #                   ymax = floa.point.upper.97.5),
  #               fill = "#009E73",
  #               alpha = 0.5) +
  #   geom_ribbon(aes(ymin = floa.point.lower.2.5,
  #                   ymax = floa.point.lower.97.5),
  #               fill = "#009E73",
  #               alpha = 0.5) +
  #   theme_minimal() +
  #   theme(axis.text.x = element_text(size = 20),
  #         axis.title.x = element_text(size = 22),
  #         axis.text.y = element_text(size = 20),
  #         axis.title.y = element_text(size = 22),
  #         legend.position = "none") +
  #   ylim(-5, 5) +
  #   labs(x = "Time-normalized signal [%]", y = "Difference")
  #
  # PLOT.DIFF


  # Area between 2.5 and 97.5 percentile borders (Cumulated Uncertainty Area)
  # POINT values are identical for the upper and lower limits
  CUA.point.upper     <- round(sum(pi95$floa.point.upper.97.5 - pi95$floa.point.upper.2.5), 2)
  CUA.point.lower     <- round(sum(pi95$floa.point.lower.97.5 - pi95$floa.point.lower.2.5), 2)
  CUA.roislien.upper  <- round(sum(pi95$floa.roislien.upper.97.5 - pi95$floa.roislien.upper.2.5), 2)
  CUA.roislien.lower  <- round(sum(pi95$floa.roislien.lower.97.5 - pi95$floa.roislien.lower.2.5), 2)
  CUA.boot.all.upper  <- round(sum(pi95$floa.boot.all.upper.97.5 - pi95$floa.boot.all.upper.2.5), 2)
  CUA.boot.all.lower  <- round(sum(pi95$floa.boot.all.lower.97.5 - pi95$floa.boot.all.lower.2.5), 2)
  CUA.boot.iid.upper  <- round(sum(pi95$floa.boot.iid.upper.97.5 - pi95$floa.boot.iid.upper.2.5), 2)
  CUA.boot.iid.lower  <- round(sum(pi95$floa.boot.iid.lower.97.5 - pi95$floa.boot.iid.lower.2.5), 2)

  uncert.area <- c(CUA.point.upper + CUA.point.lower,
                   CUA.roislien.upper + CUA.roislien.upper,
                   CUA.boot.all.upper + CUA.boot.all.lower,
                   CUA.boot.iid.upper + CUA.boot.iid.lower
                   )

  names(uncert.area) <- c("POINT", "ROISLIEN", "BOOT.all", "BOOT.iid")

  return(uncert.area)
}

