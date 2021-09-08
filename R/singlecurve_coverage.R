singlecurve_coverage <- function (data, n.boot) {
  # ####################################################################
  # Leave-one (curve) out method to estimate the uncertainty in the
  # achieved coverage (see Lenhoff et al. (1999))
  # --------------------------------------------------------------------
  #
  # Currently, different versions of the sampling process in draw_clusters()
  # (nested in floa_rcb()) are implemented:
  #
  # v1 : n = length(subjects) random strides from all strides
  # v2 : One random stride per subject
  # v3 : Fetch a SINGLE random stride from all strides
  # v4 : Roislien approach (Get one random stride from each subject ONCE
  #      and bootstrap the resulting sample
  # v5 : Pointwise B&A limits
  # ####################################################################
  n.curves <- unique(data$strideID)

  cover.cross.rcb.v1   <- vector(mode = "list", length = length(n.curves))
  cover.cross.rcb.v2   <- vector(mode = "list", length = length(n.curves))
  cover.cross.rcb.v3   <- vector(mode = "list", length = length(n.curves))
  cover.cross.point    <- vector(mode = "list", length = length(n.curves))
  cover.cross.roislien <- vector(mode = "list", length = length(n.curves))
  for (curve.idx in n.curves) {
    # Calculate FLoA with one curve left out -----------------------------
    data.one.out <- subset(data, strideID != curve.idx)

    floa.point    <- floa_point(data.one.out)
    floa.v1       <- floa_rcb(data.one.out, n.boot, ver = "v1")
    floa.v2       <- floa_rcb(data.one.out, n.boot, ver = "v2")
    floa.v3       <- floa_rcb(data.one.out, n.boot, ver = "v3")
    floa.roislien <- floa_roislien(data.one.out)

    # Plot left out curve vs. various FLoA methods -----------------------
    data.subset <- subset(data, strideID == curve.idx)

    device1 <- data.frame(subset(data.subset, device == "IMU"))
    device2 <- data.frame(subset(data.subset, device == "MC"))
    device.diff <- device1$value - device2$value

    floa.all <- data.frame(device.diff)
    floa.all$frame <- seq(0, 100)
    floa.all$floa.v1.upper <- floa.v1["upper", ]
    floa.all$floa.v1.lower <- floa.v1["lower", ]
    floa.all$floa.v2.upper <- floa.v2["upper", ]
    floa.all$floa.v2.lower <- floa.v2["lower", ]
    floa.all$floa.v3.upper <- floa.v3["upper", ]
    floa.all$floa.v3.lower <- floa.v3["lower", ]
    floa.all$floa.roislien.upper <- floa.roislien["upper", ]
    floa.all$floa.roislien.lower <- floa.roislien["lower", ]
    floa.all$floa.point.upper <- floa.point["upper", ]
    floa.all$floa.point.lower <- floa.point["lower", ]

    # # For line graphs, the data points must be grouped so that it knows which points to connect.
    # # In this case, it is simple -- all points should be connected, so group=1.
    # # When more variables are used and multiple lines are drawn, the grouping for lines is usually done by variable.
    # PLOT.DIFF <- ggplot(data = floa.all, aes(x = frame, y = device.diff)) +
    #   geom_line() +
    #   geom_line(aes(x = seq(0, 100), y = floa.v1.upper, col = "red", group = 1),
    #             linetype = "solid",
    #             colour = "red") +
    #   geom_line(aes(x = seq(0, 100), y = floa.v1.lower, col = "red", group = 1),
    #             linetype = "solid",
    #             colour = "red") +
    #   geom_line(aes(x = seq(0, 100), y = floa.roislien.upper, col = "red", group = 1),
    #             linetype = "twodash",
    #             colour = "red") +
    #   geom_line(aes(x = seq(0, 100), y = floa.roislien.lower, col = "red", group = 1),
    #             linetype = "twodash",
    #             colour = "red") +
    #   geom_line(aes(x = seq(0, 100), y = floa.point.upper, col = "red", group = 1),
    #             linetype = "dotted",
    #             colour = "red") +
    #   geom_line(aes(x = seq(0, 100), y = floa.point.lower, col = "red", group = 1),
    #             linetype = "dotted",
    #             colour = "red") +
    #   ylim(-5, 5) +
    #   labs(x = "Time-normalized signal [%]", y = "Difference (device 1 - 2)") +
    #   theme_minimal() +
    #   theme(axis.text.x = element_text(size = 17),
    #         axis.title.x = element_text(size = 17),
    #         axis.text.y = element_text(size = 17),
    #         axis.title.y = element_text(size = 17),
    #         legend.position = "none")
    #
    # ggsave(paste0("~/Desktop/tmp/floa_swap/non_gaussian_new/curve", curve.idx, ".png"))

    # Get coverage for the left out (difference) curve -------------------
    cover.cross.rcb.v1[curve.idx]   <- get_coverage_singlecurve(device.diff, floa.v1)
    cover.cross.rcb.v2[curve.idx]   <- get_coverage_singlecurve(device.diff, floa.v2)
    cover.cross.rcb.v3[curve.idx]   <- get_coverage_singlecurve(device.diff, floa.v3)
    cover.cross.roislien[curve.idx] <- get_coverage_singlecurve(device.diff, floa.roislien)
    cover.cross.point[curve.idx]    <- get_coverage_singlecurve(device.diff, floa.point)
  }

  cover.cross <- cbind(unlist(cover.cross.rcb.v1),
                       unlist(cover.cross.rcb.v2),
                       unlist(cover.cross.rcb.v3),
                       unlist(cover.cross.roislien),
                       unlist(cover.cross.point)
  )

  return(cover.cross)
}
