floa_rcb <- function(data, n.boot, ver) { # , fd.basis

  # ############################################################################
  # Randomized Cluster Bootstrap
  # ############################################################################
  clust.boot.agg <- c()
  for (boot.idx in 1:n.boot) {
    # draw_clusters returns difference curves (device1 - device2)
    # Currently, different versions of the sampling process in draw_clusters() are
    # implemented (specified by the function argument ver).
    clust.boot.agg[[boot.idx]] <- draw_clusters(data, ver = ver) # , fd.basis
  }

  # (Row-wise) Arrange difference curves to facilitate computing percentiles
  clust.agg.intrp <- matrix(unlist(clust.boot.agg),
                            ncol  = length(unique(data$frame)), # length(fd.basis$names)
                            byrow = TRUE)

  # Calculate percentiles ------------------------------------------------------
  floa.boot.percentiles <- c()
  for (i in 1:ncol(clust.agg.intrp)) {
    floa.boot.percentiles <- c(floa.boot.percentiles,
                               quantile(clust.agg.intrp[, i],
                                        probs = c(0.025, 0.5, 0.975)
                                        )
                               )
  }

  perc2.5 <- floa.boot.percentiles[which(names(floa.boot.percentiles) == "2.5%")]
  perc50 <- floa.boot.percentiles[which(names(floa.boot.percentiles) == "50%")]
  perc97.5 <- floa.boot.percentiles[which(names(floa.boot.percentiles) == "97.5%")]

  # Split the long percentile vector into 101 data points each -----------------
  floa.boot.percentiles.split <- rbind(approx(perc2.5, n = 101)$y,
                                       approx(perc50, n = 101)$y,
                                       approx(perc97.5, n = 101)$y
  )

  # plot(clust.agg.intrp[1, ],
  #     type = "l",
  #     ylim = c(-4, 4))
  # apply(clust.agg.intrp, 1, lines)
  # lines(perc2.5, col = "red", lwd = 10)
  # lines(perc97.5, col = "red", lwd = 10)

  # Add (pointwise) mean
  floa.boot.mean <- colMeans(clust.agg.intrp, dims = 1)
  floa <- rbind(floa.boot.percentiles.split,
                floa.boot.mean)

  row.names(floa) <- c("lower", "median", "upper", "mean")

  return(floa)
}
