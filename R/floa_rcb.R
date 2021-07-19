floa_rcb <- function(data, n.boot, plt) { # , fd.basis

  library(ggplot2)

  # ############################################################################
  # Randomized Cluster Bootstrap
  #
  # Add description here
  # draw_clusters(): Draw a single random curve form a single random subject
  # ############################################################################

  clust.boot.agg <- c()

  for (boot.idx in 1:n.boot) {

    # draw_clusters returns difference curves (device1 - device2)
    clust.boot.agg[[boot.idx]] <- draw_clusters(data) # , fd.basis
  }

  # (Row-wise) Arrange difference curves to facilitate computing percentiles
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

  # Interpolate to 101 data points ---------------------------------------------
  floa.boot.percentiles.intrp <- rbind(approx(perc2.5, n = 101)$y,
                                       approx(perc50, n = 101)$y,
                                       approx(perc97.5, n = 101)$y
                                       )


  return(floa.boot.percentiles.intrp)
}
