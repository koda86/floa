floa_rcb <- function(data, fd.basis, n.boot, plt) {

  # ----------------------------------------------------------------------------
  # Randomized Cluster Bootstrap
  #
  # Add description here
  # draw_clusters(): Draw a single random curve form a single random subject
  # ----------------------------------------------------------------------------

  clust.boot.agg <- c()

  for (boot.idx in 1:n.boot) {

    clust.boot.agg[[boot.idx]] <- draw_clusters(data, fd.basis)
  }

  # Arrange curves in matrix (row-wise) to facilitate computing percentiles
  clust.agg <- matrix(unlist(clust.boot.agg),
                      ncol  = length(fd.basis$names),
                      byrow = TRUE)

  clust.agg.intrp <- t(sapply(apply(t(clust.agg), 2, approx, n = 101), "[[", "y"))

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

  if (plt) {
    plot(clust.agg.intrp[1, ],
         type = "l",
         ylim = c(-20, 20),
         ylab = "Diff [deg]")
    apply(clust.agg.intrp, 1, lines)
    apply(floa.boot.percentiles.intrp, 1, lines, col = "red", lwd = 5)
  } else {
    print("")
  }

  return(results)
}
