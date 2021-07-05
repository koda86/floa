floa_rcb <- function(data, fd.basis, n.boot) {

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

  # Calculate percentiles -----------------------------------------
  floa.boot.percentiles <- c()

  for (i in 1:ncol(clust.agg)) {

    # TODO: Bias correction useful/necessary?
    floa.boot.percentiles <- c(floa.boot.percentiles, quantile(clust.agg[, i], probs = c(0.025, 0.5, 0.975)))
  }

  perc2.5 <- floa.boot.percentiles[which(names(floa.boot.percentiles) == "2.5%")]
  perc50 <- floa.boot.percentiles[which(names(floa.boot.percentiles) == "50%")]
  perc97.5 <- floa.boot.percentiles[which(names(floa.boot.percentiles) == "97.5%")]

  # perc2.5  <- floa.boot.percentiles[seq(1, length(floa.boot.percentiles) - 1, 2)]
  # perc50 <- floa.boot.percentiles[seq(2, length(floa.boot.percentiles) - 1, 2)]
  # perc97.5 <- floa.boot.percentiles[seq(3, length(floa.boot.percentiles), 2)]


  # Interpolate to 101 data points -------------------------------------------
  floa.boot.percentiles.intrp <- rbind(approx(perc2.5, n = 101)$y, approx(perc50, n = 101)$y, approx(perc97.5, n = 101)$y)

  results <- list("curves" = clust.agg, "FLoA" = floa.boot.percentiles.intrp)

  return(results)
}
