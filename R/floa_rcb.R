floa_rcb <- function(data, fd.basis, n.boot) {

  # ----------------------------------------------------------------------------
  # (Nested) Randomized Cluster Bootstrap
  #
  # In the first stage (draw_clusters()), all curves of a single (random) subject
  # are drawn. From this set, the (functional) mean is calculated. The process is
  # repeated n.boot times (n.boot = number of bootstrap iterations).
  # ----------------------------------------------------------------------------

  # Contains mean curve for n.boot iterations
  clust.boot.agg <- c()

  for (boot.idx in 1:n.boot) {

    # In draw
    clust.fdata <- draw_clusters(data, fd.basis)

    out.boot.mean <- func.mean(clust.fdata)

    clust.boot.agg[[boot.idx]] <- out.boot.mean$data
  }

  # Calculate 2.5 and 97.5 percentiles across joints
  # ----------------------------------------------------------------------------

  # Arrange mean curves in matrix (row-wise) to facilitate computing percentiles
  clust.agg <- matrix(unlist(clust.boot.agg),
                      ncol  = length(fd.basis$names),
                      byrow = TRUE)

  floa.boot.percentiles <- c()

  for (i in 1:ncol(clust.agg)) {

    # Percentiles are calculated pointwise
    # TODO: Bias correction useful/necessary?
    floa.boot.percentiles <- c(floa.boot.percentiles, quantile(clust.agg[, i], probs = c(0.025, 0.975)))
  }

  perc2.5  <- floa.boot.percentiles[seq(1, length(floa.boot.percentiles) - 1, 2)]
  perc97.5 <- floa.boot.percentiles[seq(2, length(floa.boot.percentiles), 2)]


  # Interpolate to 101 data points -------------------------------------------
  floa.boot.percentiles.intrp <- rbind(approx(perc2.5, n = 101)$y, approx(perc97.5, n = 101)$y)

  return(floa.boot.percentiles.intrp)
}
