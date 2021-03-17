{

  # ----------------------------------------------------------------------------
  # Nested bootstrap (Davison & Hinkley, 1997, pp. 100-102)
  #
  # Only first cluster level is with replacement ... second stage is without!
  # ----------------------------------------------------------------------------


  # 1. STAGE: Draws all curves of a subject WITH REPLACEMENT
  # --------------------------------------------------------------------------
  clust.boot.agg <- c()

  for (boot.idx in 1:n.boot) {

    clust.fdata <- draw_clusters(data, fd.basis)

    clust.boot.agg[[boot.idx]] <- func.mean(clust.fdata)
  }

  # 2. STAGE: The first stage sample is drawn again WITHOUT REPLACEMENT
  # --------------------------------------------------------------------------
  # Needs to be implemented correctly (look for FLoA_fun in local directory)
  # nr <- nrow(clust.fdata)
  # out.boot <- clust.fdata[sample(1:nr, size=nr, replace=FALSE), ]


  # Calculate 2.5 and 97.5 percentiles across joints
  # ----------------------------------------------------------------------------
  floa.boot.percentiles <- c()

  for (i in 1:ncol(clust.boot.agg)) {

    # Percentiles are calculated pointwise
    # TODO: Bias correction useful/necessary?
    floa.boot.percentiles <- c(floa.boot.percentiles, quantile(clust.boot.agg[, i], probs = c(0.025, 0.975)))
  }

  perc2.5 <- floa.boot.percentiles[seq(1, length(floa.boot.percentiles) - 1, 2)]
  perc97.5 <- floa.boot.percentiles[seq(2, length(floa.boot.percentiles), 2)]


  # Interpolate to 101 data points -------------------------------------------
  floa.boot.percentiles.intrp <- rbind(approx(perc2.5, n = 101)$y, approx(perc97.5, n = 101)$y)

  return(floa.boot.percentiles.intrp)
}
