floa_rcb <- function(data, fd.basis, n.boot) {

  # ----------------------------------------------------------------------------
  # Nested bootstrap (Davison & Hinkley, 1997, pp. 100-102)
  #
  # Only first cluster level is with replacement ... second stage is without!
  # ----------------------------------------------------------------------------

  clust.boot.agg <- list()

  for (boot.idx in 1:n.boot) {

    # --------------------------------------------------------------------------
    # 1. STAGE: Draws all curves of a subject WITH REPLACEMENT
    # --------------------------------------------------------------------------

    clust.fdata <- draw_clusters(data, fd.basis)

    # --------------------------------------------------------------------------
    # 2. STAGE: The first stage sample is drawn again WITHOUT REPLACEMENT
    # --------------------------------------------------------------------------

    nr <- nrow(clust.fdata)

    out.boot.mean <- clust.fdata[sample(1:nr, size=nr, replace=FALSE), ] # from fda.usc
    # ???
    # out.boot.mean <- fdata.bootstrap(fd.usc,
    #                                  statistic = func.mean,
    #                                  alpha = 0.05,
    #                                  nb = n.boot,
    #                                  draw = FALSE) # TRUE to plot data
    #
    # fda.usc.mean.boot <- func.mean(out.boot.mean$resample)
    # fda.usc.sd.boot <- sqrt(func.var(out.boot.mean$resample))

    clust.boot <- out.boot.mean$data

    # Aggregate all clusters to "final" bootstrap distribution
    clust.boot.agg[[boot.idx]] <- clust.boot
  }

  # clust.boot.agg.unlist <- unlist(clust.boot.agg, recursive = FALSE)
  clust.boot.agg.unlist <- clust.boot.agg


  # Hier fehlt noch ein Zwischenschritt: clust.agg, rbind



    # Calculate 2.5 and 97.5 percentiles across joints
    # ----------------------------------------------------------------------------
    floa.boot.percentiles <- c()

    for (i in 1:dim(clust.agg[[1]])[2]) {

      # Percentiles are calculated pointwise
      # TODO: Bias correction useful/necessary?
      floa.boot.percentiles <- c(floa.boot.percentiles, quantile(clust.boot.agg.unlist[, i], probs = c(0.025, 0.975)))
    }

    perc2.5 <- floa.boot.percentiles[seq(1, length(floa.boot.percentiles) - 1, 2)]
    perc97.5 <- floa.boot.percentiles[seq(2, length(floa.boot.percentiles), 2)]


    # Interpolate to 101 data points -------------------------------------------
    floa.boot.percentiles.intrp <- rbind(approx(perc2.5, n = 101)$y, approx(perc97.5, n = 101)$y)

  return(floa.boot.percentiles.intrp)
}
