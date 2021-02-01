floa_rcb <- function(data.long, fd.basis, n.boot) {

  # ----------------------------------------------------------------------------
  # Nested bootstrap (Davison & Hinkley, 1997)
  #
  # Only first cluster level is with replacement ... second stage is without!
  # ----------------------------------------------------------------------------

  clust.boot.agg <- list()

  for (boot.idx in 1:n.boot) {

    # --------------------------------------------------------------------------
    # 1. STAGE: Draws all curves of a subject WITH REPLACEMENT
    # --------------------------------------------------------------------------

    clust.fdata <- draw_clusters(data.long, fd.basis)

    # --------------------------------------------------------------------------
    # 2. STAGE: The first stage sample is drawn again WITHOUT REPLACEMENT
    # --------------------------------------------------------------------------

    clust.boot.joint <- list()

    for (joint.idx in 1:length(clust.fdata)) { # Iterate over all 6 joint/side combinations

      nr <- nrow(clust.fdata[[joint.idx]])

      out.boot.mean <- clust.fdata[[joint.idx]][sample(1:nr, size=nr, replace=FALSE), ] # from fda.usc

      clust.boot.joint[[joint.idx]] <- out.boot.mean$data
    }

    # Aggregate all clusters to "final" bootstrap distribution
    clust.boot.agg[[boot.idx]] <- clust.boot.joint
  }

  clust.boot.agg.unlist <- unlist(clust.boot.agg, recursive = FALSE)


  # ----------------------------------------------------------------------------
  # Map aggregated bootstrap distribution to joints and sides
  clust.agg <- vector("list", length = 6)

  for (i in seq(1, length(clust.boot.agg.unlist), 6)) {

    hip.left <-  clust.boot.agg.unlist[[i]]
    hip.right <-  clust.boot.agg.unlist[[i+1]]
    knee.left <-  clust.boot.agg.unlist[[i+2]]
    knee.right <-  clust.boot.agg.unlist[[i+3]]
    ankle.left <-  clust.boot.agg.unlist[[i+4]]
    ankle.right <-  clust.boot.agg.unlist[[i+5]]

    clust.agg[[1]] <- rbind(clust.agg[[1]], hip.left)
    clust.agg[[2]] <- rbind(clust.agg[[2]], hip.right)
    clust.agg[[3]] <- rbind(clust.agg[[3]], knee.left)
    clust.agg[[4]] <- rbind(clust.agg[[4]], knee.right)
    clust.agg[[5]] <- rbind(clust.agg[[5]], ankle.left)
    clust.agg[[6]] <- rbind(clust.agg[[6]], ankle.right)
  }



  # ----------------------------------------------------------------------------
  # Calculate 2.5 and 97.5 percentiles across joints

  floa.boot.percentiles.intrp <- list()

  for (joint.idx in 1:6) {

    floa.boot.percentiles <- c()

    for (i in 1:dim(clust.agg[[1]])[2]) { # Percentiles are calculated pointwise

      floa.boot.percentiles <- c(floa.boot.percentiles, quantile(clust.agg[[joint.idx]][, i], probs = c(0.025, 0.975)))
    }

    perc2.5 <- floa.boot.percentiles[seq(1, length(floa.boot.percentiles) - 1, 2)]
    perc97.5 <- floa.boot.percentiles[seq(2, length(floa.boot.percentiles), 2)]


    # Interpolate to 101 data points -------------------------------------------
    floa.boot.percentiles.intrp[[joint.idx]] <- rbind(approx(perc2.5, n = 101)$y, approx(perc97.5, n = 101)$y)
  }

  return(floa.boot.percentiles.intrp)
}
