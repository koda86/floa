draw_clusters <- function(data, ver) { # fd.basis

  # ----------------------------------------------------------------------------
  # This function represents the first of two stages in the randomized cluster
  # bootstrap.
  #
  # There are several variants to choose from:
  # v1   : n = length(subjects) random strides from all strides
  # v1.1 : Functional data version of v1
  # v2   : One stride per subject
  # v3   : Fetch a single stride only from all available strides
  # ----------------------------------------------------------------------------

  if (ver == "v2") {

    # One stride per subject
    # ----------------------------------------------------------------------------
    diff.curves <- pick_subwise_curves(data)
  }

  if (ver == "v3") {

    # Fetch a single stride only (from the entire set of curves)
    # ----------------------------------------------------------------------------
    curve.idx <- sample(unique(data$strideID), size = 1)
    curve <- data[data$strideID %in% curve.idx, ]

    curve0 <- subset(curve, device  == "IMU")$value
    curve0 <- matrix(curve0, ncol = length(curve0) / 100)

    curve1 <- subset(curve, device  == "MC")$value
    curve1 <- matrix(curve1, ncol = length(curve1) / 100)

    diff.curves <- curve0 - curve1
  }

  return(diff.curves)
}


  # if (ver == "v1.1") {

  # #### v1.1 (functional data) ####
  # #
  # # Not implemented correctly, yet!
  # # ----------------------------------------------------------------------------
  # #
  # # library(fda.usc)
  # #
  # # TODO: implement
  #
  # # diff.curves.fd <- Data2fd(argvals = diff.curve, basisobj = fd.basis)
  #
  # # # Pick a single curve per subject
  # # curve.samp <- sample(1:ncol(diff.curves), size = 1)
  #
  # # diff.curve.samp <- diff.curves[, curve.samp]
  #
  # # # Convert class fd to class fdata (required by fdata.bootstrap())
  # # cluster.fdata <- fdata(diff_curve.fd)
  # }

