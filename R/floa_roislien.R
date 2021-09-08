floa_roislien <- function(data) {

  # ****************************************************************************
  # Functional limits of agreement according to Roislien et al. (2012)
  #
  # In Roislien et al., FLoA are calculated as 95% CI.
  # ****************************************************************************
  diff.curves <- pick_subwise_curves(data)

  # Calculate Limits of Agreement ----------------------------------------------
  func.mean <- apply(diff.curves, 1, mean)
  func.sd <- apply(diff.curves, 1, sd)

  # Get the same structure as returned by the other methods (i. e. floa_rcb)
  floa.roislien <- rbind(func.mean + 1.96 * func.sd,
                         func.mean,
                         func.mean - 1.96 * func.sd)

  rownames(floa.roislien) <- c("upper", "mean", "lower")

  # # Plausibility check
  # plot(diff.curves[, 1], type="l", ylim = c(-5, 5))
  # apply(diff.curves, 2, lines, type = "l")
  # lines(floa.roislien["upper", ], col = "red", size = 3)
  # lines(floa.roislien["lower", ], col = "red", size = 3)

  return(floa.roislien)
}
