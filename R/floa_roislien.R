floa_roislien <- function(data, n.boot, ver) { # , fd.basis

  # ############################################################################
  # Functional limits of agreement according to Roislien et al. (2012)
  # ############################################################################

  # Get one random stride from each subject ONCE and bootstrap the resulting
  # sample (of length (n=length(subjects))
  # ----------------------------------------------------------------------------
  curve.idx <- sample(unique(data$strideID), size = 1)

  curve <- data[data$strideID %in% curve.idx, ]

  curve0 <- subset(curve, device  == "IMU")$value
  curve0 <- matrix(curve0, ncol = length(curve0) / 100)

  curve1 <- subset(curve, device  == "MC")$value
  curve1 <- matrix(curve1, ncol = length(curve1) / 100)

  diff.curves <- curve0 - curve1

  # Calculate Limits of Agreement ----------------------------------------------


  return(floa)
}
