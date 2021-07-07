draw_clusters <- function(data) { # fd.basis

  # library(fda.usc)

  # ----------------------------------------------------------------------------
  # This function represents the first of two stages in the randomized cluster
  # bootstrap.
  #
  # Add description here.
  # ----------------------------------------------------------------------------

  # One stride from each dubject -----------------------------------------------
  # subjects <- as.numeric(unique(data$subjectID))
  #
  # # Pick a single curve per subject
  # subj.idx <- sample(subjects, replace = FALSE)
  #
  # test <- by(data, data$subjectID, subset)
  #
  # subj.idx <- sample(length(test), replace = TRUE)
  #
  # tmp <- mapply("[", test, "strideID")
  # stride.idx <- sapply(tmp, function(x) {length(unique(x))})
#
#   # Zwischenschritt Auswahl eines zur Subjektreihenfolge passenden Schrittindex
#   # ...

  # n=length(subjects) random strides
  curve.idx <- as.numeric(sample(data$strideID, length(unique(data$subjectID)), replace = TRUE))

  curve <- data[data$strideID %in% curve.idx, ]

  curve0 <- subset(curve, device  == "IMU")$value
  curve0 <- matrix(curve0, ncol = length(curve0) / 100)

  curve1 <- subset(curve, device  == "MC")$value
  curve1 <- matrix(curve1, ncol = length(curve1) / 100)

  diff.curves <- curve0 - curve1

  # diff.curves.fd <- Data2fd(argvals = diff.curve, basisobj = fd.basis)

  # # Pick a single curve per subject
  # curve.samp <- sample(1:ncol(diff.curves), size = 1)

  # diff.curve.samp <- diff.curves[, curve.samp]

  # # Convert class fd to class fdata (required by fdata.bootstrap())
  # cluster.fdata <- fdata(diff_curve.fd)

  return(diff.curves) # diff.curve.samp # cluster.fdata
}
