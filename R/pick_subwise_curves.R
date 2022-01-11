pick_subwise_curves <- function (data) {

  # ******   Select a single random stride from every subject in data   ********
  # ****************************************************************************
  # Select a single random stride from every subject in data
  n.subj <- length(unique(data$subjectID))

  curve.idx <- c()
  for (subj.idx in 1:n.subj) {
    if (subj.idx %in% unique(data$subjectID)) {
      tmp <- data[data$subjectID == subj.idx, ]

      curve.idx.subj <- as.numeric(sample(tmp$strideID, size = 1))
      curve.idx <- c(curve.idx, curve.idx.subj)
    }
  }

  curve <- data[data$strideID %in% curve.idx, ]
  curve0 <- subset(curve, device  == "IMU")$value
  curve0 <- matrix(curve0, ncol = length(curve0) / 100)
  curve1 <- subset(curve, device  == "MC")$value
  curve1 <- matrix(curve1, ncol = length(curve1) / 100)

  diff.curves <- curve0 - curve1

  return(diff.curves)
}
