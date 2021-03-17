draw_clusters <- function(data, fd.basis) {

  library(fda.usc)

  # ----------------------------------------------------------------------------
  # This function represents the first of two stages in the randomized cluster
  # bootstrap.
  #
  # Here, subjectwise clusters (all curves within a subject) are drawn WITH REPLACEMENT!
  # ----------------------------------------------------------------------------

  # Pick all curves of a single random subject
  subjects <- as.numeric(unique(data$subjectID))

  # Despite replace=FALSE, this is a de facto drawing with replacement as the function is nested within FLOAboot_rcb()
  cluster.idx <- sample(subjects, 1)

  diff_curve <- c()

  for (idx in 1:length(cluster.idx)) {

    curve0 <- subset(data,
                     subjectID == cluster.idx[idx]
                     & device  == "IMU")$value

    curve0 <- matrix(curve0, ncol = length(curve0) / 100)

    curve1 <- subset(data,
                     subjectID == cluster.idx[idx]
                     & device  == "MC")$value

    curve1 <- matrix(curve1, ncol = length(curve1) / 100)

    diff_curve.tmp <- curve0 - curve1

    diff_curve <- cbind(diff_curve, diff_curve.tmp)
}

  # Functional data objects
  diff_curve.fd <- Data2fd(argvals = diff_curve, basisobj = fd.basis)

  # Convert class fd to class fdata (required by fdata.bootstrap())
  cluster.fdata <- fdata(diff_curve.fd)

  return(cluster.fdata)
}
