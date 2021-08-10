boot_mean_sd <- function (data, n.boot) {

  mean.boot <- vector(mode = "list", length = n.boot)
  sd.boot <- vector(mode = "list", length = n.boot)

  for (boot.idx in 1:n.boot) {

    data.subset <- pick_subwise_curves(data)

    mean.boot[[boot.idx]] <- functional_mean(data.subset)
  }

  func.mean.boot <- unlist(mean.boot)
  func.mean.boot <- matrix(func.mean.boot, ncol = n.boot)

  return(func.mean.boot)
}
