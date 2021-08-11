floa_roislien <- function(data, n.boot) {

  # ****************************************************************************
  # Functional limits of agreement according to Roislien et al. (2012)
  #
  # In Roislien et al., FLoA are calculated as 95% CI.
  # ****************************************************************************

  # Get one random stride from each subject ONCE and bootstrap the resulting
  # sample (of length (n=length(subjects))
  # ----------------------------------------------------------------------------

  # source("pick_subwise_curves.R")
  # source("functional_mean.R")
  # source("functional_sd.R")
  # source("boot_mean_sd.R")

  # Bootstrap to get 95% CI FLoA -----------------------------------------------
  func.boot <- boot_mean_sd(data, n.boot) # data need to have dimension [101, x]

  # Calculate Limits of Agreement ----------------------------------------------
  func.mean.boot <- functional_mean(func.boot) # mean of bootstrapped distribution
  func.sd.boot <- functional_sd(func.boot)

  # Get the same structure as returned by the other methods (i. e. floa_rcb)
  floa.roislien <- rbind(func.mean.boot + 1.96 * func.sd.boot,
                         func.mean.boot,
                         func.mean.boot - 1.96 * func.sd.boot)

  row.names(floa.roislien) <- c("upper", "mean", "lower")

  return(floa.roislien)
}
