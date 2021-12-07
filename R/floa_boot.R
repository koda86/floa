floa_boot <- function(data, k_reihe, n.boot, band, cp.begin, alpha, iid) {

  # Limit of Agreement calculation adapted from Lenhoff et al. (1999)
  # ----------------------------------------------------------------------------
  #
  # Function arguments:
  # k_reihe  : Number of coefficients
  # n.boot   : Number of bootstrap iterations
  # band     : Type of interval (prediction or confidence)
  # cp.begin : Initial value quantile
  # alpha    : Significance level
  # iid      : Independent (and identically distributed) data
  #            If TRUE, only one curve per subject is selected
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # Additional/initial step: Get difference curves
  # ----------------------------------------------------------------------------
  if (iid == TRUE) {
    # Pick only one curve per subject to satisfy iid assumption
    data.diff <- pick_subwise_curves(data)

    # plot(data.diff[, 1], type = "l", ylim = c(-3, 3))
    # apply(data.diff, 2, lines)
  } else if (iid == FALSE) {
    # Taking all curves would require some adjustment (e. g. block or cluster bootstrap)
    devices <- unique(data$device)

    device.1 <- subset(data, device  == devices[1])$value
    device.1.wide <- matrix(device.1, ncol = length(device.1) / 101)
    device.2 <- subset(data, device == devices[2])$value
    device.2.wide <- matrix(device.2, ncol = length(device.2) / 101)

    data.diff <- device.1.wide - device.2.wide
  }

  # Dimensions
  n_time    <- dim(data.diff)[1]
  n_curves  <- dim(data.diff)[2]
  time <- seq(0, (n_time-1))

  # ----------------------------------------------------------------------------
  # Approximate time series (differences) using Fourier functions
  # ----------------------------------------------------------------------------
  fourier.koeffi    = matlab::zeros(c(k_reihe*2 + 1, n_curves))
  fourier.real      = matlab::zeros(n_time, n_curves)
  fourier.mean      = matlab::zeros(k_reihe*2 + 1)
  fourier.real_mw   = matlab::zeros(n_time, 1)
  fourier.std1      = matlab::zeros(k_reihe*2 + 1, k_reihe*2 + 1, n_curves)
  fourier.kovarianz = matlab::zeros(k_reihe*2 + 1, k_reihe*2 + 1)
  fourier.std_all   = matlab::zeros(n_time, n_time)
  fourier.std       = matlab::zeros(n_time, 1)

  # Set up a Fourier series
  # General: f(t) = mu + sum(alpha cos(2pi*k*t/T) + beta sin(2pi*k*t/T))
  fourier.s = rep(1, times = n_time)
  for (k in seq(1, k_reihe*2, 2)) {
    fourier.s <- cbind(fourier.s, cos(2*pi*(k/2)*time/(n_time-1)))
    fourier.s <- cbind(fourier.s, sin(2*pi*(k/2)*time/(n_time-1)))
  }

  for (i in 1:n_curves) {
    # Least squares Regression
    fourier.koeffi[, i] = pracma::mldivide(fourier.s, data.diff[, i])
    # Fourier curve
    fourier.real[, i] = fourier.s %*% fourier.koeffi[, i]
  }

  # Mean Fourier curve
  fourier.mean[, 1] = rowMeans(fourier.koeffi)
  fourier.real_mw[, 1] = fourier.s %*% fourier.mean[, 1]

  # Standard deviation of the Fourier curve
  for (i in 1:n_curves) {
    # variance-covariance matrix
    fourier.std1[, , i] <- (fourier.koeffi[, i] - fourier.mean[, 1]) %*% t(fourier.koeffi[, i] - fourier.mean[, 1])
  }

  fourier.kovarianz <- apply(fourier.std1, c(1, 2), mean)
  # Lenhoff, Appendix A, Eq. (0.5)
  fourier.std_all <- sqrt(fourier.s %*% fourier.kovarianz %*% t(fourier.s))

  for (i in 1:n_time) {
    # Values are on the diagonal of the square matrix fourier.std_all
    fourier.std[i, 1] = fourier.std_all[i, i]
  }


  # ----------------------------------------------------------------------------
  # Bootstrap
  # ----------------------------------------------------------------------------
  bootstrap_sample        = matlab::zeros(n_time, 4)
  bootstrap.mean          = matlab::zeros(k_reihe*2 + 1, n.boot)
  bootstrap.real_mw       = matlab::zeros(n_time, n.boot)
  bootstrap.zz            = matlab::zeros(n_curves, n.boot)
  bootstrap.pseudo_koeffi = matlab::zeros(k_reihe*2 + 1, n_curves, n.boot)
  bootstrap.real          = matlab::zeros(n_time, n_curves, n.boot)
  bootstrap.std1          = matlab::zeros(k_reihe*2 + 1, k_reihe*2 + 1, n_curves)
  bootstrap.kovarianz     = matlab::zeros(k_reihe*2 + 1, k_reihe*2 + 1, n.boot)
  bootstrap.std_all       = matlab::zeros(n_time, n_time, n.boot)
  bootstrap.std           = matlab::zeros(n_time, n.boot)

  for (i in 1:n.boot) {
    # Create new bootstrap sample
    for (k in 1:n_curves) {
      bootstrap.zz[k, i] = sample(n_curves, 1)
      bootstrap.pseudo_koeffi[, k, i] = fourier.koeffi[, bootstrap.zz[k, i]]
      bootstrap.real[, k, i] = fourier.s %*% bootstrap.pseudo_koeffi[, k, i]
    }

    # Mean bootstrap curve and standard deviation
    bootstrap.mean[, i] <- rowMeans(bootstrap.pseudo_koeffi[, , 1])
    bootstrap.real_mw[, i] <- fourier.s %*% bootstrap.mean[, i]

    for (k in 1:n_curves) {
      bootstrap.std1[, , k] <- (bootstrap.pseudo_koeffi[, k, i] - bootstrap.mean[, i]) %*% t(bootstrap.pseudo_koeffi[, k, i] - bootstrap.mean[, i])
    }

    bootstrap.kovarianz[, , i] <- apply(bootstrap.std1, c(1, 2), mean)
    bootstrap.std_all[, , i] = sqrt(fourier.s %*% bootstrap.kovarianz[, , i] %*% t(fourier.s))

    for (k in 1:n_time) {
      bootstrap.std[k, i] <- bootstrap.std_all[k, k, i]
    }
  }


  # ----------------------------------------------------------------------------
  # Prediction band
  # ----------------------------------------------------------------------------
  cp.data   <- matlab::zeros(n_curves, n.boot)
  cp.data_i <- matlab::zeros(n_curves, n.boot)

  # Determine the quantile for alpha
  cp.mean <- 0
  cp.grenze <- cp.begin
  n <- 1

  while (cp.mean < (1 - alpha)) {
    for (i in 1:n.boot) {
      for (k in 1:n_curves) {
        # Lenhoff, Appendix A, Eq. (0.6)
        cp.data[k, i] <- max(abs(fourier.real[, k] - bootstrap.real_mw[, i]) / bootstrap.std[, i])
        cp.data_i[k, i] <- cp.data[k, i] < cp.grenze
      }
    }
    cp.mean <- mean(cp.data_i)
    cp.grenze <- cp.grenze + 0.05
    n <- n + 1
  }

  cp_out <- cp.grenze


  # ----------------------------------------------------------------------------
  # Confidence band
  # ----------------------------------------------------------------------------
  cc.data   <- matlab::zeros(n_curves, n.boot)
  cc.data_i <- matlab::zeros(n_curves, n.boot)

  # Determine the quantile for alpha
  cc.mean <- 0
  cc.grenze <- cp.begin
  n <- 1

  while (cc.mean < (1 - alpha)) {
    for (i in 1:n.boot) {
      for (k in 1:n_curves) {
        # Lenhoff, Appendix A, Eq. (0.7)
        cc.data[k, i] <- max(abs(bootstrap.real_mw[, i] - fourier.real_mw) / bootstrap.std[, i])
        cc.data_i[k, i] <- cc.data[k, i] < cc.grenze
      }
    }
    cc.mean <- mean(cc.data_i)
    cc.grenze <- cc.grenze + 0.05
    n <- n + 1
  }

  cc_out <- cc.grenze


  # ----------------------------------------------------------------------------
  # Construct bands
  # ----------------------------------------------------------------------------
  floa.boot.mean <- rowMeans(bootstrap.real_mw)
  floa.boot.sd   <- rowMeans(bootstrap.std)

  if (band == "prediction") {
    floa.boot <- rbind(floa.boot.mean + cp.grenze * floa.boot.sd,
                       floa.boot.mean,
                       floa.boot.mean - cp.grenze * floa.boot.sd
                       )
  } else {
    floa.boot <- rbind(floa.boot.mean + cc.grenze * floa.boot.sd,
                       floa.boot.mean,
                       floa.boot.mean - cc.grenze * floa.boot.sd
                       )
  }

  row.names(floa.boot) <- c("upper.loa", "mean", "lower.loa")

  return(floa.boot)

}
