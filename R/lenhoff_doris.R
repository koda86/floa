floa_lenhoff <- function(data, k_reihe, n.boot, band, cp.begin, alpha) {

  # Limit of Agreement calculation adapted from Lenhoff et al. (1999)
  # ----------------------------------------------------------------------------
  #
  # Function arguments:
  # k_reihe  : number of coefficients
  # n.boot   : Number of bootstrap iterations
  # band     : Type of interval (prediction or confidence)
  # cp.begin : Anfangswert Quantil
  # alpha    : Irrtumswahrscheinlichkeit
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # Get difference curves
  # ----------------------------------------------------------------------------

  devices <- unique(data$device)

  device.1 <- subset(data, device  == devices[1])$value
  device.1.wide <- matrix(device.1, ncol = length(device.1) / 101)
  device.2 <- subset(data, device == devices[2])$value
  device.2.wide <- matrix(device.2, ncol = length(device.2) / 101)

  data.diff <- device.1.wide - device.2.wide

  # Dimensions
  n_time    <- dim(data.diff)[1]
  n_kurven  <- dim(data.diff)[2]
  time <- seq(0, (n_time-1)) # 0:(n_time-1)

  # ----------------------------------------------------------------------------
  # Approximate time series (differences) using Fourier functions
  # ----------------------------------------------------------------------------

  fourier.koeffi    = matlab::zeros(c(k_reihe*2 + 1, n_kurven))
  fourier.real      = matlab::zeros(n_time, n_kurven)
  fourier.mean      = matlab::zeros(k_reihe*2 + 1)
  fourier.real_mw   = matlab::zeros(n_time, 1)
  fourier.std1      = matlab::zeros(k_reihe*2 + 1, k_reihe*2 + 1, n_kurven)
  fourier.kovarianz = matlab::zeros(k_reihe*2 + 1, k_reihe*2 + 1)
  fourier.std_all   = matlab::zeros(n_time, n_time)
  fourier.std       = matlab::zeros(n_time, 1)

  # Fourierreihe aufstellen
  # General: f(t) = mu + sum(alpha cos(2pi*k*t/T) + beta sin(2pi*k*t/T))
  fourier.s = rep(1, times = n_time)
  for (k in seq(1, k_reihe*2, 2)) {
    fourier.s <- cbind(fourier.s, cos(2*pi*(k/2)*time/(n_time-1)))
    fourier.s <- cbind(fourier.s, sin(2*pi*(k/2)*time/(n_time-1)))
  }

  for (i in 1:n_time) {
    # Least squares Regression
    fourier.koeffi[, i] = pracma::mldivide(fourier.s, data.diff[, i])
    # Fourier curve
    fourier.real[, i] = fourier.s %*% fourier.koeffi[, i]
  }

  # Mean Fourier curve
  fourier.mean[, 1] = rowMeans(fourier.koeffi)
  fourier.real_mw[, 1] = fourier.s %*% fourier.mean[, 1]

  # Standard deviation of the Fourier curve
  for (i in 1:n_kurven) {
    # variance-covariance matrix
    fourier.std1[, , i] <- (fourier.koeffi[, i] - fourier.mean[, 1]) %*% t(fourier.koeffi[, i] - fourier.mean[, 1])
  }

  fourier.kovarianz <- apply(fourier.std1, c(1, 2), mean)
  # Lenhoff, Appendix A, Eq. (0.5)
  fourier.std_all <- sqrt(fourier.s %*% fourier.kovarianz %*% t(fourier.s))

  for (i in 1:n_time) {
    # Werte liegen auf der Diagonalen der quadratischen Matrix fourier.std_all
    fourier.std[i, 1] = fourier.std_all[i, i]
  }


  # ----------------------------------------------------------------------------
  # Bootstrap
  # ----------------------------------------------------------------------------

  bootstrap_sample        = matlab::zeros(n_time, 4)
  bootstrap.mean          = matlab::zeros(k_reihe*2 + 1, n.boot)
  bootstrap.real_mw       = matlab::zeros(n_time, n.boot)
  bootstrap.zz            = matlab::zeros(n_kurven, n.boot)
  bootstrap.pseudo_koeffi = matlab::zeros(k_reihe*2 + 1, n_kurven, n.boot)
  bootstrap.real          = matlab::zeros(n_time, n_kurven, n.boot)
  bootstrap.std1          = matlab::zeros(k_reihe*2 + 1, k_reihe*2 + 1, n_kurven)
  bootstrap.kovarianz     = matlab::zeros(k_reihe*2 + 1, k_reihe*2 + 1, n.boot)
  bootstrap.std_all       = matlab::zeros(n_time, n_time, n.boot)
  bootstrap.std           = matlab::zeros(n_time, n.boot)

  for (i in 1:n.boot) {
    # neues Bootstrap Sample bilden
    for (k in 1:n_kurven) {
      bootstrap.zz[k, i] = sample(n_kurven, 1)
      bootstrap.pseudo_koeffi[, k, i] = fourier.koeffi[, bootstrap.zz[k, i]]
      bootstrap.real[, k, i] = fourier.s %*% bootstrap.pseudo_koeffi[, k, i]
    }

    # mittlere Bootstrap-Kurve und Standardabweichung
    bootstrap.mean[, i] <- rowMeans(bootstrap.pseudo_koeffi[, , 1])
    bootstrap.real_mw[, i] <- fourier.s %*% bootstrap.mean[, i]

    for (k in 1:n_kurven) {
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

  cp.data   <- matlab::zeros(n_kurven, n.boot)
  cp.data_i <- matlab::zeros(n_kurven, n.boot)

  # Bestimmung des Quantils fuer alpha
  cp.mean <- 0
  cp.grenze <- cp.begin
  n <- 1

  while (cp.mean < (1 - alpha)) {
    for (i in 1:n.boot) {
      for (k in 1:n_kurven) {
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

  cc.data   <- matlab::zeros(n_kurven, n.boot)
  cc.data_i <- matlab::zeros(n_kurven, n.boot)

  # Bestimmung des Quantils fuer alpha
  cc.mean <- 0
  cc.grenze <- cp.begin
  n <- 1

  while (cc.mean < (1 - alpha)) {
    for (i in 1:n.boot) {
      for (k in 1:n_kurven) {
        # Lenhoff, Appendix A, Eq. (0.7)
        cc.data[k, i] <- max(abs(bootstrap.real_mw[, i] - fourier.real_mw) / bootstrap.std[, i])
        # max_sd<-c(max_sd,max(abs(curve_list[[c]][,1]-curve_list[[which(names(curve_list)=="mean_curve")]][,a])/curve_list[[which(names(curve_list)=="sd_curve")]][,a]))
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

  floa.lenhoff.mean <- rowMeans(bootstrap.real_mw)
  floa.lenhoff.sd   <- rowMeans(bootstrap.std)

  if (band == "prediction") {
    floa.lenhoff <- rbind(floa.lenhoff.mean,
                          floa.lenhoff.mean + cp.grenze * floa.lenhoff.sd,
                          floa.lenhoff.mean - cp.grenze * floa.lenhoff.sd
                          )
  } else {
    floa.lenhoff <- rbind(floa.lenhoff.mean,
                          floa.lenhoff.mean + cc.grenze * floa.lenhoff.sd,
                          floa.lenhoff.mean - cc.grenze * floa.lenhoff.sd
                          )
  }

  row.names(floa.lenhoff) <- c("mean", "upper", "lower")

  # plot(floa.lenhoff["mean", ], type = "l", ylim = c(-5, 5))
  # lines(floa.lenhoff["upper", ])
  # lines(floa.lenhoff["lower", ])

  return(floa.lenhoff)

}
