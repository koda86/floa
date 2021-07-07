example_data <- function(dat, dir.data) {

  if (dat == "imu_mc") {

    # IMU vs. MC validation data ---------------------------------------------------
    data <- readRDS(paste0(dir.data, "/", "data.rds"))

    # # Intermediate step: Fit functions to discrete time series data ----------------
    # # Thus far, a transformation of time series data to functional data (Fourier,
    # # Splines etc.) is not implemented.
    # fd.basis <- create.fourier.basis(nbasis=50) # Plateau around 50 basis vectors
    # fda.delta <- fdaDelta(data, fd.basis) # Fit Fourier, returns delta curves (mean, sd)

  } else if (dat == "arima") {

    # Simulated data ARIMA ---------------------------------------------------------
    set.seed(123)
    t <- 1:100
    alpha <- 6 # mean
    beta <- 0 # deterministic time-dependent trend
    theta <- 0.8
    mc <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))

    set.seed(123)
    alpha <- 6 # mean
    beta <- 0 # deterministic time-dependent trend
    theta <- 0.8
    imu <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))

    subjectID <- rep(1:n.subj, n.strides * n.devices * n.frames)
    strideID <- 1
    frames <- rep(0:n.frames, times = n.strides * n.devices)

    data <- cbind(1, 1, 1, 1, frames)

    plot(mc)

    print("ARIMA")

  } else if (dat == "fft") {

    # Intrainvidual differences

    print("fft")
  } else if (dat == "non_stationary") {

    # Simulation outsourced to script 'simulate_data'
    data <- readRDS(paste0(dir.data, "/", "data_nonstat.rds"))

    print("non stationary")

  } else if (dat == "shock") {

    # https://stats.stackexchange.com/questions/330199/simulating-drift-in-the-data

    # Constant terms
    alpha1 <- 5
    alpha2 <- 10

    # AR coefficients
    phi1 <- c(0.80,0.15)
    phi2 <- c(0.85)

    # Number of periods
    n <- 100

    # Scaling factor (coefficient) on trend
    scale1 <- 0.05 # 0.01
    scale2 <- 0.2 # 0.07

    # Initial data values
    x <- c(rep(0,n))
    z <- c(rep(0,n))

    # Error terms
    w <- rnorm(n,mean=0,sd=1)
    v <- rnorm(n,mean=0,sd=5)

    shock1 <- 150
    shock2 <- 0.02
    shocks <- TRUE # Switch this on/off to see the effect of shocks

    for(t in 3:n) {
        if(t < 50){
          x[t] <- alpha1 + phi1[1] * x[t-1] + phi1[2] * x[t-2] + w[t] + scale1*t  # AR(2) with constant and trend
          z[t] <- alpha2 + phi2[1] * z[t-1] + v[t] + scale2*t                     # AR(1) with constant and trend
        } else if(t == 50){
          x[t] <- alpha1 + shock1 + phi1[1] * x[t-1] + phi1[2]* x[t-2] + w[t] + scale1*t  # Intercept shock on
          z[t] <- alpha2 + phi2[1] * z[t-1] + v[t] + scale2*t
        }
        else if(t == 51){
          x[t] <- alpha1 - shock1 + phi1[1] * x[t-1] + phi1[2]* x[t-2] + w[t] + scale1*t  # Intercept shock off
          z[t] <- alpha2 + phi2[1] * z[t-1] + v[t] + scale2*t
        } else if(t > 51){
          x[t] <- alpha1 + phi1[1] * x[t-1] + phi1[2] * x[t-2] + w[t] + scale1*t  + 0.02 * z[t-12] # Shock to DGP (Exog variable)
          z[t] <- alpha2 + phi2[1] * z[t-1] + 5*v[t] + (scale2+shock2*t/100)*t                     # Shock to DGP (Error and trend)
        }
    }

    plot(x[1:100], type = "l")
  }

}
