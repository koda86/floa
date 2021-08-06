fdaDelta <- function(data, fd.basis){

  # Approximates time series using Fourier functions ---------------------------

  library(Data2fd)

  # First device
  # --------------------------
  dev1 <- subset(data, device  == "IMU")$value

  dev1.wide <- matrix(dev1, ncol = length(dev1) / 101)

  # Functional data object
  dev1.wide.fd <- Data2fd(argvals = dev1.wide, basisobj = fd.basis)

  # Second device
  # --------------------------
  dev2 <- subset(data, device == "MC")$value

  dev2.wide <- matrix(dev2, ncol = length(dev2) / 101)

  dev2.wide.fd <- Data2fd(argvals = dev2.wide, basisobj = fd.basis)

  # Differences
  # --------------------------
  fd.diff <- dev1.wide.fd - dev2.wide.fd

  # point-wise statistics
  mean.diff.fd <- mean.fd(fd.diff)
  sd.diff.fd <- std.fd(fd.diff)

  fda <- list()

  fda[[1]] <- fd.diff
  fda[[2]] <- mean.diff.fd
  fda[[3]] <- sd.diff.fd

  return(fda)
}
