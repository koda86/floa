fdaData <- function (data.long, fd.basis) {

  # Convert discrete time series to functional data
  fdaData <- function(data.long, fd.basis){

    # ----------------------------------------------------------------------------
    # Approximates empirical time series data using Fourier series (functions)
    # ----------------------------------------------------------------------------

    ### HIP left -----------------------------------------------------------------
    HIP.sag.left.IMU <- subset(data.long,
                               device == "IMU"
                               & joint == "HIP"
                               & side == "left"
    )$value

    HIP.sag.left.IMU.wide <- matrix(HIP.sag.left.IMU,
                                    ncol=length(HIP.sag.left.IMU) / 101)
    # Functional data object
    HIP.sag.left.IMU.wide.fd <- Data2fd(argvals=HIP.sag.left.IMU.wide, basisobj=fd.basis)

    HIP.sag.left.MC <- subset(data.long,device == "MC"
                              & joint == "HIP"
                              & side == "left"
    )$value

    HIP.sag.left.MC.wide <- matrix(HIP.sag.left.MC,
                                   ncol=length(HIP.sag.left.MC) / 101)
    # Functional data object
    HIP.sag.left.MC.wide.fd <- Data2fd(argvals=HIP.sag.left.MC.wide, basisobj=fd.basis)

    # # Visually check fit
    # par(mfrow=c(2,1))
    # plot(HIP.sag.left.IMU.wide[,1], type = "l", col = "red", ylim = c(-20, 30))
    # plot(HIP.sag.left.IMU.wide.fd[,][1,1], ylim = c(-20, 30))

    HIP.sag.left.fd.diff <- HIP.sag.left.IMU.wide.fd - HIP.sag.left.MC.wide.fd
    mean.diff.fd.HIP.sag.left <- mean.fd(HIP.sag.left.fd.diff) # point-wise mean
    sd.diff.fd.HIP.sag.left <- std.fd(HIP.sag.left.fd.diff) # point-wise sd's



    ### HIP right ----------------------------------------------------------------
    HIP.sag.right.IMU <- subset(data.long,
                                device == "IMU"
                                & joint == "HIP"
                                & side == "right"
    )$value

    HIP.sag.right.IMU.wide <- matrix(HIP.sag.right.IMU,
                                     ncol=length(HIP.sag.right.IMU) / 101)
    # Functional data object
    HIP.sag.right.IMU.wide.fd <- Data2fd(argvals=HIP.sag.right.IMU.wide, basisobj=fd.basis)

    HIP.sag.right.MC <- subset(data.long,
                               device == "MC"
                               & joint == "HIP"
                               & side == "right"
    )$value

    HIP.sag.right.MC.wide <- matrix(HIP.sag.right.MC,
                                    ncol=length(HIP.sag.right.MC) / 101)
    # Functional data object
    HIP.sag.right.MC.wide.fd <- Data2fd(argvals=HIP.sag.right.MC.wide, basisobj=fd.basis)


    HIP.sag.right.fd.diff <- HIP.sag.right.IMU.wide.fd - HIP.sag.right.MC.wide.fd
    mean.diff.fd.HIP.sag.right <- mean.fd(HIP.sag.right.fd.diff)
    sd.diff.fd.HIP.sag.right <- std.fd(HIP.sag.right.fd.diff)



    ### KNEE left ----------------------------------------------------------------
    KNEE.sag.left.IMU <- subset(data.long,
                                device == "IMU"
                                & joint == "KNEE"
                                & side == "left"
    )$value

    KNEE.sag.left.IMU.wide <- matrix(KNEE.sag.left.IMU,
                                     ncol=length(KNEE.sag.left.IMU) / 101)
    # Functional data object
    KNEE.sag.left.IMU.wide.fd <- Data2fd(argvals=KNEE.sag.left.IMU.wide, basisobj=fd.basis)

    KNEE.sag.left.MC <- subset(data.long,
                               device == "MC"
                               & joint == "KNEE"
                               & side == "left"
    )$value

    KNEE.sag.left.MC.wide <- matrix(KNEE.sag.left.MC,
                                    ncol=length(KNEE.sag.left.MC) / 101)
    # Functional data object
    KNEE.sag.left.MC.wide.fd <- Data2fd(argvals=KNEE.sag.left.MC.wide, basisobj=fd.basis)


    KNEE.sag.left.fd.diff <- KNEE.sag.left.IMU.wide.fd - KNEE.sag.left.MC.wide.fd
    mean.diff.fd.KNEE.sag.left <- mean.fd(KNEE.sag.left.fd.diff)
    sd.diff.fd.KNEE.sag.left <- std.fd(KNEE.sag.left.fd.diff)



    ### KNEE right ---------------------------------------------------------------
    KNEE.sag.right.IMU <- subset(data.long,
                                 device == "IMU"
                                 & joint == "KNEE"
                                 & side == "right"
    )$value

    KNEE.sag.right.IMU.wide <- matrix(KNEE.sag.right.IMU,
                                      ncol=length(KNEE.sag.right.IMU) / 101)
    # Functional data object
    KNEE.sag.right.IMU.wide.fd <- Data2fd(argvals=KNEE.sag.right.IMU.wide, basisobj=fd.basis)

    KNEE.sag.right.MC <- subset(data.long,
                                device == "MC"
                                & joint == "KNEE"
                                & side == "right"
    )$value

    KNEE.sag.right.MC.wide <- matrix(KNEE.sag.right.MC,
                                     ncol=length(KNEE.sag.right.MC) / 101)
    # Functional data object
    KNEE.sag.right.MC.wide.fd <- Data2fd(argvals=KNEE.sag.right.MC.wide, basisobj=fd.basis)


    KNEE.sag.right.fd.diff <- KNEE.sag.right.IMU.wide.fd - KNEE.sag.right.MC.wide.fd
    mean.diff.fd.KNEE.sag.right <- mean.fd(KNEE.sag.right.fd.diff)
    sd.diff.fd.KNEE.sag.right <- std.fd(KNEE.sag.right.fd.diff)



    ### ANKLE left ---------------------------------------------------------------
    ANKLE.sag.left.IMU <- subset(data.long,
                                 device == "IMU"
                                 & joint == "ANKLE"
                                 & side == "left"
    )$value

    ANKLE.sag.left.IMU.wide <- matrix(ANKLE.sag.left.IMU,
                                      ncol=length(ANKLE.sag.left.IMU) / 101)
    # Functional data object
    ANKLE.sag.left.IMU.wide.fd <- Data2fd(argvals=ANKLE.sag.left.IMU.wide, basisobj=fd.basis)

    ANKLE.sag.left.MC <- subset(data.long,
                                device == "MC"
                                & joint == "ANKLE"
                                & side == "left"
    )$value

    ANKLE.sag.left.MC.wide <- matrix(ANKLE.sag.left.MC,
                                     ncol=length(ANKLE.sag.left.MC) / 101)
    # Functional data object
    ANKLE.sag.left.MC.wide.fd <- Data2fd(argvals=ANKLE.sag.left.MC.wide, basisobj=fd.basis)


    ANKLE.sag.left.fd.diff <- ANKLE.sag.left.IMU.wide.fd - ANKLE.sag.left.MC.wide.fd
    mean.diff.fd.ANKLE.sag.left <- mean.fd(ANKLE.sag.left.fd.diff)
    sd.diff.fd.ANKLE.sag.left <- std.fd(ANKLE.sag.left.fd.diff)



    ### ANKLE right --------------------------------------------------------------
    ANKLE.sag.right.IMU <- subset(data.long,
                                  device == "IMU"
                                  & joint == "ANKLE"
                                  & side == "right"
    )$value

    ANKLE.sag.right.IMU.wide <- matrix(ANKLE.sag.right.IMU,
                                       ncol=length(ANKLE.sag.right.IMU) / 101)
    # Functional data object
    ANKLE.sag.right.IMU.wide.fd <- Data2fd(argvals=ANKLE.sag.right.IMU.wide, basisobj=fd.basis)

    ANKLE.sag.right.MC <- subset(data.long,
                                 device == "MC"
                                 & joint == "ANKLE"
                                 & side == "right"
    )$value

    ANKLE.sag.right.MC.wide <- matrix(ANKLE.sag.right.MC,
                                      ncol=length(ANKLE.sag.right.MC) / 101)
    # Functional data object
    ANKLE.sag.right.MC.wide.fd <- Data2fd(argvals=ANKLE.sag.right.MC.wide, basisobj=fd.basis)


    ANKLE.sag.right.fd.diff <- ANKLE.sag.right.IMU.wide.fd - ANKLE.sag.right.MC.wide.fd
    mean.diff.fd.ANKLE.sag.right <- mean.fd(ANKLE.sag.right.fd.diff)
    sd.diff.fd.ANKLE.sag.right <- std.fd(ANKLE.sag.right.fd.diff)



    fda <- list()

    fda[[1]] <- HIP.sag.left.fd.diff
    fda[[2]] <- mean.diff.fd.HIP.sag.left
    fda[[3]] <- sd.diff.fd.HIP.sag.left
    fda[[4]] <- HIP.sag.right.fd.diff
    fda[[5]] <- mean.diff.fd.HIP.sag.right
    fda[[6]] <- sd.diff.fd.HIP.sag.right
    fda[[7]] <- KNEE.sag.left.fd.diff
    fda[[8]] <- mean.diff.fd.KNEE.sag.left
    fda[[9]] <- sd.diff.fd.KNEE.sag.left
    fda[[10]] <- KNEE.sag.right.fd.diff
    fda[[11]] <- mean.diff.fd.KNEE.sag.right
    fda[[12]] <- sd.diff.fd.KNEE.sag.right
    fda[[13]] <- ANKLE.sag.left.fd.diff
    fda[[14]] <- mean.diff.fd.ANKLE.sag.left
    fda[[15]] <- sd.diff.fd.ANKLE.sag.left
    fda[[16]] <- ANKLE.sag.right.fd.diff
    fda[[17]] <- mean.diff.fd.ANKLE.sag.right
    fda[[18]] <- sd.diff.fd.ANKLE.sag.right

    return(fda)
  }
}
