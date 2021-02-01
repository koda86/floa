draw_clusters <- function(data.long, fd.basis) {

  # ----------------------------------------------------------------------------
  # This function represents the first of two stages in the randomized cluster
  # bootstrap.
  #
  # Here, subjectwise clusters (all curves within a subject) are drawn WITH REPLACEMENT!
  # ----------------------------------------------------------------------------

  cluster.idx <- sample(unique(data.long$subjectID), replace = TRUE)

  HIP.sag.left.diff <- c()
  HIP.sag.right.diff <- c()
  KNEE.sag.left.diff <- c()
  KNEE.sag.right.diff <- c()
  ANKLE.sag.left.diff <- c()
  ANKLE.sag.right.diff <- c()

  cluster.fdata <- list()

  # Iterates over all subjects
  for (idx in 1:length(cluster.idx)) {

    ### HIP left ---------------------------------------------------------------
    HIP.sag.left.IMU <- subset(data.long,
                               subjectID == cluster.idx[idx]
                               & device == "IMU"
                               & joint == "HIP"
                               & side == "left"
    )$value

    HIP.sag.left.IMU.wide <- matrix(HIP.sag.left.IMU,
                                    ncol=length(HIP.sag.left.IMU) / 100)
    HIP.sag.left.MC <- subset(data.long,
                              subjectID == cluster.idx[idx]
                              & device == "MC"
                              & joint == "HIP"
                              & side == "left"
    )$value

    HIP.sag.left.MC.wide <- matrix(HIP.sag.left.MC,
                                   ncol=length(HIP.sag.left.MC) / 100)

    HIP.sag.left.diff.tmp <- HIP.sag.left.IMU.wide - HIP.sag.left.MC.wide

    HIP.sag.left.diff <- cbind(HIP.sag.left.diff, HIP.sag.left.diff.tmp)



    ### HIP right --------------------------------------------------------------
    HIP.sag.right.IMU <- subset(data.long,
                                subjectID == cluster.idx[idx]
                                & device == "IMU"
                                & joint == "HIP"
                                & side == "right"
    )$value

    HIP.sag.right.IMU.wide <- matrix(HIP.sag.right.IMU,
                                     ncol=length(HIP.sag.right.IMU) / 100)
    HIP.sag.right.MC <- subset(data.long,
                               subjectID == cluster.idx[idx]
                               & device == "MC"
                               & joint == "HIP"
                               & side == "right"
    )$value

    HIP.sag.right.MC.wide <- matrix(HIP.sag.right.MC,
                                    ncol=length(HIP.sag.right.MC) / 100)

    HIP.sag.right.diff.tmp <- HIP.sag.right.IMU.wide - HIP.sag.right.MC.wide

    HIP.sag.right.diff <- cbind(HIP.sag.right.diff, HIP.sag.right.diff.tmp)



    ### KNEE left --------------------------------------------------------------
    KNEE.sag.left.IMU <- subset(data.long,
                                subjectID == cluster.idx[idx]
                                & device == "IMU"
                                & joint == "KNEE"
                                & side == "left"
    )$value

    KNEE.sag.left.IMU.wide <- matrix(KNEE.sag.left.IMU,
                                     ncol=length(KNEE.sag.left.IMU) / 100)
    KNEE.sag.left.MC <- subset(data.long,
                               subjectID == cluster.idx[idx]
                               & device == "MC"
                               & joint == "KNEE"
                               & side == "left"
    )$value

    KNEE.sag.left.MC.wide <- matrix(KNEE.sag.left.MC,
                                    ncol=length(KNEE.sag.left.MC) / 100)

    KNEE.sag.left.diff.tmp <- KNEE.sag.left.IMU.wide - KNEE.sag.left.MC.wide

    KNEE.sag.left.diff <- cbind(KNEE.sag.left.diff, KNEE.sag.left.diff.tmp)



    ### KNEE right -------------------------------------------------------------
    KNEE.sag.right.IMU <- subset(data.long,
                                 subjectID == cluster.idx[idx]
                                 & device == "IMU"
                                 & joint == "KNEE"
                                 & side == "right"
    )$value

    KNEE.sag.right.IMU.wide <- matrix(KNEE.sag.right.IMU,
                                      ncol=length(KNEE.sag.right.IMU) / 100)
    KNEE.sag.right.MC <- subset(data.long,
                                subjectID == cluster.idx[idx]
                                & device == "MC"
                                & joint == "KNEE"
                                & side == "right"
    )$value

    KNEE.sag.right.MC.wide <- matrix(KNEE.sag.right.MC,
                                     ncol=length(KNEE.sag.right.MC) / 100)

    KNEE.sag.right.diff.tmp <- KNEE.sag.right.IMU.wide - KNEE.sag.right.MC.wide

    KNEE.sag.right.diff <- cbind(KNEE.sag.right.diff, KNEE.sag.right.diff.tmp)



    ### ANKLE left -------------------------------------------------------------
    ANKLE.sag.left.IMU <- subset(data.long,
                                 subjectID == cluster.idx[idx]
                                 & device == "IMU"
                                 & joint == "ANKLE"
                                 & side == "left"
    )$value

    ANKLE.sag.left.IMU.wide <- matrix(ANKLE.sag.left.IMU,
                                      ncol=length(ANKLE.sag.left.IMU) / 100)
    ANKLE.sag.left.MC <- subset(data.long,
                                subjectID == cluster.idx[idx]
                                & device == "MC"
                                & joint == "ANKLE"
                                & side == "left"
    )$value

    ANKLE.sag.left.MC.wide <- matrix(ANKLE.sag.left.MC,
                                     ncol=length(ANKLE.sag.left.MC) / 100)

    ANKLE.sag.left.diff.tmp <- ANKLE.sag.left.IMU.wide - ANKLE.sag.left.MC.wide

    ANKLE.sag.left.diff <- cbind(ANKLE.sag.left.diff, ANKLE.sag.left.diff.tmp)



    ### ANKLE right ------------------------------------------------------------
    ANKLE.sag.right.IMU <- subset(data.long,
                                  subjectID == cluster.idx[idx]
                                  & device == "IMU"
                                  & joint == "ANKLE"
                                  & side == "right"
    )$value

    ANKLE.sag.right.IMU.wide <- matrix(ANKLE.sag.right.IMU,
                                       ncol=length(ANKLE.sag.right.IMU) / 100)
    ANKLE.sag.right.MC <- subset(data.long,
                                 subjectID == cluster.idx[idx]
                                 & device == "MC"
                                 & joint == "ANKLE"
                                 & side == "right"
    )$value

    ANKLE.sag.right.MC.wide <- matrix(ANKLE.sag.right.MC,
                                      ncol=length(ANKLE.sag.right.MC) / 100)

    ANKLE.sag.right.diff.tmp <- ANKLE.sag.right.IMU.wide - ANKLE.sag.right.MC.wide

    ANKLE.sag.right.diff <- cbind(ANKLE.sag.right.diff, ANKLE.sag.right.diff.tmp)
  }

  # Functional data objects
  HIP.sag.left.diff.fd <- Data2fd(argvals = HIP.sag.left.diff, basisobj = fd.basis)
  HIP.sag.right.diff.fd <- Data2fd(argvals = HIP.sag.right.diff, basisobj = fd.basis)
  KNEE.sag.left.diff.fd <- Data2fd(argvals = KNEE.sag.left.diff, basisobj = fd.basis)
  KNEE.sag.right.diff.fd <- Data2fd(argvals = KNEE.sag.right.diff, basisobj = fd.basis)
  ANKLE.sag.left.diff.fd <- Data2fd(argvals = ANKLE.sag.left.diff, basisobj = fd.basis)
  ANKLE.sag.right.diff.fd <- Data2fd(argvals = ANKLE.sag.right.diff, basisobj = fd.basis)

  # Convert class fd to class fdata (required by fdata.bootstrap())
  cluster.fdata[[1]] <- fdata(HIP.sag.left.diff.fd)
  cluster.fdata[[2]] <- fdata(HIP.sag.right.diff.fd)
  cluster.fdata[[3]] <- fdata(KNEE.sag.left.diff.fd)
  cluster.fdata[[4]] <- fdata(KNEE.sag.right.diff.fd)
  cluster.fdata[[5]] <- fdata(ANKLE.sag.left.diff.fd)
  cluster.fdata[[6]] <- fdata(ANKLE.sag.right.diff.fd)

  return(cluster.fdata)
}
