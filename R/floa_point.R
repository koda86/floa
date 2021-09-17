floa_point <- function(data) {

  # ****************************************************************************
  #  Pointwise FLoA (FLoA_Point) calculated using linear mixed effects models
  # ----------------------------------------------------------------------------
  #
  # Implemetation inspired by Parker et al. (2020) Using multiple agreement methods for
  # continuous repeated measures data: a tutorial for practitioners
  # https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-020-01022-x
  #
  # [...] We focus specifically on the linear mixed effects model implementation
  # of the methods [...].  The justification of this focus is because mixed
  # effects modelling is increasingly used in clinical research and has advantages
  # over fixed effects methods (e.g. Analysis Of Variance (ANOVA)) for several
  # reasons outlined in Brown (2015). In particular, (i) missing or unbalanced
  # data poses fewer problems for analysis, and (ii) inference can be made based
  # on a wider population of patients
  # ****************************************************************************
  n.frames <- length(unique(data$frame))

  mean.diff <- vector(mode = "list", length = n.frames)
  total.sd <- vector(mode = "list", length = n.frames)
  for (frame.idx in 0:100){
    data.by.frame <- subset(data, frame == frame.idx)

    # Get variance from ANOVA (see Bland & Altman, 2007)
    # Prepare data for ANOVA
    data.by.frame.wide <- reshape2::dcast(data = data.by.frame, subjectID + strideID ~ device, value.var = "value")
    data.by.frame.wide$device.diff <- data.by.frame.wide$IMU - data.by.frame.wide$MC
    data.by.frame.wide$subjectID <- as.factor(data.by.frame.wide$subjectID)

    LMEM.2 <- aov(device.diff ~ subjectID, data = data.by.frame.wide)
    S <- summary(LMEM.2)
    # Example from Bland & Altman (2007): total.sd <- sqrt((S[[1]]$`Mean Sq`[1] - S[[1]]$`Mean Sq`[2]) / ((60^2 - 312)/(11*60)) + S[[1]]$`Mean Sq`[2] )
    # ((60^2 - 312)/(11*60)) reduces to m (here: 10) because all subjects have the same number of observations
    total.sd[[frame.idx + 1]] <- sqrt((S[[1]]$`Mean Sq`[1] - S[[1]]$`Mean Sq`[2]) / 10 + S[[1]]$`Mean Sq`[2])

    mean.diff[[frame.idx + 1]] <- mean(data.by.frame.wide$device.diff)
  }

  floa.point <- data.frame(unlist(mean.diff), unlist(total.sd))
  names(floa.point) <- c("mean.diff", "total.sd")

  # Calculate mean, upper and lower limits of agreement (boundaries) to get the
  # same structure as returned by the other methods (i. e. floa_rcb)
  floa.point.bnd <- rbind(floa.point$mean.diff + 1.96 * floa.point$total.sd,
                          floa.point$mean.diff,
                          floa.point$mean.diff - 1.96 * floa.point$total.sd
                          )

  row.names(floa.point.bnd) <- c("upper", "mean", "lower")

  return(floa.point.bnd)
}
