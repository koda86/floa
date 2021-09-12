floa_point <- function(data) {

  # ----------------------------------------------------------------------------
  #  Pointwise FLoA (FLoA_Point) calculated using linear mixed effects models
  # ----------------------------------------------------------------------------
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
  library(lme4)

  n.frames <- length(unique(data$frame))

  mean.diff <- vector(mode = "list", length = n.frames)
  total.sd <- vector(mode = "list", length = n.frames)
  for (frame.idx in 0:100){
    data.by.frame <- subset(data, frame == frame.idx)

    # Get within and between subject standard deviation from framewise linear
    # mixed effects models
    LMEM <- lmer(value ~ device + (1|subjectID), #  (1 + subjectID|strideID.rep)
                 data = data.by.frame)

    mean.diff[[frame.idx + 1]] <- mean(data.by.frame$value[data.by.frame$device == "IMU"] - data.by.frame$value[data.by.frame$device == "MC"])

    # Code from Parker et al. (2020)
    # https://static-content.springer.com/esm/art%3A10.1186%2Fs12874-020-01022-x/MediaObjects/12874_2020_1022_MOESM1_ESM.pdf
    # lmer(d ~ (1|subject) + (1|activity))
    # total.sd <- sqrt(as.numeric(summary(LMEM)$varcor[1]) + # Comment DK: (1| subjectID)
    #                  as.numeric(summary(LMEM)$varcor[2]) + # Comment DK: (1|activity)
    #                  as.numeric(summary(LMEM)$sigma^2) # Comment DK: Residual Standard Deviation
    #                  )

    total.sd[[frame.idx + 1]] <- sqrt(data.frame(VarCorr(LMEM))[1, 4] + # Estimated random-effects variance (subjectID)
                                      data.frame(VarCorr(LMEM))[2, 4]   # Residual Standard Deviation
                                      )
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
