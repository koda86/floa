floa_point <- function(data) {

  # ----------------------------------------------------------------------------
  #  Pointwise FLoA (FLoA_Point) calculated using linear mixed effects models
  # ----------------------------------------------------------------------------
  # Implemetation inspired by:
  # https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-020-01022-x
  library(lme4)

  n.frames <- length(unique(data$frame))

  mean.diff <- vector(mode = "list", length = n.frames)
  total.sd <- vector(mode = "list", length = n.frames)
  # median.diff <- vector(mode = "list", length = n.frames)
  for (frame.idx in 0:100){
    data.by.frame <- subset(data, frame == frame.idx)

    # Get within and between subject standard deviation from framewise linear
    # mixed effects models
    LMEM <- lmer(value ~ device + (subjectID|strideID.rep), #  (1|subjectID) + (1|strideID.rep),
                 data = data.by.frame)
                 # REML = FALSE) # method=REML to get unbiased estimates of the variance

    mean.diff[[frame.idx + 1]] <- mean(data.by.frame$value[data.by.frame$device == "IMU"] - data.by.frame$value[data.by.frame$device == "MC"])

    # # Code from https://static-content.springer.com/esm/art%3A10.1186%2Fs12874-020-01022-x/MediaObjects/12874_2020_1022_MOESM1_ESM.pdf
    # total.sd <- sqrt(as.numeric(summary(LMEM)$varcor[1]) +
    #                  as.numeric(summary(LMEM)$varcor[2]) +
    #                  as.numeric(summary(LMEM)$sigma^2)
    #                  )

    # Modified
    total.sd[[frame.idx + 1]] <- data.frame(VarCorr(LMEM))[1, 5] + # ...
                                 data.frame(VarCorr(LMEM))[2, 5] + # ...
                                 data.frame(VarCorr(LMEM))[4, 5]   # Residual Standard Deviation
                                 # data.frame(sigma(LMEM))[1] # Residual Standard Deviation
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
