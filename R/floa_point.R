floa_point <- function(data) {

  # ----------------------------------------------------------------------------
  #  Pointwise FLoA (FLoA_Point) calculated using linear mixed effects models
  # ----------------------------------------------------------------------------

  library(lme4)

  # data.long.joint <- data.long[data.long$joint==joint & data.long$side == side, ]

  mean.diff <- list()
  sd.between <- list()
  sd.within <- list()

  for (frame.idx in 0:100){

    data.by.frame <- subset(data, frame == frame.idx)

    # Get within and between subject standard deviation from framewise linear
    # mixed effects models
    LMEM <- lmer(value ~ device + (1|subjectID),
                 data = data.by.frame,
                 REML = FALSE) # method=REML to get unbiased estimates of the variance

    mean.diff[[frame.idx + 1]] <- mean(data.by.frame$value[data.by.frame$device == "IMU"] - data.by.frame$value[data.by.frame$device == "MC"])

    sd.between[[frame.idx + 1]] <- summary(LMEM)$coefficients[2, 2] # standard error of the difference MC - IMU
    sd.within[[frame.idx + 1]] <- data.frame(summary(LMEM)$varcor)$sdcor[1]
  }

  floa.point <- data.frame(unlist(mean.diff), unlist(sd.between), unlist(sd.within))
  names(floa.point) <- c("mean.diff", "sd.between", "sd.within")

  # Calculate mean, upper and lower limits of agreement (boundaries) to get the
  # same structure as returned by the other methods (i. e. floa_rcb)
  lwr.bnd <- floa.boot.percentiles.intrp[1, ]
  upr.bnd <- floa.boot.percentiles.intrp[3, ]

  floa.point.bnd <- rbind(floa.point$mean.diff + 1.96 * (floa.point$sd.between + floa.point$sd.within),
                          floa.point$mean.diff,
                          floa.point$mean.diff - 1.96 * (floa.point$sd.between + floa.point$sd.within)
                          )

  return(floa.pointbnd)
}
