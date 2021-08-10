floa_point <- function(data) {

  # ----------------------------------------------------------------------------
  #  Pointwise FLoA (FLoA_Point) calculated using linear mixed effects models
  # ----------------------------------------------------------------------------

  library(lme4)

  n.frames <- length(unique(data$frame))

  mean.diff <- vector(mode = "list", length = n.frames)
  median.diff <- vector(mode = "list", length = n.frames)
  sd.between <- vector(mode = "list", length = n.frames)
  sd.within <- vector(mode = "list", length = n.frames)

  for (frame.idx in 0:100){

    data.by.frame <- subset(data, frame == frame.idx)

    # Get within and between subject standard deviation from framewise linear
    # mixed effects models
    LMEM <- lmer(value ~ device + (1|subjectID),
                 data = data.by.frame,
                 REML = FALSE) # method=REML to get unbiased estimates of the variance

    mean.diff[[frame.idx + 1]] <- mean(data.by.frame$value[data.by.frame$device == "IMU"] - data.by.frame$value[data.by.frame$device == "MC"])
    median.diff[[frame.idx + 1]] <- mean(data.by.frame$value[data.by.frame$device == "IMU"] - data.by.frame$value[data.by.frame$device == "MC"])

    sd.between[[frame.idx + 1]] <- summary(LMEM)$coefficients[2, 2] # standard error of the difference MC - IMU
    sd.within[[frame.idx + 1]] <- data.frame(summary(LMEM)$varcor)$sdcor[1]
  }

  floa.point <- data.frame(unlist(median.diff), unlist(mean.diff), unlist(sd.between), unlist(sd.within))
  names(floa.point) <- c("median.diff", "mean.diff", "sd.between", "sd.within")

  # Calculate mean, upper and lower limits of agreement (boundaries) to get the
  # same structure as returned by the other methods (i. e. floa_rcb)
  floa.point.bnd <- rbind(floa.point$mean.diff + 1.96 * (floa.point$sd.between + floa.point$sd.within),
                          floa.point$mean.diff,
                          floa.point$median.diff,
                          floa.point$mean.diff - 1.96 * (floa.point$sd.between + floa.point$sd.within)
                          )

  row.names(floa.point.bnd) <- c("upper", "mean", "median", "lower")

  return(floa.point.bnd)
}
