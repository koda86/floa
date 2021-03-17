FLOAboot_2SD <- function(fda.delta, n.boot) {

  # Get first index of new subject
  data.subject.idx <- match(unique(data$subjectID), data$subjectID)
  fd.fda.subject.idx <- data$strideID[data.subject.idx]

  floa.boot.2SD <- list()

  for (i in seq(1, length(fda.delta), 3)) {

    stride.sample <- fda.delta[[i]][sample(fd.fda.subject.idx)]

    fd.usc <- fdata(stride.sample)

    out.boot.mean <- fdata.bootstrap(fd.usc,
                                     statistic = func.mean,
                                     alpha = 0.05,
                                     nb = n.boot,
                                     draw = FALSE) # TRUE to plot data

    fda.usc.mean.boot <- func.mean(out.boot.mean$resample)
    fda.usc.sd.boot <- sqrt(func.var(out.boot.mean$resample))

    floa.boot.2SD[[i]] <- fda.usc.mean.boot
    floa.boot.2SD[[i+1]] <- fda.usc.sd.boot
  }

  # Delete empty entries
  delete.NULLs  <-  function(x.list){
    x.list[unlist(lapply(x.list, length) != 0)]
  }

  floa.boot.2SD <- delete.NULLs(floa.boot.2SD)

  return(floa.boot.2SD)
}
