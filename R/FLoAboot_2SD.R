FLOAboot_2SD <- function(fd.fda.jointwise, n.boot, fd.fda.subject.idx) {

  # Get first index of new subject in fd.fda.jointwise -------------------------
  data.long.subject.idx <- match(unique(data.long$subjectID), data.long$subjectID)
  fd.fda.subject.idx <- data.long$strideID[data.long.subject.idx]

  floa.boot.2SD <- list()

  for (i in seq(1, length(fd.fda.jointwise), 3)) {

    stride.sample <- fd.fda.jointwise[[i]][sample(fd.fda.subject.idx)]

    fd.usc <- fdata(stride.sample) # Convert class fd to class fdata (required by fdata.bootstrap())

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
