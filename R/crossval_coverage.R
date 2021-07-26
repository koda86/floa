crossval_coverage <- function (data, n.boot, method) {

  n.subj <- unique(data$subjectID)

  cover.distro  <- c()

  for (i in n.subj) {

    data.subset <- subset(data, subjectID != i)

    if (method == "floa.rcb" | method == "all") {

      floa.boot.percentiles.intrp <- floa_rcb(data.subset, n.boot)
      floa.rcb <- data.frame(t(floa.boot.percentiles.intrp))

      cover.distro <- c(cover.distro,
                        get_coverage(data, t(floa.rcb)) # Select floa method
      )
    }

    if (method == "floa.point"  | method == "all") {

      floa.point <- floa_point(data)
      floa.point <- data.frame(t(floa.point))

      cover.distro <- c(cover.distro,
                        get_coverage(data, t(floa.point)) # Select floa method
      )
    } else {

      print("Method does not exist")
    }

  }
  return(cover.distro)
}
