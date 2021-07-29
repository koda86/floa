crossval_coverage <- function (data, n.boot, method, ver) {

  n.subj <- unique(data$subjectID)

  cover.distro  <- c()
  cover.distro.rcb  <- c()
  cover.distro.point  <- c()

  for (i in n.subj) {

    data.subset <- subset(data, subjectID != i)

    if (method == "all") {

      # FLoA RCB
      # --------------------------------------------------------------------
      floa.boot.percentiles.intrp <- floa_rcb(data.subset, n.boot, ver)
      floa.rcb <- data.frame(t(floa.boot.percentiles.intrp))

      cover.distro.rcb <- c(cover.distro.rcb,
                            get_coverage(data, t(floa.rcb)) # Select floa method
                            )

      print(paste("floa.rcb", i))

      # FLoA Point
      # --------------------------------------------------------------------
      floa.point <- floa_point(data)
      floa.point <- data.frame(t(floa.point))

      cover.distro.point <- c(cover.distro.point,
                              get_coverage(data, t(floa.point)) # Select floa method
      )

      print(paste("floa.point", i))

    } else if (method == "floa.rcb") {

      floa.boot.percentiles.intrp <- floa_rcb(data.subset, n.boot)
      floa.rcb <- data.frame(t(floa.boot.percentiles.intrp))

      cover.distro <- c(cover.distro,
                        get_coverage(data, t(floa.rcb)) # Select floa method
      )

      print(paste("floa.rcb", i))

    } else if (method == "floa.point" | method == "all") {

      floa.point <- floa_point(data)
      floa.point <- data.frame(t(floa.point))

      cover.distro <- c(cover.distro,
                        get_coverage(data, t(floa.point)) # Select floa method
      )

      print(paste("floa.point", i))

    } else {

      print("Method does not exist")
    }

  }


  if (exists("cover.distro.rcb")) {

    # Summarize results in case more than one method was specified (method == "all")
    cover.distro <- cbind(cover.distro.rcb,
                          cover.distro.point)
  }

  return(cover.distro)
}
