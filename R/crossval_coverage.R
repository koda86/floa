crossval_coverage <- function (data, floa, floa.point, method) {

  n.subj <- unique(data$subjectID)

  cover.distro  <- c()
  cover.distro.rcb  <- c()
  cover.distro.point  <- c()

  for (i in n.subj) {

    # Leave subject "i" out
    data.subset <- subset(data, subjectID != i)

    if (method == "all") {

      # FLoA RCB
      # --------------------------------------------------------------------
      cover.distro.rcb <- c(cover.distro.rcb,
                            get_coverage(data.subset, floa)
                            )
      # print(paste("floa.rcb", i))

      # FLoA Point
      # --------------------------------------------------------------------
      cover.distro.point <- c(cover.distro.point,
                              get_coverage(data.subset, floa.point)
      )
      # print(paste("floa.point", i))

    } else if (method == "floa.rcb") {

      cover.distro <- c(cover.distro,
                        get_coverage(data.subset, floa)
      )

    } else if (method == "floa.point" | method == "all") {

      cover.distro <- c(cover.distro,
                        get_coverage(data.subset, floa.point)
      )

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
