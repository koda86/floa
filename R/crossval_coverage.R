crossval_coverage <- function (data, n.boot) {

  # ####################################################################
  # Leave-one (subject) out method to estimate the achieved coverage
  # See e. g. Lenhoff et al. (1999)
  # ####################################################################
  #
  # Currently, different versions of the sampling process in draw_clusters()
  # (nested in floa_rcb()) are implemented:
  #
  # v1 : n = length(subjects) random strides from all strides
  # v2 : One random stride per subject
  # v3 : Fetch a SINGLE random stride from all strides
  # v4 : Roislien approach (Get one random stride from each subject ONCE and boot-
  #      strap the resulting sample (of length (n=length(subjects))
  #
  # Output:
  #   * Coverage levels [%] across n=length(subjectID) iterations
  # ####################################################################

  n.subj <- unique(data$subjectID)

  floa.point  <- floa_point(data)
  floa.v1     <- floa_rcb(data, n.boot, ver = "v1")
  floa.v2     <- floa_rcb(data, n.boot, ver = "v2")
  floa.v3     <- floa_rcb(data, n.boot, ver = "v3")
  floa.roislien     <- floa_roislien(data, n.boot)

  cover.cross  <- c()
  cover.cross.rcb  <- c()
  cover.cross.point  <- c()

  for (i in n.subj) {

    data.subset <- subset(data, subjectID != i) # Leave subject "i" out

    # FLoA
    # --------------------------------------------------------------------
    cover.cross.rcb.v1 <- c(cover.cross.rcb.v1,
                             get_coverage(data.subset, floa.v1)
                             )

    cover.cross.rcb.v2 <- c(cover.cross.rcb.v2,
                             get_coverage(data.subset, floa.v2)
    )

    cover.cross.rcb.v3 <- c(cover.cross.rcb.v3,
                             get_coverage(data.subset, floa.v3)
    )

    cover.cross.roislien <- c(cover.cross.roislien,
                             get_coverage(data.subset, floa.roislien)
    )

    cover.cross.point <- c(cover.cross.point,
                            get_coverage(data.subset, floa.point)
    )
  }

  cover.cross <- cbind(cover.cross.rcb.v1,
                       cover.cross.rcb.v2,
                       cover.cross.rcb.v3,
                       cover.cross.roislien,
                       cover.cross.point)

  return(cover.cross)
}
