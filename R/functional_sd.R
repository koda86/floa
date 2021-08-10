functional_sd <- function (data) {

  # data need to have dimension [101, x]
  n.frames <- dim(data)[1]

  func.sd <- vector(mode = "numeric", length = n.frames)

  for (i in 1:n.frames) {
    func.sd[i] <- sd(data[i, ])
  }

  return(func.sd)
}
