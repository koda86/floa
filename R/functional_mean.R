functional_mean <- function (data) {

  # data need to have dimension [101, x]
  n.frames <- dim(data)[1]

  func.mean <- vector(mode = "numeric", length = n.frames)

  for (i in 1:n.frames) {
    func.mean[i] <- mean(data[i, ])
  }

  return(func.mean)
}
