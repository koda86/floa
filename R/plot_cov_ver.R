plot_cov_ver <- function (cover.cross) {

  # Prepare data for (gg)plotting
  cover.cross.df <- data.frame(cover.cross)
  cover.cross.long <- reshape(cover.cross.df, direction = "long", varying = list(names(cover.cross.df)))[, 1:2]
  cover.cross.long$time <- as.factor(cover.cross.long$time)

  names(cover.cross.long)[names(cover.cross.long) != 'time'] <- 'value'
  names(cover.cross.long)[names(cover.cross.long) == 'time'] <- 'method'

  cover.PLOT <- ggplot(cover.cross.long, aes(x = method, y = value)) +
    geom_boxplot() +
    ylim(0, 1) +
    ylab("coverage")

  cover.PLOT
}
