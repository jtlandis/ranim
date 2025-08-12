plot_cords <- function(x, y) {
  xlim <- range(x, na.rm = TRUE)
  ylim <- range(y, na.rm = TRUE)
  xm <- 0.04 * (xlim[2] - xlim[1]) * c(1, -1)
  ym <- 0.04 * (ylim[2] - ylim[1]) * c(1, -1)
  list(
    xlim = xlim + xm,
    ylim = ylim + ym
  )
}

render_frame <- function(shape, range_x, range_y) {
  coords <- plot_cords(range_x, range_y)
  po <- par(mar = c(0, 0, 0, 0))
  dev.hold()
  on.exit(dev.flush())
  plot.new()
  plot.window(xlim = coords$xlim, ylim = coords$ylim)
  render(shape)
}
