get_positions <- new_generic(
  "get_positions",
  "obj",
  function(obj) S7_dispatch()
)

method(get_positions, shape) <- function(obj) {
  lst <- lapply(obj@children, get_positions) |>
    unlist()
  positions(obj@global, !!!lst)
}

get_extent <- function(obj) {
  positions <- get_positions(obj)
  xs <- vapply(positions, \(p) S7_data(p@x), numeric(1))
  ys <- vapply(positions, \(p) S7_data(p@y), numeric(1))

  list(
    xlim = range(xs),
    ylim = range(ys)
  )
}

plot_cords <- function(x = NULL, y = NULL, shape) {
  if (is.null(x) || is.null(y)) {
    ext <- get_extent(shape)
    xlim <- ext$xlim
    ylim <- ext$ylim
  } else {
    xlim <- range(x, na.rm = TRUE)
    ylim <- range(y, na.rm = TRUE)
  }

  xm <- 0.04 * (xlim[2] - xlim[1]) * c(1, -1)
  ym <- 0.04 * (ylim[2] - ylim[1]) * c(1, -1)
  list(
    xlim = xlim + xm,
    ylim = ylim + ym
  )
}

render_frame <- function(shape, range_x = NULL, range_y = NULL) {
  coords <- plot_cords(range_x, range_y, shape)
  po <- par(mar = c(0, 0, 0, 0))
  dev.hold()
  on.exit(dev.flush())
  plot.new()
  plot.window(xlim = coords$xlim, ylim = coords$ylim)
  render(shape)
}
