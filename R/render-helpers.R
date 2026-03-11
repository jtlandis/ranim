get_positions <- new_generic(
  "get_positions",
  "obj",
  function(obj) S7_dispatch()
)

method(get_positions, shape) <- function(obj) {
  pos <- get_positions(obj@children)
  vctrs::vec_c(obj@global, pos, .ptype = new_raw_pos())
}

method(get_positions, class_pos) <- function(obj) {
  obj
}

method(get_positions, S7::class_list) <- function(obj) {
  vctrs::list_unchop(lapply(obj, get_positions), ptype = new_raw_pos())
}


get_extent <- function(obj) {
  positions <- get_positions(obj)
  range(positions)
}

center <- function(obj) {
  extent <- get_extent(obj)
  mean(extent)
}

lc <- function(obj) {
  ext <- get_extent(obj)
  pos(ext$x[1], mean(ext$y))
}

rc <- function(obj) {
  ext <- get_extent(obj)
  pos(ext$x[2], mean(ext$y))
}

tc <- function(obj) {
  ext <- get_extent(obj)
  pos(mean(ext$x), ext$y[2])
}

bc <- function(obj) {
  ext <- get_extent(obj)
  pos(mean(ext$x), ext$y[1])
}

tl <- function(obj) {
  extent <- get_extent(obj)
  pos(pos_x(extent)[1], pos_y(extent)[2])
}

tr <- function(obj) {
  extent <- get_extent(obj)
  pos(pos_x(extent)[2], pos_y(extent)[2])
}

bl <- function(obj) {
  extent <- get_extent(obj)
  pos(pos_x(extent)[1], pos_y(extent)[1])
}

br <- function(obj) {
  extent <- get_extent(obj)
  pos(pos_x(extent)[2], pos_y(extent)[1])
}

plot_cords <- function(x = NULL, y = NULL, shape) {
  if (is.null(x) || is.null(y)) {
    ext <- get_extent(shape)
    xlim <- ext$x
    ylim <- ext$y
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
