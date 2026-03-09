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

method(get_positions, rect) <- function(obj) {
  pos <- obj@global
  x <- vctrs::field(pos, "x")
  y <- vctrs::field(pos, "y")
  w <- obj@width / 2
  h <- obj@height / 2
  pos(c(x + w, x - w), c(y + h, y - h))
}

get_extent <- function(obj) {
  positions <- get_positions(obj)
  xs <- vctrs::field(positions, "x")
  ys <- vctrs::field(positions, "y")

  list(
    xlim = range(xs),
    ylim = range(ys)
  )
}

center <- function(obj) {
  extent <- get_extent(obj)
  pos(mean(extent$xlim), mean(extent$ylim))
}

tl <- function(obj) {
  extent <- get_extent(obj)
  pos(extent$xlim[1], extent$ylim[2])
}

tr <- function(obj) {
  extent <- get_extent(obj)
  pos(extent$xlim[2], extent$ylim[2])
}

bl <- function(obj) {
  extent <- get_extent(obj)
  pos(extent$xlim[1], extent$ylim[1])
}

br <- function(obj) {
  extent <- get_extent(obj)
  pos(extent$xlim[2], extent$ylim[1])
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
