#' Get positions of a shape or object
#'
#' `get_positions()` recursively collects all [`pos`] objects from a
#' shape and its children. Used internally to compute bounding boxes
#' and extents.
#'
#' @param obj A [`shape`] or other object containing positions.
#'
#' @return A [`pos`] vector containing all positions.
#'
#' @keywords internal
get_positions <- new_generic(
  "get_positions",
  "obj",
  function(obj) S7_dispatch()
)

#' @keywords internal
method(get_positions, shape) <- function(obj) {
  pos <- get_positions(obj@children)
  vctrs::vec_c(obj@global, pos, .ptype = new_raw_pos())
}

#' @keywords internal
method(get_positions, class_pos) <- function(obj) {
  obj
}

#' @keywords internal
method(get_positions, S7::class_list) <- function(obj) {
  vctrs::list_unchop(lapply(obj, get_positions), ptype = new_raw_pos())
}


#' Get extent (bounding box) of a shape
#'
#' `get_extent()` computes the bounding box of a [`shape`] by finding
#' the minimum and maximum x and y coordinates of all positions in
#' the shape hierarchy.
#'
#' @param obj A [`shape`] object.
#'
#' @return A [`pos`] object with two elements: `[1]` is the bottom-left
#'   corner (min x, min y), and `[2]` is the top-right corner (max x, max y).
#'
#' @keywords internal
#'
#' @seealso [`get_positions()`], [`center()`], [`lc()`], [`rc()`], [`tc()`], [`bc()`]
get_extent <- function(obj) {
  positions <- get_positions(obj)
  range(positions)
}

#' Get the center of a shape
#'
#' `center()` computes the center point of a [`shape`]'s bounding box.
#'
#' @param obj A [`shape`] object.
#'
#' @return A [`pos`] object representing the center.
#'
#' @keywords internal
#'
#' @seealso [`get_extent()`], [`lc()`], [`rc()`], [`tc()`], [`bc()`]
center <- function(obj) {
  extent <- get_extent(obj)
  mean(extent)
}

#' Get edge centers of a shape
#'
#' These functions return the center points of each edge of a shape's
#' bounding box.
#'
#' * `lc()`: left center
#' * `rc()`: right center
#' * `tc()`: top center
#' * `bc()`: bottom center
#'
#' @param obj A [`shape`] object.
#'
#' @return A [`pos`] object representing the requested edge center.
#'
#' @keywords internal
#'
#' @seealso [`tl()`], [`tr()`], [`bl()`], [`br()`], [`center()`]
lc <- function(obj) {
  ext <- get_extent(obj)
  pos(ext$x[1], mean(ext$y))
}

#' @rdname lc
#' @keywords internal
rc <- function(obj) {
  ext <- get_extent(obj)
  pos(ext$x[2], mean(ext$y))
}

#' @rdname lc
#' @keywords internal
tc <- function(obj) {
  ext <- get_extent(obj)
  pos(mean(ext$x), ext$y[2])
}

#' @rdname lc
#' @keywords internal
bc <- function(obj) {
  ext <- get_extent(obj)
  pos(mean(ext$x), ext$y[1])
}

#' Get corners of a shape
#'
#' These functions return the corner positions of a shape's bounding box.
#'
#' * `tl()`: top-left
#' * `tr()`: top-right
#' * `bl()`: bottom-left
#' * `br()`: bottom-right
#'
#' @param obj A [`shape`] object.
#'
#' @return A [`pos`] object representing the requested corner.
#'
#' @keywords internal
#'
#' @seealso [`lc()`], [`rc()`], [`tc()`], [`bc()`], [`center()`]
tl <- function(obj) {
  extent <- get_extent(obj)
  pos(pos_x(extent)[1], pos_y(extent)[2])
}

#' @rdname tl
#' @keywords internal
tr <- function(obj) {
  extent <- get_extent(obj)
  pos(pos_x(extent)[2], pos_y(extent)[2])
}

#' @rdname tl
#' @keywords internal
bl <- function(obj) {
  extent <- get_extent(obj)
  pos(pos_x(extent)[1], pos_y(extent)[1])
}

#' @rdname tl
#' @keywords internal
br <- function(obj) {
  extent <- get_extent(obj)
  pos(pos_x(extent)[2], pos_y(extent)[1])
}

#' Compute plot coordinates with margin
#'
#' `plot_cords()` computes the xlim and ylim arguments for plotting
#' a [`shape`], adding a 4% margin on all sides.
#'
#' @param x Optional numeric vector of x-coordinates. If `NULL`,
#'   computed from the shape's extent.
#' @param y Optional numeric vector of y-coordinates. If `NULL`,
#'   computed from the shape's extent.
#' @param shape A [`shape`] object to plot.
#'
#' @return A list with elements `xlim` and `ylim`, suitable for
#'   passing to [graphics::plot.window()].
#'
#' @keywords internal
#'
#' @seealso [`render_frame()`], [`get_extent()`]
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

#' Render a single frame with automatic coordinate scaling
#'
#' `render_frame()` sets up a graphics device with appropriate coordinate
#' scaling and then renders a [`shape`] hierarchy. It handles margin setup
#' and coordinate range computation automatically.
#'
#' @param shape A [`shape`] object to render.
#' @param range_x Optional numeric vector giving x-axis limits. If `NULL`,
#'   computed from the shape's extent.
#' @param range_y Optional numeric vector giving y-axis limits. If `NULL`,
#'   computed from the shape's extent.
#'
#' @return `NULL` invisibly. Called for side effects (rendering to device).
#'
#' @keywords internal
#'
#' @seealso [`render()`], [`plot_cords()`], [`shape`]
render_frame <- function(shape, range_x = NULL, range_y = NULL) {
  coords <- plot_cords(range_x, range_y, shape)
  po <- par(mar = c(0, 0, 0, 0))
  dev.hold()
  on.exit(dev.flush())
  plot.new()
  plot.window(xlim = coords$xlim, ylim = coords$ylim)
  render(shape)
}
