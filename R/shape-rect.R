#' Rectangle shape
#'
#' `rect()` creates a rectangular shape object. Rectangles are centered
#' at their position and scale proportionally with their width and height.
#'
#' @param trans A [`transform`] object giving position, size, and angle.
#'   Defaults to `transform()` (origin, size 1, no rotation).
#' @param parent An optional parent [`shape`]. If provided, this shape
#'   becomes a child of the parent.
#' @param children Optional list of child [`shape`] objects.
#' @param actions Optional list of [`action`] objects to apply.
#' @param color A color specification (see [`class_color`]). Default `"black"`.
#' @param width Numeric; width of the rectangle (default 1).
#' @param height Numeric; height of the rectangle (default 1).
#' @param border Color of the rectangle border, or `NA` for no border
#'   (default).
#'
#' @section Properties:
#' * `width`: Numeric width of the rectangle.
#' * `height`: Numeric height of the rectangle.
#' * `border`: Border color or `NA`.
#' * All properties inherited from [`shape`]: `trans`, `parent`,
#'   `children`, `actions`, `color`, etc.
#'
#' @return An object of class `rect` (inherits from [`shape`]).
#'
#' @examples
#' # Create a red square at (5, 5)
#' r <- rect(
#'   width = 2, height = 2,
#'   color = "red",
#'   trans = transform(pos(5, 5))
#' )
#'
#' # Add a border
#' r <- rect(width = 2, height = 2, border = "black")
#'
#' @seealso [`point()`], [`polygon()`], [`text()`], [`shape`]
#'
#' @export
rect <- new_class("rect",
  parent = shape,
  properties = list(
    width = scalar_num_prop,
    height = scalar_num_prop,
    border = new_property(
      class = new_union(class_color, class_logical),
      default = NA
    )
  )
)


#' @export
method(render, rect) <- function(shape) {
  pos <- shape@global
  x <- vctrs::field(pos, "x")
  y <- vctrs::field(pos, "y")
  w <- shape@width / 2
  h <- shape@height / 2
  graphics::rect(
    xleft = x - w,
    ybottom = y - h,
    xright = x + w,
    ytop = y + h,
    col = shape@color,
    border = shape@border
  )

  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}


#' @export
method(obj_scale, list(rect, class_pos)) <-
  function(obj, around, ...,
           size,
           target_size = obj_size(obj) + size,
           scale = target_size / obj_size(obj),
           local = FALSE, recursive = TRUE) {
    obj@height <- obj@height * scale
    obj@width <- obj@width * scale

    obj_scale(
      obj = super(obj, shape),
      around = around,
      ...,
      scale = scale,
      local = local,
      recursive = recursive
    )
  }


#' @export
method(get_positions, rect) <- function(obj) {
  pos <- obj@global
  x <- vctrs::field(pos, "x")
  y <- vctrs::field(pos, "y")
  w <- obj@width / 2
  h <- obj@height / 2
  positions(pos(c(x + w, x - w), c(y + h, y - h)), get_positions(obj@children))
}


#' Calculate grid positions within an object
#'
#' `calc_grid()` computes the positions for a regular grid of cells
#' within an object's bounding box. Useful for laying out grids of
#' shapes with uniform spacing.
#'
#' @param obj A [`shape`] object whose extent is used to define the grid.
#' @param ncol Number of columns in the grid (default 1).
#' @param nrow Number of rows in the grid (default 1).
#' @param padding Padding as a fraction of available space (default 0.05).
#'   Applied uniformly unless overridden by `xpadding`/`ypadding`.
#' @param xpadding Padding in x direction as a fraction of space.
#' @param ypadding Padding in y direction as a fraction of space.
#' @param margin Margin outside the grid as a fraction of padding
#'   (default 0.5). Applied uniformly unless overridden by `xmargin`/`ymargin`.
#' @param xmargin Margin in x direction.
#' @param ymargin Margin in y direction.
#'
#' @return A list of [`pos`] vectors, one per column, where each vector
#'   contains the positions of all cells in that column. An attribute
#'   `"features"` contains the computed cell dimensions and spacing.
#'
#' @details
#' The grid is computed as follows:
#' 1. The object's extent is determined via [`get_extent()`].
#' 2. Total space is divided by padding to yield the grid cell size.
#' 3. Cell positions are computed column-by-column.
#'
#' @keywords internal
#'
#' @seealso [`spawn_grid()`], [`get_extent()`], [`get_positions()`]
calc_grid <- function(
  obj,
  ncol = 1,
  nrow = 1,
  padding = 0.05,
  xpadding = padding,
  ypadding = padding,
  margin = 0.5,
  xmargin = margin,
  ymargin = margin
) {
  # browser()
  xt <- get_extent(obj)
  .xspace <- diff(xt$x)
  .x_pad <- .xspace * xpadding
  .yspace <- diff(xt$y)
  .y_pad <- .yspace * ypadding
  nxbtwn <- ncol - 1L
  if (nxbtwn == 0L) {
    xmarg <- .x_pad
  } else {
    xmarg <- xmargin * .x_pad
  }
  xbtwn_tot <- .x_pad - xmarg
  nybtwn <- nrow - 1L
  if (nybtwn == 0L) {
    ymarg <- .y_pad
  } else {
    ymarg <- ymargin * .y_pad
  }
  ybtwn_tot <- .y_pad - ymarg
  .width <- (.xspace - .x_pad) / ncol
  .height <- (.yspace - .y_pad) / nrow
  if (nxbtwn == 0) {
    xbtwn <- 0
  } else {
    xbtwn <- xbtwn_tot / nxbtwn
  }
  ybtwn <- ybtwn_tot / (nrow - 1)
  if (nybtwn == 0) {
    ybtwn <- 0
  } else {
    ybtwn <- ybtwn_tot / nybtwn
  }
  .x_move <- xbtwn + .width
  .y_move <- -(ybtwn + .height)

  .x_start <- xt$x[1] + ((.width + xmarg) / 2)
  .y_start <- xt$y[2] - ((.height + ymarg) / 2)

  x <- .x_start + ((seq_len(ncol) - 1L) * .x_move)
  y <- .y_start + ((seq_len(nrow) - 1L) * .y_move)

  p <- vector("list", ncol)
  for (i in seq_len(ncol)) {
    p[[i]] <- pos(x[i], y)
  }
  attr(p, "features") <- c(
    height = .height, width = .width,
    xmarg = xmarg, xbtwn = xbtwn,
    ymarg = ymarg, ybtwn = ybtwn
  )
  p
}
