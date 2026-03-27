#' Polygon shape
#'
#' `polygon()` creates a polygon shape object defined by a set of points.
#' Polygons are filled shapes that can be animated and transformed.
#'
#' @param ... One or more [`pos`] objects defining the vertices of the polygon
#'   in order.
#' @param trans A [`transform`] object giving position, size, and angle.
#'   Defaults to `transform()` (origin, size 1, no rotation).
#' @param parent An optional parent [`shape`]. If provided, this shape
#'   becomes a child of the parent.
#' @param children Optional list of child [`shape`] objects.
#' @param actions Optional list of [`action`] objects to apply.
#' @param color A color specification (see [`class_color`]). Default `"black"`.
#'
#' @return An object of class `apoly` (inherits from [`shape`]).
#'
#' @details
#' Polygons are defined by a sequence of [`pos`] points that form the vertices.
#' The polygon is closed automatically by the rendering system.
#'
#' When scaling or rotating a polygon, all vertex positions are transformed
#' accordingly while maintaining their relative positions.
#'
#' @examples
#' # Create a triangle
#' tri <- polygon(
#'   pos(-1, -1),
#'   pos(1, -1),
#'   pos(0, 1),
#'   color = "blue",
#'   trans = transform(pos(5, 5))
#' )
#'
#' @seealso [`line()`], [`rect()`], [`point()`], [`shape`]
#'
#' @export
polygon <- function(...,
                    trans = transform(),
                    parent = NULL,
                    children = list(),
                    actions = list(),
                    color = "black") {
  apoly(...,
    trans = trans,
    parent = parent,
    children = children,
    actions = actions,
    color = color
  )
}

apoly <- new_class(
  "apoly",
  parent = shape,
  properties = list(
    points = class_pos
  ),
  constructor = function(...,
                         trans = transform(),
                         parent = NULL,
                         children = list(),
                         actions = list(),
                         color = "black") {
    S7::new_object(shape(
      trans = trans, parent = parent,
      children = children, color = class_color(color),
      actions = actions
    ), points = positions(...))
  }
)


method(render, apoly) <- function(shape) {
  pos <- shape@points + shape@global
  graphics::polygon(
    x = vctrs::field(pos, "x"),
    y = vctrs::field(pos, "y"),
    col = shape@color
  )
  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}

method(obj_scale, list(apoly, class_pos)) <-
  function(obj, around, ...,
           size,
           target_size = obj_size(obj) + size,
           scale = target_size / obj_size(obj),
           local = FALSE, recursive = TRUE) {
    obj@points <- scale_local_pos(
      obj@points,
      scale = scale
    )
    obj_scale(
      obj = super(obj, shape),
      around = around,
      ...,
      scale = scale,
      local = local,
      recursive = recursive
    )
  }

method(obj_rotate, list(apoly, class_pos)) <-
  function(obj, around,
           ...,
           radians = degrees * pi / 180,
           degrees = radians * 180 / pi,
           local = FALSE,
           recursive = TRUE) {
    obj@points <- rotate_local_pos(
      obj@points,
      radians = radians
    )
    obj_rotate(
      obj = super(obj, shape),
      around = around,
      ...,
      radians = radians,
      local = local,
      recursive = recursive
    )
  }

method(get_positions, apoly) <- function(obj) {
  lst <- get_positions(obj@children)
  points <- obj@points + obj@global
  positions(obj@global, points, lst)
}
