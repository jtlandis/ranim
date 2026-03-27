#' Point shape
#'
#' `point()` creates a point (dot) shape object. Points are rendered as
#' small circles at their position, with size controlled by the `size`
#' property of the shape's transform.
#'
#' @param trans A [`transform`] object giving position, size, and angle.
#'   Defaults to `transform()` (origin, size 1, no rotation).
#' @param parent An optional parent [`shape`]. If provided, this shape
#'   becomes a child of the parent.
#' @param children Optional list of child [`shape`] objects.
#' @param actions Optional list of [`action`] objects to apply.
#' @param color A color specification (see [`class_color`]). Default `"black"`.
#'
#' @return An object of class `point` (inherits from [`shape`]).
#'
#' @details
#' Points are rendered as single points on a plot. The `size` property
#' controls the point size (via the `cex` parameter in `plot.xy()`).
#'
#' @examples
#' # Create a blue point at (5, 5)
#' p <- point(
#'   color = "blue",
#'   trans = transform(pos(5, 5))
#' )
#'
#' # Create a larger point
#' p <- point(color = "red", trans = transform(pos(5, 5), size = 2))
#'
#' @seealso [`rect()`], [`polygon()`], [`text()`], [`shape`]
#'
#' @export
point <- new_class(
  "point",
  parent = shape
)

#' @export
method(render, point) <- function(shape) {
  pos <- shape@global
  plot.xy(xy.coords(pos@x, pos@y),
    pch = 16,
    type = "p",
    col = shape@color,
    cex = abs(shape@size)
  )
  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}
