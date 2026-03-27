obj_color <- new_generic("obj_color", "shape", function(shape, color) S7_dispatch())

method(obj_color, shape) <- function(shape, color) {
  shape@color <- color
  invisible(shape)
}

#' Interpolate color between two values
#'
#' `obj_color_between()` sets a shape's color to an interpolated value
#' between two colors. This is used internally by the [`color()`] action.
#'
#' @param obj A [`shape`] object to recolor.
#' @param color_from Starting color.
#' @param color_to Ending color.
#' @param amount Numeric in \\[0, 1\\] giving interpolation fraction
#'   (0 = fully `color_from`, 1 = fully `color_to`).
#'
#' @return The modified `shape`, invisibly.
#'
#' @seealso [color()], [class_color]
#'
#' @keywords internal
obj_color_between <- function(obj, color_from, color_to, amount) {
  obj_color(
    obj,
    color = lerp_colors(color_from, color_to, amount)
  )
}
