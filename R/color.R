obj_color <- new_generic("obj_color", "shape", function(shape, color) S7_dispatch())

method(obj_color, shape) <- function(shape, color) {
  shape@color <- color
  invisible(shape)
}

obj_color_between <- function(obj, color_from, color_to, amount) {
  obj_color(
    obj,
    color = lerp_colors(color_from, color_to, amount)
  )
}
