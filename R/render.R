render <- new_generic("render", "shape", function(shape) S7_dispatch())

method(render, shape) <- function(shape) {
  pos <- shape@global
  plot.xy(xy.coords(pos@x, pos@y),
    pch = 16,
    type = "p",
    col = shape@color
  )
  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}
