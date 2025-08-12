point <- new_class(
  "point",
  parent = shape,
  properties = list(
    size = scalar_num,
  )
)

method(render, point) <- function(shape) {
  pos <- shape@global
  plot.xy(xy.coords(pos@x, pos@y),
    pch = 16,
    type = "p",
    col = shape@color,
    cex = shape@size
  )
  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}
