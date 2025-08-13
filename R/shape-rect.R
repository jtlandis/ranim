rect <- new_class("rect",
  parent = shape,
  properties = list(
    width = new_property(
      class = scalar_num,
      default = scalar(1)
    ),
    height = new_property(
      class = scalar_num,
      default = scalar(1)
    )
  )
)


method(render, rect) <- function(shape) {
  pos <- shape@global
  x <- S7_data(pos@x)
  y <- S7_data(pos@y)
  w <- S7_data(shape@width)
  h <- S7_data(shape@height)
  graphics::rect(
    xleft = x - w / 2,
    ybottom = y - h / 2,
    xright = x + w / 2,
    ytop = y + h / 2,
    col = shape@color
  )
}
