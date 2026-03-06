rect <- new_class("rect",
  parent = shape,
  properties = list(
    width = new_property(
      class = scalar_num,
      default = scalar(1),
      getter = function(self) {
        self@width
      },
      setter = function(self, value) {
        self@width <- scalar(value)
        self
      }
    ),
    height = new_property(
      class = scalar_num,
      default = scalar(1),
      getter = function(self) {
        self@height
      },
      setter = function(self, value) {
        self@height <- scalar(value)
        self
      }
    ),
    border = new_property(
      class = new_union(class_color, class_logical),
      default = NA
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
    col = shape@color,
    border = shape@border
  )
}


method(obj_scale, list(rect, pos)) <-
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
