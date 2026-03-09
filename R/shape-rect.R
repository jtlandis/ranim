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
