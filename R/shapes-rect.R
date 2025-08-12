rect <- new_class("rect",
  parent = shape,
  properties = list(
    width = new_property(
      class = scalar_num,
      default = 1
    ),
    height = new_property(
      class = scalar_num,
      default = 1
    )
  )
)
