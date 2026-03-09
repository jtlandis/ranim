transform <- new_class("transform",
  properties = list(
    anchor = scalar_pos_prop,
    offset = scalar_pos_prop,
    global = new_property(
      class = class_pos,
      getter = function(self) {
        self@anchor + self@offset
      },
      setter = function(self, value) {
        if (!is_scalar_pos(value)) {
          stop("value must be a pos object", call. = FALSE)
        }
        self@offset <- value - self@anchor
        invisible(self)
      }
    ),
    size = scalar_num_prop,
    angle = scalar_num_prop
  ),
  constructor = function(offset = pos(0), anchor = pos(0)) {
    new_object(S7_object(),
      offset = new_pos(offset),
      anchor = new_pos(anchor),
      size = 1,
      angle = 0
    )
  }
)

method(format, transform) <- function(x, ...) {
  sprintf("transform: %s + %s -> %s", format(x@anchor), format(x@offset), format(x@global))
}

method(print, transform) <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}


obj_size <- new_generic(
  "obj_size",
  "obj",
  function(obj) {
    S7_dispatch()
  }
)

obj_pos <- new_generic(
  "obj_pos",
  "obj",
  function(obj, local = FALSE) {
    S7_dispatch()
  }
)

method(
  obj_size,
  transform
) <- function(obj) {
  obj@size
}

obj_anchor <- new_generic(
  "obj_anchor",
  "obj",
  function(obj) {
    S7_dispatch()
  }
)

method(
  obj_anchor,
  transform
) <- function(obj) {
  obj@anchor
}

obj_angle <- new_generic(
  "obj_angle",
  "obj",
  function(obj) {
    S7_dispatch()
  }
)

method(
  obj_angle,
  transform
) <- function(obj) {
  obj@angle
}

method(obj_pos, transform) <- function(obj, local = FALSE) {
  if (local) {
    obj@offset
  } else {
    obj@global
  }
}
