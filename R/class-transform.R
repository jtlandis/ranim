transform <- new_class("transform",
  properties = list(
    anchor = pos,
    offset = pos,
    global = new_property(
      class = pos,
      getter = function(self) {
        self@anchor + self@offset
      },
      setter = function(self, value) {
        if (!S7_inherits(value, pos)) {
          stop("value must be a pos object", call. = FALSE)
        }
        self@offset <- value - self@anchor
        invisible(self)
      }
    ),
    size = scalar_num,
    angle = scalar_num
  ),
  constructor = function(offset = pos(), anchor = pos()) {
    new_object(S7_object(),
      offset = offset,
      anchor = anchor,
      size = scalar(1),
      angle = scalar(0)
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
