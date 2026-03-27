#' Transform objects
#'
#' A `transform` encapsulates the spatial properties of a [shape]:
#' position, size, and rotation angle. Transforms use an anchor point
#' (typically the parent's position) and an offset to compute the
#' global position.
#'
#' @section Properties:
#' * `anchor`: A [`pos`] object marking the reference point
#'   (typically inherited from the parent shape).
#' * `offset`: A [`pos`] object giving the local displacement
#'   relative to the anchor.
#' * `global`: Computed property; the sum of `anchor` and `offset`.
#'   Can be assigned to directly; assignment updates `offset`.
#' * `size`: Numeric scale factor for the shape (default 1).
#' * `angle`: Numeric rotation angle in degrees (default 0).
#'
#' @section Constructor:
#' `transform(offset = pos(0), anchor = pos(0))`
#'
#' @export
transform <- S7::new_class("transform",
  properties = list(
    anchor = scalar_pos_prop,
    offset = scalar_pos_prop,
    global = S7::new_property(
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
    S7::new_object(S7::S7_object(),
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


#' Query size of an object
#'
#' `obj_size()` retrieves the size property of a shape or transform.
#'
#' @param obj Object to query (typically a [`transform`] or [`shape`]).
#'
#' @return Numeric size value.
#'
#' @export
obj_size <- S7::new_generic(
  "obj_size",
  "obj",
  function(obj) {
    S7_dispatch()
  }
)

#' Query position of an object
#'
#' `obj_pos()` retrieves the position of a [`shape`] or [`transform`].
#'
#' @param obj Object to query.
#' @param local If `FALSE` (default), return global (world) coordinates;
#'   if `TRUE`, return local offset relative to the anchor.
#'
#' @return A [`pos`] object.
#'
#' @export
obj_pos <- S7::new_generic(
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

#' Query anchor of an object
#'
#' `obj_anchor()` retrieves the anchor point used for transformations.
#'
#' @param obj Object to query.
#'
#' @return A [`pos`] object.
#'
#' @export
obj_anchor <- S7::new_generic(
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

#' Query rotation angle of an object
#'
#' `obj_angle()` retrieves the rotation angle of a shape or transform.
#'
#' @param obj Object to query.
#'
#' @return Numeric angle in degrees.
#'
#' @export
obj_angle <- S7::new_generic(
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
