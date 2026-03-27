#' Rotate a shape or transform
#'
#' `obj_rotate()` rotates a [shape][shape] or [transform][transform]
#' around a given point. It updates the position and/or angle of the
#' object, and can optionally recurse into all children of a shape.
#'
#' @param obj Object to rotate. Methods are provided for [shape][shape]
#'   and [transform][transform].
#' @param around Location to rotate around. If missing, the object's
#'   global position is used (see [obj_pos()]).
#' @param ... Passed on to methods.
#' @param radians Numeric; rotation angle in radians.
#' @param degrees Numeric; rotation angle in degrees. If both are provided,
#'   `radians` takes precedence.
#' @param local Logical; if `TRUE`, rotate in the object's local
#'   coordinate system instead of world coordinates.
#'
#' @return The modified object, invisibly.
#'
#' @examples
#' w <- window(bl = 0, tr = pos(10, 10))
#' rect_obj <- rect(width = 2, height = 2, trans = transform(pos(5, 5)))
#' w@child(rect_obj)
#'
#' # Rotate 45 degrees around the center
#' obj_rotate(rect_obj, around = pos(5, 5), degrees = 45)
#'
#' @seealso [obj_scale()], [obj_translate()], [window()]
#'
#' @export
obj_rotate <- new_generic(
  "obj_rotate",
  c("obj", "around"),
  function(obj, around,
           ...,
           radians = degrees * pi / 180,
           degrees = radians * 180 / pi,
           local = FALSE) {
    if (missing(radians)) {
      if (missing(degrees)) {
        stop(
          "Either 'radians' or 'degrees' must be provided",
          call. = FALSE
        )
      }
      force(radians)
    } else if (!missing(degrees)) {
      warning(
        "Both 'radians' and 'degrees' provided; 'degrees' will be ignored",
        call. = TRUE, immediate. = TRUE
      )
    }
    S7_dispatch()
  }
)

#' @export
method(
  obj_rotate,
  list(class_any, class_missing)
) <-
  function(obj, around,
           ...,
           radians = degrees * pi / 180,
           degrees = radians * 180 / pi,
           local = FALSE) {
    obj_rotate(
      obj,
      around = obj_pos(obj),
      ...,
      radians = radians,
      local = local
    )
  }


#' @export
method(
  obj_rotate,
  list(transform, class_pos)
) <-
  function(obj, around,
           ...,
           radians = degrees * pi / 180,
           degrees = radians * 180 / pi,
           local = FALSE) {
    rotate_trans(obj, around = around, radians = radians, local = local)
  }


#' Rotate a transform
#'
#' @param trans A [`transform`] object.
#' @param around A [`pos`] point to rotate around.
#' @param radians Numeric rotation angle in radians.
#' @param local Logical; if `TRUE`, rotate in local coordinates.
#'
#' @return The modified `transform`, invisibly.
#'
#' @keywords internal
rotate_trans <- function(trans, around, radians, local = FALSE) {
  if (local) {
    trans@offset <- rotate_local_pos(obj = trans@offset, radians = radians)
  } else {
    trans@global <- rotate_pos_around_pos(
      obj = trans@global, around = around, radians = radians
    )
  }
  trans@angle <- trans@angle + (radians * 180 / pi)
  trans
}

#' Rotate a position locally
#'
#' @param obj A [`pos`] to rotate.
#' @param radians Numeric rotation angle in radians.
#'
#' @return A rotated [`pos`].
#'
#' @keywords internal
rotate_local_pos <- function(obj, radians) {
  cosa <- cos(radians)
  sina <- sin(radians)
  xp <- vctrs::field(obj, "x")
  yp <- vctrs::field(obj, "y")
  pos(
    xp * cosa - yp * sina,
    xp * sina + yp * cosa
  )
}

#' Rotate a position around another position
#'
#' @param obj A [`pos`] to rotate.
#' @param around A [`pos`] center point.
#' @param radians Numeric rotation angle in radians.
#'
#' @return A rotated [`pos`].
#'
#' @keywords internal
rotate_pos_around_pos <- function(obj, around, radians) {
  rotate_local_pos(obj - around, radians) + around
}


#' @export
method(obj_rotate, list(shape, class_pos)) <-
  function(obj, around,
           ...,
           radians = degrees * pi / 180,
           degrees = radians * 180 / pi,
           local = FALSE,
           recursive = TRUE) {
    obj@trans <- rotate_trans(
      obj@trans,
      around = around,
      radians = radians,
      local = local
    )
    new_anchor <- obj@global
    if (recursive) {
      for (child in obj@children) {
        child@trans@anchor <- new_anchor
        obj_rotate(child,
          around = around, radians = radians,
          local = TRUE,
          recursive = recursive
        )
      }
    } else {
      update_trans(obj)
    }

    obj
  }

# method(rotate_local, shape) <- function(obj, angle, ...) {
#   obj@trans <- rotate_local(obj@trans, angle)
#   new_anchor <- obj@global
#   for (child in obj@children) {
#     child@trans@anchor <- new_anchor
#     rotate_local(child, angle)
#   }
#   obj
# }

# method(rotate, list(shape, class_missing)) <-
#   function(obj, around, angle, ...) {
#     rotate(obj, around = obj@global, angle = angle)
#   }

# rotate_local <- new_generic(
#   "rotate_local",
#   "obj", function(obj, angle, ...) S7_dispatch()
# )
# method(rotate_local, pos) <- function(obj, angle, ...) {
#   rotate_local_pos(obj, angle)
# }

# method(rotate_local, transform) <- function(obj, angle, ...) {
#   obj@offset <- rotate_local_pos(obj@offset, angle)
#   obj
# }
#
# method(rotate, list(transform, pos)) <- rotate_transform_around_pos

# method(rotate, list(transform, class_missing)) <- function(
#     obj, around, angle, ...) {
#   rotate_transform_around_pos(obj, obj@anchor, angle)
# }
