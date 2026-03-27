#' Translate an object to a new position
#'
#' `obj_translate()` moves a [shape] or [transform] to a new position.
#' The position can be specified in global (world) or local coordinates.
#'
#' @param obj Object to translate. Methods exist for [transform] and [shape].
#' @param to Target position, typically a [pos()] object. If missing,
#'   defaults to `pos(0, 0)`.
#' @param ... Additional arguments passed to methods.
#' @param local If `FALSE` (default), translate in world coordinates;
#'   if `TRUE`, set local offset relative to the anchor.
#'
#' @return The modified object.
#'
#' @seealso [obj_rotate()], [obj_scale()], [translate()], [window()]
#'
#' @export
obj_translate <- new_generic(
  "obj_translate",
  c("obj", "to"),
  function(obj, to,
           ...,
           local = FALSE) {
    S7_dispatch()
  }
)

#' @export
method(
  obj_translate,
  list(class_any, class_missing)
) <-
  function(obj, to,
           ...,
           local = FALSE) {
    obj_translate(
      obj,
      to = pos(0, 0),
      ...
    )
  }


#' @export
method(
  obj_translate,
  list(transform, class_pos)
) <-
  function(obj, to,
           ...,
           local = FALSE) {
    translate_trans(obj, to = to, local = local)
  }


#' Translate a transform
#'
#' @param trans A [`transform`] object.
#' @param to Target position.
#' @param local If `FALSE`, update global position; if `TRUE`, update local offset.
#'
#' @return The modified transform.
#'
#' @keywords internal
translate_trans <- function(trans, to, local = FALSE) {
  if (local) {
    trans@offset <- to
  } else {
    trans@global <- to
  }
  trans
}


#' @export
method(obj_translate, list(shape, class_pos)) <-
  function(obj, to,
           ...,
           local = FALSE) {
    obj@trans <- translate_trans(
      obj@trans,
      to = to,
      local = local
    )

    update_trans(obj)

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
