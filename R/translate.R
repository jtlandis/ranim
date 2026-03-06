obj_translate <- new_generic(
  "obj_translate",
  c("obj", "to"),
  function(obj, to,
           ...,
           local = FALSE) {
    S7_dispatch()
  }
)

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


method(
  obj_translate,
  list(transform, pos)
) <-
  function(obj, to,
           ...,
           local = FALSE) {
    translate_trans(obj, to = to, local = local)
  }


translate_trans <- function(trans, to, local = FALSE) {
  if (local) {
    trans@offset <- to
  } else {
    trans@global <- to
  }
  trans
}


method(obj_translate, list(shape, pos)) <-
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
