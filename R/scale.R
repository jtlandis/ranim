#' @param obj dispatch arg to scale
#' @param around location to scale from
#' @param ... for extensions
#' @param size,target_size,scale These arguments
#' do much of the same thing. `scale` determines the
#' scaling factor that will be applied to transforms.
#' target_scale represents the end size value the transform
#' will have. `size` represents a value or delta to change
#' the objects current size value. Note that for `size` and
#' `target_size` are merely reparameteriztions for `scale`
#' which will be the value used by inner functions.
#' @export
obj_scale <- new_generic(
  "obj_scale",
  c("obj", "around"),
  function(obj,
           around,
           ...,
           size,
           target_size = obj_size(obj) + size,
           scale = target_size / obj_size(obj),
           local = FALSE) {
    if (missing(size)) {
      if (missing(target_size)) {
        if (missing(scale)) {
          stop("Either 'size' or 'scale' or 'target_size' must be provided",
            call. = FALSE
          )
        } else {
          target_size <- obj_size(obj) * scale
        }
      } else {
        force(scale)
      }
    } else if (!missing(scale)) {
      warning(
        "Both 'size' and 'scale' provided; 'size' will be ignored",
        call. = TRUE, immediate. = TRUE
      )
    }
    if (abs(scale) < 1e-12) {
      scale <- if (scale == 0) {
        1e-12
      } else {
        1e-12 * sign(scale)
      }
    }
    S7_dispatch()
  }
)

method(
  obj_scale,
  list(class_any, class_missing)
) <- function(
    obj,
    around,
    ...,
    size,
    target_size = obj_size(obj) + size,
    scale = target_size / obj_size(obj),
    local = FALSE,
    recursive = TRUE) {
  obj_scale(
    obj = obj,
    around = obj_pos(obj),
    ...,
    scale = scale,
    local = local,
    recursive = recursive
  )
}

method(
  obj_scale,
  list(transform, pos)
) <- function(
    obj,
    around,
    ...,
    size,
    target_size = obj_size(obj) + size,
    scale = target_size / obj_size(obj),
    local = FALSE) {
  scale_trans(
    trans = obj,
    around = around,
    scale = scale,
    local = local
  )
}

method(
  obj_scale,
  list(shape, pos)
) <- function(
    obj,
    around,
    ...,
    size,
    target_size = obj_size(obj) + size,
    scale = target_size / obj_size(obj),
    local = FALSE,
    recursive = TRUE) {
  obj@trans <- scale_trans(
    trans = obj@trans,
    around = around,
    scale = scale,
    local = local
  )
  new_anchor <- obj@global
  if (recursive) {
    for (child in obj@children) {
      child@trans@anchor <- new_anchor
      obj_scale(
        obj = child,
        around = around,
        ...,
        scale = scale,
        local = TRUE,
        recursive = recursive
      )
    }
  } else {
    update_trans(obj)
  }
  obj
}

scale_trans <- function(trans, around, scale, local = FALSE) {
  if (local) {
    trans@offset <- scale_local_pos(
      trans@offset,
      scale = scale
    )
  } else {
    trans@global <- scale_pos_from_pos(
      trans@global,
      scale = scale,
      around = around
    )
  }
  trans@size <- trans@size * scale
  trans
}

scale_local_pos <- function(pos, scale) {
  pos * scale
}

scale_pos_from_pos <- function(pos, scale, around) {
  scale_local_pos(pos = pos - around, scale = scale) + around
}

scale_points_from_pos <- function(pts, scale, pos) {
  out <- lapply(pts,
    scale_pos_from_pos,
    around = pos,
    scale = scale
  )
  positions(!!!out)
}

# obj_scale <- new_generic(
#   "obj_scale",
#   c("obj", "around"),
#   function(obj, around, target_size, scale = target_size / target_size(obj), ...) S7_dispatch()
# )

# obj_scale_local <- new_generic(
#   "obj_scale_local",
#   "obj",
#   function(obj, target_size, scale, ...) S7_dispatch()
# )



# method(obj_scale, list(transform, pos)) <-
#   function(obj, around, target_size, scale, ...) {
#     obj@global <- scale_pos_from_pos(
#       obj@global,
#       scale = scale,
#       around = around
#     )
#     obj
#   }

# method(obj_scale, list(transform, class_missing)) <-
#   function(obj, around, target_size, scale, ...) {
#     obj@global <- scale_pos_from_pos(
#       pos = obj@global,
#       scale = scale,
#       around = obj@anchor
#     )
#     obj
#   }

# method(obj_scale_local, transform) <-
#   function(obj, scale, ...) {
#     obj@offset <- scale_local_pos(
#       pos = obj@offset,
#       scale = scale
#     )
#     obj
#   }

# method(obj_scale, list(shape, pos)) <-
#   function(obj, around, scale, ...) {
#     obj@trans@global <- scale_pos_from_pos(
#       pos = obj@trans@global,
#       around = around,
#       scale = scale
#     )
#     obj@target_size <- obj@target_size * scale
#     new_anchor <- obj@global
#     for (child in obj@children) {
#       child@trans@anchor <- new_anchor
#       obj_scale_local(child, scale = scale)
#       # obj@children[[which(obj@children == child)]] <- child
#     }
#     obj
#   }

# method(obj_scale_local, shape) <- function(obj, scale, ...) {
#   obj@trans <- obj_scale_local(obj@trans, scale = scale)
#   obj@target_size <- obj@target_size * scale
#   new_anchor <- obj@global
#   for (child in obj@children) {
#     child@trans@anchor <- new_anchor
#     obj_scale_local(child, scale = scale)
#   }
#   obj
# }

# method(obj_scale, list(shape, class_missing)) <-
#   function(obj, around, scale, ...) {
#     obj_scale(obj, around = obj@global, scale = scale)
#   }
