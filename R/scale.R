obj_scale <- new_generic(
  "obj_scale",
  c("obj", "around"),
  function(obj,
           around,
           ...,
           size,
           scale = size / obj_size(obj),
           local = FALSE) {
    if (missing(size)) {
      if (missing(scale)) {
        stop("Either 'size' or 'scale' must be provided", call. = FALSE)
      } else {
        size <- obj_size(obj) * scale
      }
    } else {
      if (!missing(scale)) {
        warning(
          "Both 'size' and 'scale' provided; 'scale' will be ignored",
          call. = TRUE, immediate. = TRUE
        )
      }
      scale <- size / obj_size(obj)
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
    scale = size / obj_size(obj),
    local = FALSE,
    recursive = TRUE) {
  obj_scale(
    obj = obj,
    around = obj_anchor(obj),
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
    scale = size / obj_size(obj),
    local = FALSE) {
  scale_trans(
    trans = obj,
    around = around,
    ...,
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
    scale = size / obj_size(obj),
    local = FALSE,
    recursive = TRUE) {
  obj@trans <- scale_trans(
    trans = obj@trans,
    around = around,
    ...,
    scale = scale,
    local = local
  )
  new_anchor <- obj@global
  if (recursive && length(obj@children)) {
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
#   function(obj, around, size, scale = size / size(obj), ...) S7_dispatch()
# )

# obj_scale_local <- new_generic(
#   "obj_scale_local",
#   "obj",
#   function(obj, size, scale, ...) S7_dispatch()
# )



# method(obj_scale, list(transform, pos)) <-
#   function(obj, around, size, scale, ...) {
#     obj@global <- scale_pos_from_pos(
#       obj@global,
#       scale = scale,
#       around = around
#     )
#     obj
#   }

# method(obj_scale, list(transform, class_missing)) <-
#   function(obj, around, size, scale, ...) {
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
#     obj@size <- obj@size * scale
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
#   obj@size <- obj@size * scale
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
