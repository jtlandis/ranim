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


method(
  obj_rotate,
  list(transform, pos)
) <-
  function(obj, around,
           ...,
           radians = degrees * pi / 180,
           degrees = radians * 180 / pi,
           local = FALSE) {
    rotate_trans(obj, around = around, radians = radians, local = local)
  }


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

rotate_local_pos <- function(obj, radians) {
  cosa <- cos(radians)
  sina <- sin(radians)
  xp <- S7_data(obj@x)
  yp <- S7_data(obj@y)
  pos(
    xp * cosa - yp * sina,
    xp * sina + yp * cosa
  )
}

rotate_pos_around_pos <- function(obj, around, radians) {
  rotate_local_pos(obj - around, radians) + around
}



method(obj_rotate, list(shape, pos)) <-
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
        # obj@children[[which(obj@children == child)]] <- child
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
