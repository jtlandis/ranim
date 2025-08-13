rotate <- new_generic(
  "rotate",
  c("obj", "around"), function(obj, around, angle, ...) S7_dispatch()
)

rotate_local <- new_generic(
  "rotate_local",
  "obj", function(obj, angle, ...) S7_dispatch()
)

method(rotate_local, pos) <- function(obj, angle, ...) {
  rotate_local_pos(obj, angle)
}

method(rotate_local, transform) <- function(obj, angle, ...) {
  obj@offset <- rotate_local_pos(obj@offset, angle)
  obj
}

# maintain_global_set_anchor <- function(trans, anchor) {
#   shift <- trans@anchor - anchor
#   trans@anchor <- anchor
#   trans@offset <- trans@offset + shift
#   trans
# }

rotate_local_pos <- function(obj, angle) {
  angle <- angle * pi / 180
  cosa <- cos(angle)
  sina <- sin(angle)
  xp <- S7_data(obj@x)
  yp <- S7_data(obj@y)
  pos(
    xp * cosa - yp * sina,
    xp * sina + yp * cosa
  )
}

rotate_pos_around_pos <- function(obj, around, angle) {
  rotate_local_pos(obj - around, angle) + around
}

rotate_transform_around_pos <- function(obj, around, angle, ...) {
  obj@global <- rotate_pos_around_pos(obj@global, around, angle)
  obj
}

method(rotate, list(transform, pos)) <- rotate_transform_around_pos

method(rotate, list(transform, class_missing)) <- function(
    obj, around, angle, ...) {
  rotate_transform_around_pos(obj, obj@anchor, angle)
}

method(rotate, list(shape, pos)) <-
  function(obj, around, angle, new_anchor = NULL, ...) {
    obj@trans@global <- rotate_pos_around_pos(obj@trans@global, around, angle)
    new_anchor <- obj@global
    for (child in obj@children) {
      child@trans@anchor <- new_anchor
      rotate_local(child, angle = angle)
      # obj@children[[which(obj@children == child)]] <- child
    }
    obj
  }

method(rotate_local, shape) <- function(obj, angle, ...) {
  obj@trans <- rotate_local(obj@trans, angle)
  new_anchor <- obj@global
  for (child in obj@children) {
    child@trans@anchor <- new_anchor
    rotate_local(child, angle)
  }
  obj
}

method(rotate, list(shape, class_missing)) <-
  function(obj, around, angle, ...) {
    rotate(obj, around = obj@global, angle = angle)
  }
