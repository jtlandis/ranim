#' @include color.R
#' @include scale.R
#' @include rotate.R
#' @include translate.R
#' @include render.R
#' @include render-helpers.R
#' @include shape-point.R
#' @include shape-rect.R
#' @include shape-polygon.R
#' @include shape-text.R
#' @include shape-line.R
#' @include class-window.R

#' Clone objects
#'
#' `obj_clone()` creates a deep copy of an object, recursively cloning
#' all nested structures. This is essential for preserving animation
#' state without modifying the original object, and for creating
#' independent variations of complex shape hierarchies.
#'
#' @param obj Object to clone. Methods are provided for:
#'   - Basic types: returned as-is (immutable)
#'   - `list`: all elements and attributes cloned recursively
#'   - `env` and [`time`]: all properties and state cloned
#'   - `shape` and subclasses: entire hierarchy with children cloned
#'   - `action` and [`act_series`]: time and captured variables cloned
#'
#' @return A deep copy of `obj`, with all nested structures independently
#'   cloned. Modifications to the clone do not affect the original.
#'
#' @details
#' For S7 objects like [`shape`], [`action`], and [`time`], all properties
#' and internal state are recursively cloned. Parent-child relationships
#' are preserved in the cloned hierarchy.
#'
#' For actions, the captured variables in the closure are cloned,
#' ensuring that the cloned action maintains its own independent state.
#'
#' @examples
#' w <- window(bl = 0, tr = pos(10, 10))
#' rect_obj <- rect(width = 2, height = 2, trans = transform(pos(5, 5)))
#' w@child(rect_obj)
#'
#' # Clone the entire hierarchy
#' w_clone <- obj_clone(w)
#'
#' # Modify the clone without affecting the original
#' rect_obj_clone <- w_clone@children[[1]]
#' rect_obj_clone@color <- "red"
#'
#' @seealso [`tracker()`], which preserves references to specific shapes
#'   during cloning.
#'
#' @export
obj_clone <- new_generic(
  "obj_clone",
  "obj",
  function(obj) {
    S7_dispatch()
  }
)

#' @export
method(obj_clone, class_any) <- function(obj) {
  obj
}

#' @export
method(obj_clone, class_list) <- function(obj) {
  lst <- lapply(obj, obj_clone)
  attributes(lst) <- lapply(
    attributes(obj),
    obj_clone
  )
  lst
}

#' @export
method(obj_clone, class_env) <- function(obj) {
  if (S7_inherits(obj, S7_object)) {
    s7_class <- S7_class(obj)
    frmls <- formals(s7_class)
    lst <- names(frmls) |>
      lapply(function(x, obj) obj_clone(prop(obj, x)), obj = obj)
    new_obj <- do.call(
      s7_class,
      lst
    )
    nms <- ls(obj, all.names = TRUE)
    for (nm in nms) {
      new_obj[[nm]] <- obj_clone(obj[[nm]])
    }
    new_obj
  } else {
    lst <- ls(obj, all.names = TRUE) |>
      setNames(nm = _) |>
      lapply(\(name, obj) obj_clone(obj[[name]]), obj = obj)
    env(lst)
  }
}

#' @export
method(obj_clone, time) <- function(obj) {
  s7_class <- S7_class(obj)
  frmls <- formals(s7_class)
  lst <- names(frmls) |>
    lapply(function(x, obj) obj_clone(prop(obj, x)), obj = obj)
  new_obj <- do.call(
    s7_class,
    lst
  )
  nms <- ls(obj, all.names = TRUE)
  for (nm in nms) {
    new_obj[[nm]] <- obj_clone(obj[[nm]])
  }
  at <- attributes(obj)
  at <- at[-match(names(at), c("class", "S7_class"), nomatch = 0L)]
  for (nm in names(at)) {
    attr(new_obj, nm) <- obj_clone(at[[nm]])
  }
  new_obj
}

#' @export
method(obj_clone, action) <- function(obj) {
  enclose <- new.env(parent = emptyenv())
  time <- obj_clone(obj@time)
  new_act <- function() {}
  body(new_act) <- body(obj)
  formals(new_act) <- formals(obj)
  attributes(new_act) <- lapply(attributes(obj), obj_clone)
  old_enc <- environment(obj)
  for (nm in setdiff(ls(old_enc), "time")) {
    enclose[[nm]] <- obj_clone(old_enc[[nm]])
  }
  enclose$time <- attr(new_act, "time")
  environment(new_act) <- enclose
  parent.env(enclose) <- parent.env(old_enc)
  new_act
}

#' @export
method(obj_clone, act_series) <- function(obj) {
  new_act <- act_series(
    !!!lapply(obj@actions, obj_clone),
    repeating = obj@time@repeating,
  )
  at <- attributes(obj@time)
  at <- at[-match(names(at), c("class", "S7_class"), nomatch = 0L)]
  new_t <- new_act@time
  for (nm in names(at)) {
    attr(new_t, nm) <- obj_clone(at[[nm]])
  }
  new_act
}


#' @export
method(obj_clone, shape) <- function(obj) {
  # browser()
  s7_class <- S7_class(obj)
  frmls <- formals(s7_class)
  lst <- names(frmls) |>
    setdiff(c("parent", "children", "...")) |>
    setNames(nm = _) |>
    lapply(function(x, obj) obj_clone(prop(obj, x)), obj = obj)
  new_obj <- do.call(
    s7_class,
    lst
  )
  nms <- ls(obj, all.names = TRUE)
  for (nm in nms) {
    new_obj[[nm]] <- obj_clone(obj[[nm]])
  }
  # new_obj
  for (child in obj@children) {
    new_obj@child(
      obj_clone(child)
    )
  }
  new_obj
}

#' @export
method(obj_clone, apoly) <- function(obj) {
  new_obj <- obj_clone(super(obj, shape))
  new_obj@points <- obj_clone(obj@points)
  new_obj
}

#' @export
method(obj_clone, line) <- function(obj) {
  new_obj <- obj_clone(super(obj, shape))
  new_obj@points <- obj_clone(obj@points)
  new_obj
}
