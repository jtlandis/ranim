obj_clone <- new_generic(
  "obj_clone",
  "obj",
  function(obj) {
    S7_dispatch()
  }
)

method(obj_clone, class_any) <- function(obj) {
  obj
}

method(obj_clone, class_list) <- function(obj) {
  lst <- lapply(obj, obj_clone)
  attributes(lst) <- lapply(
    attributes(obj),
    obj_clone
  )
  lst
}

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

method(obj_clone, act_series) <- function(obj) {
  act_series(
    !!!lapply(obj@actions, obj_clone)
  )
}


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
