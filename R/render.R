render <- new_generic("render", "shape", function(shape) S7_dispatch())

method(render, shape) <- function(shape) {
  pos <- shape@global
  plot.xy(xy.coords(pos@x, pos@y),
    pch = 16,
    type = "p",
    col = shape@color
  )
  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}

anim <- new_generic("anim", "obj", function(obj, fps = NULL) S7_dispatch())

method(anim, shape) <- function(obj, fps = NULL) {
  obj <- obj_clone(obj)

  if (!is.null(fps)) {
    obj@fps <- fps
  }
  start_time <- Sys.time()
  set_action_time(obj, set_time_start(start_time))
  while (obj@act()) {
    render(obj)
  }
  obj
}
