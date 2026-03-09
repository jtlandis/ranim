render <- new_generic("render", "shape", function(shape) S7_dispatch())

method(render, shape) <- function(shape) {
  pos <- shape@global
  # plot.xy(xy.coords(pos@x, pos@y),
  #   pch = 16,
  #   type = "p",
  #   col = shape@color
  # )
  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}

.env <- new.env(parent = emptyenv())

anim <- new_generic("anim", "obj", function(obj, fps = NULL) S7_dispatch())

simple_clock <- new_class(
  "clock",
  parent = class_env,
  properties = list(
    delta_time = scalar_num_prop
  ),
  constructor = function(delta_time = 1 / 60) {
    S7::new_object(env(), delta_time = delta_time)
  }
)

timed_clock <- new_class(
  "timed_clock",
  parent = simple_clock,
  properties = list(
    last_time = new_S3_class("POSIXct"),
    delta_time = new_property(
      class = class_numeric,
      getter = function(self) {
        this_time <- Sys.time()
        delta_time <- this_time |>
          difftime(self@last_time, units = "secs") |>
          as.numeric()
        self@last_time <- this_time
        delta_time
      },
    )
  ),
  constructor = function(last_time = Sys.time()) {
    S7::new_object(simple_clock(delta_time = 0), last_time = last_time)
  }
)

method(anim, shape) <- function(obj, fps = NULL) {
  obj <- obj_clone(obj)
  .env$.last_anim <- obj
  clock <- if (!is.null(fps)) {
    simple_clock(delta_time = 1 / fps)
  } else {
    timed_clock()
  }
  # set_action_time(obj, set_time_start(start_time))
  while (obj@act(clock@delta_time)) {
    render(obj)
  }
  obj
}
