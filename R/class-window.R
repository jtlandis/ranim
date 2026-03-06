set_action_time <- new_generic(
  "set_action_time",
  "obj",
  function(obj, f) {
    S7_dispatch()
  }
)

set_time_mode_fps <- function(fps) {
  force(fps)
  function(obj) {
    obj@time@mode <- "fps"
    attr(obj@time, "delta_time") <- obj@time@time_scale / fps
    invisible(obj)
  }
}

set_time_mode_time <- function() {
  function(obj) {
    obj@time@mode <- "time"
    invisible(obj)
  }
}

set_time_start <- function(start_time) {
  force(start_time)
  function(obj) {
    obj@time@start_time <- start_time
    invisible(obj)
  }
}

method(set_action_time, action) <- function(obj, f) {
  f(obj)
}
method(set_action_time, act_series) <- function(obj, f) {
  for (action in obj@actions) {
    set_action_time(action, f)
  }
  f(obj)
}

method(set_action_time, shape) <- function(obj, f) {
  for (action in obj@actions) {
    set_action_time(action, f)
  }

  for (child in obj@children) {
    set_action_time(child, f)
  }
}

scale_points <- function(obj, scale) {
  for (child in obj@children) {
    if (S7_inherits(child, point)) {
      child@trans@size <- child@trans@size * scale
    }
    scale_points(child, scale)
  }
  invisible(obj)
}

window <- new_class(
  "window",
  parent = shape,
  properties = list(
    bl = pos,
    tr = pos,
    fps = new_property(
      class = new_union(NULL, scalar_num),
      default = NULL,
      setter = function(self, value) {
        if (!is.null(value) && (!is.numeric(value) ||
          length(value) != 1 || value <= 0)) {
          stop("fps must be a positive number or NULL", call. = FALSE)
        }
        if (is.null(value)) {
          ## all time actions are in real time mode
          set_action_time(self, set_time_mode_time())
        } else {
          ## all time actions are in fps mode
          set_action_time(self, set_time_mode_fps(value))
        }
        attr(self, "fps") <- value
        invisible(self)
      },
      getter = function(self) {
        attr(self, "fps")
      }
    )
  ),
  constructor = function(
    bl, tr, trans = transform(), parent = NULL,
    children = list(), actions = list(),
    color = "black", fps = NULL
  ) {
    S7::new_object(shape(
      trans = trans, parent = parent, children = children,
      actions = actions, color = color
    ), bl = new_pos(bl), tr = new_pos(tr), fps = fps)
  }
)

method(get_positions, window) <- function(obj) {
  positions(obj@bl, obj@tr)
}

#' if recursive is TRUE, rotate all children
#' if recursive is FLASE, only rotate the window
method(obj_rotate, list(window, pos)) <- function(obj, around,
                                                  ...,
                                                  radians = degrees * pi / 180,
                                                  degrees = radians * 180 / pi,
                                                  local = FALSE,
                                                  recursive = TRUE) {
  if (recursive) {
    for (child in obj@children) {
      child@trans@anchor <- around
      obj_rotate(child,
        around = around, radians = radians,
        local = TRUE,
        recursive = recursive
      )
    }
  } else {
    obj@bl <- rotate_local_pos(obj@bl, radians)
    obj@tr <- rotate_local_pos(obj@tr, radians)
    obj_rotate(
      obj = super(obj, shape),
      around = around,
      ...,
      radians = radians,
      local = local,
      recursive = FALSE
    )
  }
  invisible(obj)
}

method(
  obj_scale,
  list(window, pos)
) <- function(
  obj,
  around,
  ...,
  size,
  target_size = obj_size(obj) + size,
  scale = target_size / obj_size(obj),
  local = FALSE,
  recursive = TRUE
) {
  if (recursive) {
    for (child in obj@children) {
      child@trans@anchor <- around
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
    obj@bl <- scale_local_pos(obj@bl, scale)
    obj@tr <- scale_local_pos(obj@tr, scale)
    obj_scale(
      obj = super(obj, shape),
      around = around,
      ...,
      scale = scale,
      local = local,
      recursive = FALSE
    )
    scale_points(obj, scale = 1 / scale)
  }

  invisible(obj)
}

method(render, window) <- function(shape) {
  coords <- plot_cords(shape = shape)
  po <- par(mar = c(0, 0, 0, 0))
  dev.hold()
  on.exit(dev.flush())
  plot.new()
  plot.window(xlim = coords$xlim, ylim = coords$ylim)
  for (child in shape@children) {
    render(child)
  }
  invisible(shape)
}
