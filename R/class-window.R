#' @include render-helpers.R
#' @include rotate.R
#' @include scale.R
#' @include render.R

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

#' Window objects
#'
#' A `window` is a special [`shape`] that defines the canvas for animation.
#' It has a bounding box (via `bl` and `tr` for bottom-left and top-right),
#' and optional FPS settings that apply to all children.
#'
#' @section Properties:
#' * `bl`: A [`pos`] for the bottom-left corner.
#' * `tr`: A [`pos`] for the top-right corner.
#' * `fps`: Frames per second for all actions. If `NULL`, uses real-time
#'   mode; if numeric, sets fixed frame timing.
#' * All properties inherited from [shape]: `trans`, `parent`,
#'   `children`, `actions`, `color`, etc.
#'
#' @section Constructor:
#' `window(bl, tr, trans = transform(), parent = NULL, children = list(),
#' actions = list(), color = "#ffffff00", fps = NULL)`
#'
#' @param bl A [`pos`] for the bottom-left corner of the window.
#' @param tr A [`pos`] for the top-right corner of the window.
#' @param trans A [`transform`] object. Defaults to `transform()`.
#' @param parent An optional parent [shape]
#' @param children Optional list of child [shape] objects.
#' @param actions Optional list of [`action`] objects to apply.
#' @param color A color specification. Default is transparent white.
#' @param fps Frames per second for animation. If `NULL` (default),
#'   uses real-time mode. If numeric, uses fixed frame timing.
#'
#' @return An object of class `window` (inherits from [shape]).
#'
#' @examples
#' # Create a 10x5 window
#' w <- window(bl = pos(0, 0), tr = pos(10, 5))
#'
#' # With FPS set to 25
#' w <- window(bl = 0, tr = pos(10, 5), fps = 25)
#'
#' @seealso [shape], [rect()], [anim()], [render_gif()]
#'
#' @export
window <- new_class(
  "window",
  parent = shape,
  properties = list(
    bl = scalar_pos_prop,
    tr = scalar_pos_prop,
    fps = new_property(
      class = new_union(NULL, class_numeric),
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
    color = "#ffffff00", fps = NULL
  ) {
    S7::new_object(
      shape(
        trans = trans, parent = parent, children = children,
        actions = actions, color = color
      ),
      bl = vctrs::vec_cast(bl, new_raw_pos()),
      tr = vctrs::vec_cast(tr, new_raw_pos()), fps = fps
    )
  }
)

method(get_positions, window) <- function(obj) {
  positions(obj@bl, obj@tr)
}

#' Window rotation helper
#'
#' If `recursive` is `TRUE`, rotate all children; if `FALSE`, only rotate
#' the window itself.
#'
#' @name window-rotation
#' @keywords internal
NULL

method(obj_rotate, list(window, class_pos)) <- function(obj, around,
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
  list(window, class_pos)
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
