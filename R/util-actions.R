#' Rotation action
#'
#' `rotate()` creates an action that rotates a [`shape`] around a point
#' over time. The rotation angle can be specified in degrees or radians.
#'
#' @param degrees Numeric; rotation angle in degrees.
#' @param radians Numeric; rotation angle in radians. If both `degrees`
#'   and `radians` are provided, `radians` takes precedence.
#' @param time A [`time`] object controlling duration and easing.
#'
#' @return An [`action`] object.
#'
#' @examples
#' w <- window(bl = 0, tr = pos(10, 10))
#' rect_obj <- rect(width = 2, height = 2, trans = transform(pos(5, 5)))
#' w@child(rect_obj)
#' rect_obj@action(rotate(degrees = 360, time = time(2)))
#'
#' @seealso [`action`], [`time`], [`translate()`], [`scale2()`]
#'
#' @export
rotate <- into_action(obj_rotate, expr_map(degrees = degrees * time@delta))

#' Scaling action
#'
#' `scale()` creates an action that continuously scales a [`shape`]
#' by a given `size` delta per unit time.
#'
#' @param size Numeric; size change per unit time. Can be positive
#'   (to grow) or negative (to shrink).
#' @param time A [`time`] object controlling duration and easing.
#'
#' @return An [`action`] object.
#'
#' @details
#' This function applies a continuous scaling over time. For scaling
#' from one specific size to another, use [`scale2()`] instead.
#'
#' @seealso [`scale2()`], [`action`], [`time`], [`rotate()`]
#'
#' @keywords internal
scale <- into_action(obj_scale, expr_map(size = size * time@delta))

#' Color transition action
#'
#' `color()` creates an action that smoothly transitions a [`shape`]'s
#' color from one to another over time, using linear interpolation.
#'
#' @param color_from Starting color (any format accepted by [`class_color`]).
#' @param color_to Ending color.
#' @param time A [`time`] object controlling duration and easing.
#'
#' @return An [`action`] object.
#'
#' @examples
#' w <- window(bl = 0, tr = pos(10, 10))
#' rect_obj <- rect(
#'   width = 2, height = 2, color = "red",
#'   trans = transform(pos(5, 5))
#' )
#' w@child(rect_obj)
#' rect_obj@action(color("red", "blue", time = time(1)))
#'
#' @seealso [`colors()`], [`action`], [`time`], [`class_color`]
#'
#' @export
color <- into_action(
  obj_color_between,
  expr_map(amount = time@value, color_from = color_from, color_to = color_to)
)

#' Multiple color transitions
#'
#' `colors()` creates an action sequence that transitions through
#' a list of colors, visiting each in sequence. Useful for
#' multi-step color animations.
#'
#' @param colors A list or character vector of colors (any format
#'   accepted by [`class_color`]).
#' @param durations Either a single numeric duration (applied to all
#'   transitions), a [`time`] object (cloned for each transition),
#'   or a list of [`time`] objects (one per transition).
#' @param repeating Number of times to repeat the entire color sequence
#'   after the first playthrough.
#'
#' @return An [`act_series`] object executing all color transitions in sequence.
#'
#' @examples
#' w <- window(bl = 0, tr = pos(10, 10))
#' rect_obj <- rect(width = 2, height = 2, trans = transform(pos(5, 5)))
#' w@child(rect_obj)
#'
#' # Cycle through red -> green -> blue, each taking 0.5 seconds
#' rect_obj@action(colors(c("red", "green", "blue"), durations = 0.5))
#'
#' @seealso [`color()`], [`act_series`]
#'
#' @export
colors <- function(colors, durations, repeating = 0) {
  n_transitions <- length(colors) - 1
  colors <- lapply(colors, class_color)

  if (length(durations) <= 1L) {
    durations <- if (is.numeric(durations)) {
      lapply(seq_len(n_transitions), function(i) time(duration = durations))
    } else if (S7_inherits(durations, time)) {
      lapply(seq_len(n_transitions), function(i) obj_clone(durations))
    } else {
      stop(
        "durations must be a numeric value or a time object or a list of time objects",
        call. = FALSE
      )
    }
  } else if (length(durations) != n_transitions) {
    stop("durations must be length 1 or length(colors) - 1", call. = FALSE)
  } else {
    durations <- if (is.numeric(durations)) {
      lapply(seq_len(n_transitions), function(i) time(duration = i))
    } else if (is.list(durations)) {
      if (!all(vapply(durations, \(x) S7_inherits(x, time), logical(1)))) {
        stop("durations must be a list durations must be a numeric value or a time object or a list of time objects time objects", call. = FALSE)
      }
    }
  }
  actions <- vector("list", length = n_transitions)
  for (i in seq_len(n_transitions)) {
    actions[[i]] <- color(
      color_from = colors[[i]],
      color_to = colors[[i + 1]],
      time = durations[[i]]
    )
  }
  act_series(!!!actions, repeating = repeating)
}

#' Add a child shape to a parent
#'
#' `spawn_from()` is a low-level helper that adds a child [`shape`]
#' to a parent shape. It is typically called from within actions
#' to dynamically create shapes during animation.
#'
#' @param parent A [`shape`] object (the parent).
#' @param child A [`shape`] object (the child to add).
#' @param offset Optional [`pos`] giving the local position of the child
#'   relative to the parent. If `NULL`, the child keeps its current
#'   global position.
#'
#' @return The parent `shape`, invisibly.
#'
#' @seealso [`spawn()`], [`spawn_sibling()`], [`shape`]
#'
#' @keywords internal
spawn_from <- function(parent, child, offset = NULL) {
  parent@child(
    child = child,
    offset = offset
  )
}

#' Spawn a clone at action completion
#'
#' `spawn()` creates an action that clones a shape and adds it as
#' a child when the action completes.
#'
#' @param obj A [`shape`] object to clone and spawn.
#' @param time A [`time`] object controlling when the spawn occurs.
#'
#' @return An [`action`] object.
#'
#' @seealso [`spawn_from()`], [`spawn_sibling()`], [`despawn()`]
#'
#' @keywords internal
spawn <- function(obj, time) {
  time <- new_time(time)
  obj_to_clone <- obj
  action(
    time = time,
    function(obj, time) {
      if (time@is_done) {
        spawn_from(parent = obj, child = obj_clone(obj_to_clone))
      }
    }
  )
}

#' Remove a shape from its parent
#'
#' `despawn()` creates an action that detaches a shape from its
#' parent when the action completes.
#'
#' @param time A [`time`] object controlling when the removal occurs.
#'   Default 0 means remove immediately.
#'
#' @return An [`action`] object.
#'
#' @seealso [`spawn()`], [`spawn_sibling()`], [`spawn_from()`]
#'
#' @keywords internal
despawn <- function(time = 0) {
  time <- new_time(time)
  action(
    function(obj, time) {
      if (time@is_done) {
        obj@parent <- NULL
      }
    },
    time = time
  )
}

#' Spawn a sibling shape
#'
#' `spawn_sibling()` creates an action that clones and adds a shape
#' as a sibling (to the same parent) when the action completes.
#'
#' @param sib A [`shape`] object to clone and spawn as a sibling.
#' @param time A [`time`] object controlling when the spawn occurs.
#' @param offset Optional [`pos`] giving the relative position of the
#'   sibling.
#'
#' @return An [`action`] object.
#'
#' @seealso [`spawn()`], [`spawn_from()`], [`despawn()`]
#'
#' @keywords internal
spawn_sibling <- function(sib, time, offset = NULL) {
  force(time)
  time <- new_time(time)
  if (is.null(offset)) {
    offset <- pos(0)
  }
  action(function(obj, time) {
    force(time)
    if (time@is_done) {
      parent <- obj@parent
      spawn_from(parent, obj_clone(sib), obj_pos(obj, local = TRUE) + offset)
    }
  }, time = time)
}

#' Duplicate the current shape
#'
#' `duplicate()` creates an action that clones the current shape
#' and adds the clone as a sibling when the action completes.
#'
#' @param time A [`time`] object controlling when the duplication occurs.
#'
#' @return An [`action`] object.
#'
#' @seealso [`spawn()`], [`spawn_sibling()`], [`spawn_from()`]
#'
#' @keywords internal
duplicate <- function(time) {
  force(time)
  time <- new_time(time)
  action(function(obj, time) {
    force(time)
    if (time@is_done) {
      parent <- obj@parent
      spawn_from(parent, obj_clone(obj))
    }
  }, time = time)
}


#' Offset a shape's position
#'
#' `obj_offset()` moves a shape by a given delta.
#'
#' @param obj A [`shape`] object.
#' @param by A [`pos`] offset to apply.
#'
#' @return The modified `shape`, invisibly.
#'
#' @keywords internal
obj_offset <- function(obj, by) {
  obj_translate(obj, to = obj_pos(obj) + by)
}

#' Offset action
#'
#' `offset()` creates an action that moves a [`shape`] by a continuous
#' delta per unit time. Over the course of the action's duration,
#' the total movement is `by * time@value`.
#'
#' @param by A [`pos`] delta (or per-time-unit delta); will be scaled
#'   by `time@delta` during animation.
#' @param time A [`time`] object controlling duration and easing.
#'
#' @return An [`action`] object.
#'
#' @seealso [`translate()`], [`action`], [`time`]
#'
#' @keywords internal
offset <- into_action(
  obj_offset,
  expr_map(
    by = by * time@delta
  )
)

#' Translation action
#'
#' `translate()` creates an action that moves a [`shape`] from its
#' current position to a target position over time.
#'
#' @param to A [`pos`] giving the target position in world coordinates.
#' @param time A [`time`] object controlling duration and easing.
#'
#' @return An [`action`] object.
#'
#' @examples
#' w <- window(bl = 0, tr = pos(10, 10))
#' rect_obj <- rect(width = 2, height = 2, trans = transform(pos(2, 2)))
#' w@child(rect_obj)
#'
#' # Move from (2, 2) to (8, 8) over 2 seconds
#' rect_obj@action(translate(to = pos(8, 8), time = time(2)))
#'
#' @seealso [`offset()`], [`action`], [`time`], [`rotate()`]
#'
#' @export
translate <- function(to, time) {
  time <- new_time(time)
  to <- new_pos(to)
  action(
    function(obj, time) {
      force(time)
      if (is.null(time$from)) {
        time$from <- obj_pos(obj)
      }
      from <- time$from
      offset <- to - from
      obj_translate(obj = obj, to = from + (offset * time@value))
    },
    time = time
  )
}


#' Scale action (endpoint specification)
#'
#' `scale2()` creates an action that scales a [`shape`] to a target
#' size over time. Unlike [`scale()`], which applies a continuous
#' delta, `scale2()` interpolates from the current size to an absolute
#' target size.
#'
#' @param size Numeric; the amount to add to the current size to reach
#'   the target. (I.e., `target_size = current_size + size`.)
#' @param time A [`time`] object controlling duration and easing.
#'
#' @return An [`action`] object.
#'
#' @examples
#' w <- window(bl = 0, tr = pos(10, 10))
#' rect_obj <- rect(width = 1, height = 1, trans = transform(pos(5, 5)))
#' w@child(rect_obj)
#'
#' # Scale up by 1 unit over 1 second
#' rect_obj@action(scale2(size = 1, time = time(1)))
#'
#' @seealso [`scale()`], [`action`], [`time`], [`translate()`]
#'
#' @export
scale2 <- function(size, time) {
  time <- new_time(time)
  action(function(obj, time) {
    force(time)
    if (is.null(time$from)) {
      time$from <- obj_size(obj)
    }
    from <- time$from
    obj_scale(obj = obj, target_size = from + (size * time@value))
  }, time = time)
}


#' Spawn a grid of shapes
#'
#' `spawn_grid()` creates an action that generates a grid of child
#' shapes with optional spacing, padding, and individual actions.
#' Each cell is spawned with staggered timing for visual effect.
#'
#' @param nrow Number of rows in the grid.
#' @param ncol Number of columns in the grid.
#' @param padding Padding as a fraction of available space (default 0.05).
#'   Applied uniformly; see `xpadding` and `ypadding` for axis-specific control.
#' @param xpadding Padding in x direction as a fraction of available space.
#' @param ypadding Padding in y direction as a fraction of available space.
#' @param color Color of each grid cell.
#' @param apply An optional function `function(cell)` applied to each
#'   newly created cell before animation.
#' @param actions A list of [`action`] objects to attach to each cell.
#' @param spawn_time Time delay before spawning each cell (default 0).
#' @param xtime Time to stagger cells horizontally (default 0.1).
#' @param ytime Time to stagger cells vertically (default 0.1).
#' @param time A [`time`] object for the overall spawning action
#'   (default instant, `time(0)`).
#'
#' @return An [`action`] object.
#'
#' @details
#' The grid spawning uses a nested spawning pattern: horizontal stalks
#' are created, and within each stalk, cells are spawned vertically.
#' This creates a visual sweep effect when `xtime` and `ytime` are nonzero.
#'
#' @seealso [`spawn()`], [`spawn_from()`], [`rect()`]
#'
#' @export
spawn_grid <- function(
  nrow = 1, ncol = 1, padding = 0.05, color = "black", apply = NULL,
  actions = list(),
  spawn_time = 0, xtime = 0.1, ytime = 0.1, time = 0,
  xpadding = padding,
  ypadding = padding
) {
  nrow
  ncol
  padding
  xpadding
  ypadding
  color
  apply
  actions
  spawn_time
  xtime
  ytime
  time <- new_time(time)
  action(
    time = time,
    function(obj, time) {
      if (time@is_done) {
        extent <- get_extent(obj)
        y_size <- diff(extent$y)
        y_pad <- y_size * ypadding
        x_size <- diff(extent$x)
        x_pad <- x_size * xpadding
        width <- (x_size - x_pad) / ncol
        height <- (y_size - y_pad) / nrow
        x_pad_ <- x_pad / (ncol + 1L)
        y_pad_ <- y_pad / (nrow + 1L)

        x_move <- x_pad_ + width
        y_move <- -(y_pad_ + height)
        ele <- rect(
          color = color, width = width, height = height,
          actions = actions
        )
        if (!is.null(apply)) {
          apply(ele)
        }

        spawner <- shape(
          trans = transform(
            pos(
              extent$x[1] + (width / 2) + x_pad_,
              extent$y[2] - (height / 2) - y_pad_
            )
          )
        )@action(
          spawn_sibling(
            time = time(0),
            shape()@action(
              spawn_sibling(
                time = time(spawn_time),
                ele
              )@then(
                offset(by = pos(0, y_move), time = time(ytime))
              )@repeating(nrow - 1L)@finally(despawn())
            )
          )@then(
            offset(by = pos(x_move, 0), time = time(xtime)))@repeating(ncol - 1L)@finally(despawn())
        )
        obj@child(spawner)
      }
    }
  )
}


#' Extract opacity from a color
#'
#' `opaqueness()` extracts the alpha (opacity) value from a color
#' and returns it as a numeric in [0, 1].
#'
#' @param col A color specification.
#'
#' @return Numeric in [0, 1]; 0 is fully transparent, 1 is fully opaque.
#'
#' @seealso [`opaque()`], [`class_color`]
#'
#' @export
opaqueness <- function(col) col2rgb(col, alpha = TRUE)[4, , drop = TRUE] / 255L

#' Adjust color opacity
#'
#' `opaque()` modifies the alpha (opacity) value of a color while
#' keeping the RGB components unchanged.
#'
#' @param col A color specification (any format accepted by [`class_color`]).
#' @param level Numeric in [0, 1]; 0 is fully transparent, 1 is fully opaque.
#'
#' @return A color object with the specified opacity.
#'
#' @examples
#' # Semi-transparent red
#' opaque("red", level = 0.5)
#'
#' # Fully opaque blue
#' opaque("blue", level = 1)
#'
#' @seealso [`opaqueness()`], [`class_color`]
#'
#' @export
opaque <- function(col, level = 1) {
  if (level < 0 || level > 1) {
    stop("opaque level must be between 0 and 1", call. = FALSE)
  }
  rgb <- col2rgb(col, alpha = TRUE)
  rgb["alpha", ] <- (255 * level)
  class_color(rgb)
}
