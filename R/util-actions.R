rotate <- into_action(obj_rotate, expr_map(degrees = degrees * time@delta))

scale <- into_action(obj_scale, expr_map(size = size * time@delta))

color <- into_action(
  obj_color_between,
  expr_map(amount = time@value, color_from = color_from, color_to = color_to)
)

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

spawn_from <- function(parent, child, offset = NULL) {
  parent@child(
    child = child,
    offset = offset
  )
}

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


obj_offset <- function(obj, by) {
  obj_translate(obj, to = obj_pos(obj) + by)
}

offset <- into_action(
  obj_offset,
  expr_map(
    by = by * time@delta
  )
)

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


opaqueness <- function(col) col2rgb(col, alpha = TRUE)[4, , drop = TRUE] / 255L

opaque <- function(col, level = 1) {
  if (level < 0 || level > 1) {
    stop("opaque level must be between 0 and 1", call. = FALSE)
  }
  rgb <- col2rgb(col, alpha = TRUE)
  rgb["alpha", ] <- (255 * level)
  class_color(rgb)
}
