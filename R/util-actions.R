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

spawn <- into_action(
  spawn_from,
  expr_map(
    child = obj_clone(child),
    offset = offset
  )
)

spawn_sibling <- function(sib, time, offset = NULL) {
  force(time)
  if (is.null(offset)) {
    offset <- pos()
  }
  action(function(obj, time) {
    force(time)
    parent <- obj@parent
    spawn_from(parent, obj_clone(sib), obj_pos(obj, local = TRUE) + offset)
  }, time = time)
}

duplicate <- function(time) {
  force(time)
  action(function(obj, time) {
    force(time)
    parent <- obj@parent
    spawn_from(parent, obj_clone(obj))
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

translate <- into_action(
  obj_translate,
  expr_map(
    to = (to - obj_pos(obj)) * time@value
  )
)
