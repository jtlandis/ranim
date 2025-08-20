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
