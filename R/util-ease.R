elinear <- identity

ecos <- function(t) {
  1 - cos(0.5 * t * pi)
}

esin <- function(t) {
  sin(0.5 * t * pi)
}

equad <- function(t) {
  t^2
}

ecubic <- function(t) {
  t^3
}



is_valid_ease <- function(f) {
  is.function(f) && length(formals(f)) == 1L
}

check_valid_ease <- function(f, name) {
  if (!is_valid_ease(f)) {
    stop(sprintf(
      "`%s` must be a function with exactly one argument",
      name
    ), call. = FALSE)
  }
  if (!all.equal(f(c(0, 1)), c(0, 1), tolerance = 1e-6)) {
    warning(sprintf(
      "`%s` does not map 0 to 0 and 1 to 1; this may lead to unexpected behavior",
      name
    ), call. = FALSE, immediate. = TRUE)
  }
  invisible(TRUE)
}

ease <- new_class(
  "ease",
  parent = class_function,
  constructor = function(ease_in = ease_linear,
                         ease_out = ease_in,
                         ease_time = ease_linear) {
    check_valid_ease(ease_in, "ease_in")
    check_valid_ease(ease_out, "ease_out")
    check_valid_ease(ease_time, "ease_time")
    call_ease <- if (identical(ease_in, ease_out)) {
      # Symmetric ease
      ease_fn <- ease_in
      quote(ease_fn(t))
    } else {
      # Asymmetric ease
      call_ease_in <- quote(ease_in(t))
      call_ease_out <- quote(ease_out(t))
      substitute(((1 - t) * call_ease_in) + (t * call_ease_out))
    }
    if (!identical(ease_time, identity)) {
      call_ease <- substitute({
        t <- ease_time(t)
        call_ease
      })
    }
    func <- function(t) {
      t
    }
    body(func) <- call_ease
    new_object(func)
  }
)


method(print, ease) <- function(x, ...) {
  cat("<ease> function (t) ")
  print(
    do.call(
      substitute,
      list(body(x), environment(x))
    )
  )
  invisible(x)
}

method(plot, ease) <- function(x, ...) {
  t <- seq(0, 1, length.out = 100)
  plot(t, x(t), type = "l", xlab = "t", ylab = "ease(t)", main = "Ease Function")
}
