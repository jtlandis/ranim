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
  constructor = function(ein = NULL,
                         eout = NULL,
                         etime = elinear,
                         mode = c("default", "weighted")) {
    ease_in <- ein
    ease_out <- eout
    ease_time <- etime
    if (use_ease_in <- !is.null(ease_in)) {
      check_valid_ease(ease_in, "ein")
      ease_in <- substitute(ease_in(t))
    }
    if (use_ease_out <- !is.null(ease_out)) {
      check_valid_ease(ease_out, "eout")
      ease_out <- substitute((1 - ease_out(1 - t)))
    }
    check_valid_ease(ease_time, "etime")
    res <- use_ease_in + use_ease_out
    call_ease <- switch(res,
      `1` = {
        if (use_ease_in) {
          ease_in
        } else {
          ease_out
        }
      },
      `2` = {
        mode <- match.arg(mode, c("default", "weighted"))
        switch(mode,
          default = {
            ease_in <- ensub(ease_in, list(t = quote(t * 2)))
            ease_in <- substitute(ease_in / 2)
            ease_out <- ensub(ease_out, list(t = quote(((t - .5) * 2))))
            ease_out <- substitute(0.5 + (ease_out / 2))
            substitute({
              if (t < .5) {
                ease_in
              } else {
                ease_out
              }
            })
          },
          weighted = {
            substitute(((1 - t) * ease_in) + (t * ease_out))
          },
        )
      }
    )

    if (use_ease_time <- !identical(ease_time, identity)) {
      ease_time <- substitute(t <- ease_time(t))
    }
    do <- sprintf(
      "do%s%s",
      if (use_ease_time) "time" else "",
      if (!is.null(call_ease)) "value" else ""
    )
    func <- switch(do,
      do = function(t) t,
      dotime = substitute(function(t) {
        ease_time
        t
      }) |> eval(),
      dovalue = substitute(function(t) {
        call_ease
      }) |> eval(),
      dotimevalue = substitute(function(t) {
        ease_time
        call_ease
      }) |> eval()
    )
    # func <- if (!is.null(call_ease)) {
    # } else {
    #   function(t) {
    #     t
    #   }
    # }
    # if (!is.null(call_ease)) {

    # }
    # body(func) <- call_ease
    new_object(func)
  }
)


method(print, ease) <- function(x, ...) {
  cat("<ease> function (t) ")
  # print(
  #   do.call(
  #     substitute,
  #     list(body(x), environment(x))
  #   )
  # )
  print(body(x))
  invisible(x)
}

method(plot, ease) <- function(x, ...) {
  t <- seq(0, 1, length.out = 100)
  plot(t, Vectorize(x)(t), type = "l", xlab = "t", ylab = "ease(t)", main = "Ease Function")
}
