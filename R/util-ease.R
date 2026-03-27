#' Ease functions
#'
#' Pre-defined easing functions for animation timing. These can be used
#' directly or combined with [`ease()`] to create custom easing curves.
#'
#' @details
#' Standard easing functions:
#' * `elinear`: Linear easing (identity function)
#' * `ecos`: Cosine-based ease-in curve
#' * `esin`: Sine-based ease-in curve
#' * `equad`: Quadratic ease-in curve (t^2)
#' * `ecubic`: Cubic ease-in curve (t^3)
#'
#' All easing functions take a numeric value `t` in [0, 1] and return
#' a value in [0, 1], mapping start to end of animation.
#'
#' @param t A numeric value in [0, 1] representing normalized time.
#'
#' @return Numeric value in [0, 1].
#'
#' @examples
#' # Linear easing
#' elinear(0.5)
#'
#' # Cubic easing
#' ecubic(0.5)
#'
#' # Plot easing curve
#' t <- seq(0, 1, length.out = 100)
#' plot(t, Vectorize(ecubic)(t), type = "l")
#'
#' @export
elinear <- identity

#' @rdname elinear
#' @export
ecos <- function(t) {
  1 - cos(0.5 * t * pi)
}

#' @rdname elinear
#' @export
esin <- function(t) {
  sin(0.5 * t * pi)
}

#' @rdname elinear
#' @export
equad <- function(t) {
  t^2
}

#' @rdname elinear
#' @export
ecubic <- function(t) {
  t^3
}


#' Check if a function is a valid ease function
#'
#' @param f A function to validate.
#'
#' @return Logical scalar.
#'
#' @keywords internal
is_valid_ease <- function(f) {
  is.function(f) && length(formals(f)) == 1L
}

#' Validate and check easing functions
#'
#' @param f A function to validate.
#' @param name Name of the function for error messages.
#'
#' @return `TRUE` invisibly if valid; stops with error otherwise.
#'
#' @keywords internal
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

#' Composite ease functions
#'
#' `ease()` creates custom easing functions by combining ease-in,
#' ease-out, and ease-time transformations. This allows building
#' sophisticated timing curves for animations.
#'
#' @param ein An optional ease-in function (applied to the first half
#'   of the interval in default mode).
#' @param eout An optional ease-out function (applied to the second half
#'   of the interval in default mode).
#' @param etime A base time transformation applied to the entire interval.
#'   Defaults to [`elinear`].
#' @param mode How to combine `ein` and `eout`:
#'   - `"default"`: Apply `ein` to [0, 0.5) and `eout` to [0.5, 1]
#'   - `"weighted"`: Blend `ein` and `eout` based on position in [0, 1]
#'
#' @return An object of class `ease` that can be used with [`time()`]
#'   or passed to action constructors.
#'
#' @details
#' Easing functions must satisfy:
#' - Exactly one argument (the normalized time `t` in [0, 1])
#' - Map 0 to 0 and 1 to 1
#'
#' @examples
#' # Linear easing (same as elinear)
#' e_linear <- ease()
#'
#' # Ease-in with cubic
#' e_in <- ease(ein = ecubic)
#'
#' # Ease-in-out cubic (default mode)
#' e_inout <- ease(ein = ecubic, eout = ecubic)
#'
#' @export
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


#' @export
method(print, ease) <- function(x, ...) {
  cat("<ease> function (t) ")
  print(body(x))
  invisible(x)
}

#' @export
method(plot, ease) <- function(x, ...) {
  t <- seq(0, 1, length.out = 100)
  plot(t, Vectorize(x)(t), type = "l", xlab = "t", ylab = "ease(t)", main = "Ease Function")
}
