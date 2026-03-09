one_tol <- 1 - 1e-12

time <- new_class(
  "time",
  parent = class_env,
  properties = list(
    time = scalar_prop(time),
    delta_time = new_property(
      class = scalar_num,
      getter = function(self) {
        self@delta_time
      }
    ),
    iter = scalar_prop(iter),
    last_time = new_union(NULL, new_S3_class("POSIXct")),
    start_time = new_union(NULL, new_S3_class("POSIXct")),
    time_scale = scalar_prop(time_scale),
    value = scalar_prop(value),
    delta = scalar_prop(delta),
    step = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(delta_time) {
          if (self@is_done) {
            self@delta <- scalar(0)
            attr(self, "delta_time") <- scalar(0)
            return(invisible(self))
          }
          old_value <- self@value
          instant <- self@is_instant
          delta_left <- self@left
          if (!instant) {
            self@time <- self@time + (self@time_scale * delta_time)
          } else {
            self@time <- scalar(Inf)
          }

          # switch(self@mode,
          #   time = {
          #     this_time <- Sys.time()
          #     last_time <- self@last_time %||% self@start_time %||% this_time
          #     delta_time <- this_time |>
          #       difftime(last_time, units = "secs") |>
          #       as.numeric()
          #     self@last_time <- this_time
          #     attr(self, "delta_time") <- scalar(delta_time)

          #   },
          #   fps = {
          #     self@time <- self@time + self@delta_time
          #   }
          # )

          ## WE SHOULDNT ALLOW WRAPPING.
          # if self@time >= 1 consider exiting with
          # remainder time left? then restepping?
          if (self@time >= one_tol) {
            if (!instant) {
              delta_time <- delta_left
            }
            self@time <- scalar(1)
            # if (self@repeating > self@iter) {
            #   # old_value becomes the new delta
            #   old_value <- old_value - self@ease(scalar(1))
            #   # set time to the wrapped point
            #   self@time <- self@time_scale * new_delta_time
            # } else {
            #   self@time <- scalar(1)
            # }
            self@cycled <- TRUE
            self@iter <- self@iter + 1L
          }
          attr(self, "delta_time") <- scalar(delta_time)
          self@value <- self@ease(self@time)
          self@delta <- self@value - old_value
          invisible(self)
        }
      }
    ),
    ease = new_property(
      class = class_function,
      setter = function(self, value) {
        if (!is.function(value)) {
          stop("ease must be a function", call. = FALSE)
        }
        if (length(formals(value)) != 1L) {
          stop("ease function must have exactly one argument",
            call. = FALSE
          )
        }
        attr(self, "ease") <- value
        invisible(self)
      }
    ),
    repeating = scalar_prop(repeating),
    is_done = new_property(
      class = class_logical,
      getter = function(self) {
        self@repeating < self@iter
      }
    ),
    reset = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function() {
          self@time <- scalar(0)
          self@last_time <- NULL
          self@iter <- 0L
          self@value <- self@ease(self@time)
          self@delta <- 0
          self@cycled <- FALSE
          invisible(self)
        }
      }
    ),
    mode = new_property(
      class = class_character,
      setter = function(self, value) {
        value <- match.arg(value, c("time", "fps"))
        attr(self, "mode") <- value
        invisible(self)
      }
    ),
    duration = new_property(
      class = scalar_num,
      getter = function(self) {
        1 / self@time_scale
      }
    ),
    left = new_property(
      class = scalar_num,
      getter = function(self) {
        (1 - self@time) * self@duration
      }
    ),
    remaining = new_property(
      class = scalar_num,
      getter = function(self) {
        curr <- self@left
        if (is.finite(reps <- self@repeating)) {
          remaining <- reps - self@iter
          if (remaining > 0) {
            curr <- curr + (self@duration * remaining)
          }
        } else {
          # cannot compute remaining time on infinite reps
          return(scalar(Inf))
        }
        curr
      }
    ),
    cycled = class_logical,
    is_instant = new_property(
      class = class_logical,
      getter = function(self) {
        is.infinite(self@time_scale)
      }
    )
  ),
  constructor = function(duration = 1,
                         ease = identity,
                         repeating = 0,
                         mode = "time") {
    time <- scalar(0)
    time_scale <- scalar(1 / duration)
    new_object(env(),
      time = time,
      time_scale = time_scale,
      last_time = NULL,
      delta_time = time_scale / 50, # 50 FPS
      value = ease(time),
      delta = 0,
      ease = ease,
      repeating = repeating,
      mode = mode,
      iter = 0L,
      cycled = FALSE
    )
  }
)

class_time <- time

method(print, class_time) <- function(x, ...) {
  cat(sprintf(
    "<time> %s\n",
    format(x)
  ))
  invisible(x)
}

method(format, class_time) <- function(x, ...) {
  sprintf(
    "remaining: %.02f | left: %.02f | time: %.02f%% | value: %.02f%% | iter: %s/%s",
    x@remaining,
    x@left,
    100 * x@time,
    100 * x@value,
    format(x@iter),
    format(x@repeating + 1L)
  )
}

remaining_time <- new_generic(
  "remaining_time", "object",
  function(object, ...) {
    S7::S7_dispatch()
  }
)

method(remaining_time, time) <- function(object, ..., ignore_inf = TRUE) {
  remain <- object@remaining
  if (ignore_inf && is.infinite(remain)) {
    return(scalar(0))
  }
  remain
}

# ease_in_out_sine <- function(t) {
#   -0.5 * (cos(pi * t) - 1)
# }
# start <- 0
# end <- 100
# t <- seq(0, 1, length.out = 100)
# values <- start + (end - start) * ease_in_out_sine(t)
# plot(t, values, type = "l")
