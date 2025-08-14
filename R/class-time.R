time <- new_class(
  "time",
  parent = class_env,
  properties = list(
    time = scalar_num,
    delta_time = new_property(
      class = scalar_num,
      getter = function(self) {
        self@delta_time
      }
    ),
    last_time = new_union(NULL, new_S3_class("POSIXct")),
    time_scale = scalar_num,
    value = scalar_num,
    delta = scalar_num,
    step = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function() {
          if (self@repeating < 0) {
            self@delta <- scalar(0)
            return(invisible(self))
          }
          old_value <- self@value
          switch(self@mode,
            time = {
              this_time <- Sys.time()
              last_time <- self@last_time %||% this_time
              delta_time <- this_time |>
                difftime(last_time, units = "secs") |>
                as.numeric()
              self@last_time <- this_time
              attr(self, "delta_time") <- scalar(delta_time)
              self@time <- self@time + (self@time_scale * delta_time)
            },
            increment = {
              self@time <- self@time + self@delta_time
            }
          )
          if (self@time >= 1) {
            if (self@repeating >= 0) {
              self@time <- scalar(0)
              self@value <- self@ease(self@time)
              self@repeating <- self@repeating - 1
            } else {
              self@time <- scalar(1)
            }
          }
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
    repeating = new_property(
      class = scalar_num,
      default = scalar(0),
    ),
    mode = new_property(
      class = class_character,
      setter = function(self, value) {
        value <- match.arg(value, c("time", "increment"))
        attr(self, "mode") <- value
        invisible(self)
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
      delta = scalar(0),
      ease = ease,
      repeating = scalar(repeating),
      mode = mode
    )
  }
)

class_time <- time

ease_in_out_sine <- function(t) {
  -0.5 * (cos(pi * t) - 1)
}
start <- 0
end <- 100
t <- seq(0, 1, length.out = 100)
values <- start + (end - start) * ease_in_out_sine(t)
plot(t, values, type = "l")
