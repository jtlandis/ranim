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
    value = scalar_num,
    delta = scalar_num,
    step = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function() {
          old_value <- self@value
          if (self@time >= 1) {
            if (self@repeating > 0) {
              self@time <- 0
              self@value <- self@ease(self@time)
              self@repeating <- self@repeating - 1
            } else {
              self@delta <- scalar(0)
              return(invisible(self))
            }
          }
          self@time <- self@time + self@delta_time
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
    )
  ),
  constructor = function(steps = 1000,
                         ease = identity) {
    time <- scalar(0)
    new_object(env(),
      time = time,
      delta_time = scalar(1 / steps),
      value = ease(time),
      delta = scalar(0),
      ease = ease
    )
  }
)

ease_in_out_sine <- function(t) {
  -0.5 * (cos(pi * t) - 1)
}
start <- 0
end <- 100
t <- seq(0, 1, length.out = 100)
values <- start + (end - start) * ease_in_out_sine(t)
plot(t, values, type = "l")
