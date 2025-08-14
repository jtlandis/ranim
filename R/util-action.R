action <- new_class(
  "action",
  parent = class_function,
  properties = list(
    time = time
  ),
  constructor = function(
      func,
      time) {
    
    if (!is.function(func)) {
      stop("func must be a function", call. = FALSE)
    }
    if (identical(names(formals(func)), c("obj", "time"))) {
      stop("func must have exactly two arguments: (obj, time)", call. = FALSE)
    }
    new_object(
      function(obj) {
        func(obj, time@step())
      },
      time = time
    )
  }
)
