env <- function(.data) {
  out <- if (missing(.data)) {
    new.env(parent = emptyenv())
  } else {
    if (!is.list(.data)) {
      .data <- as.list(.data)
    }
    list2env(.data, parent = emptyenv())
  }
  class(out) <- "env"
  out
}

class_env <- new_S3_class("env", constructor = env)
