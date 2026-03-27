#' Environment objects
#'
#' `env()` creates an environment object with an isolated parent
#' (emptyenv). This is used internally throughout **ranim** to manage
#' object state in a controlled way.
#'
#' @param .data Optional list or environment to initialize the new
#'   environment with. If missing, an empty environment is created.
#'
#' @return An object of class `env` (which is an environment).
#'
#' @details
#' The returned environment has `emptyenv()` as its parent, preventing
#' accidental access to the global environment or other parent frames.
#' This is primarily an internal utility; most users will not directly
#' call `env()`.
#'
#' @keywords internal
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

#' @keywords internal
class_env <- S7::new_S3_class("env", constructor = env)
