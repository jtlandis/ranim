#' @include clone.R

#' @title Tracker objects
#' @name tracker
#'
#' @description
#' A `tracker` manages references to specific shapes within a hierarchy,
#' preserving those references when the hierarchy is cloned. This is
#' essential for working with complex shape trees where you need to
#' maintain access to particular child shapes across multiple animation
#' iterations.
#'
#' @section Properties:
#' * `object`: The root [`shape`] being tracked.
#' * `tracked`: A named list of [`shape`] objects to keep track of.
#' * `find(name)`: Function to locate a tracked shape by name.
#' * `track(...)`: Function to register new shapes to track.
#' * `clone()`: Function to create a deep copy of the tracker with all
#'   references updated.
#' * `restore()`: Function to clone and restore tracked shapes to the
#'   calling environment as variables.
#'
#' @section Constructor:
#' `tracker(object, tracked = list())`
#'
#' @param object A [`shape`] object (typically a [window()]) to track
#'   children within.
#' @param tracked Optional named list of shapes to track initially.
#'
#' @return An object of class `tracker`.
#'
#' @details
#' A `tracker` is useful when you have a complex shape hierarchy and want
#' to save checkpoints of the animation state. By tracking specific shapes
#' (e.g., the main grid, rows, columns), you can restore them later via
#' `$clone()` and `$restore()` without losing references.
#'
#' Example workflow:
#' 1. Create a window and some shapes.
#' 2. Use `tracker$track(shape1 = my_shape, shape2 = another)` to register them.
#' 3. Add animations.
#' 4. Save state with `checkpoint <- tracker$clone()`.
#' 5. Continue animating.
#' 6. Restore with `checkpoint$restore()` to get back the original shapes.
#'
#' @examples
#' w <- window(bl = 0, tr = pos(10, 10))
#' rect_obj <- rect(width = 2, height = 2, trans = transform(pos(5, 5)))
#' w@child(rect_obj)
#'
#' # Create a tracker
#' trk <- tracker(w)
#' trk@track(my_rect = rect_obj)
#'
#' # Add animations
#' rect_obj@action(translate(to = pos(8, 8), time = time(2)))
#'
#' # Save state
#' checkpoint <- trk@clone()
#'
#' # Continue animating
#' rect_obj@action(color("red", "blue", time = time(1)))
#'
#' # Restore to checkpoint
#' restored_trk <- checkpoint@restore()
#'
#' @seealso [obj_clone()], [shape], [window()]
#'
#' @export
tracker <- new_class(
  "tracker",
  parent = class_env,
  properties = list(
    object = shape,
    tracked = new_property(
      class = class_list,
      default = list(),
      validator = function(value) {
        if (length(value) == 0L) {
          return(NULL)
        }
        names <- names(value)
        if (is.null(names) || anyDuplicated(names) || any(names == "")) {
          return("should be a uniquely named list")
        }
        if (!all(vapply(value, S7_inherits, FALSE, class = shape))) {
          return("all elements should be a shape object")
        }
        NULL
      }
    ),
    find = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(name) {
          if (!is.character(name) || length(name) != 1L) {
            stop("name should be a single string", call. = FALSE)
          }
          target <- self@tracked[[name]]
          if (is.null(target)) {
            stop(
              sprintf("no object tracked with name '%s'", name),
              call. = FALSE
            )
          }
          find_child(self@object, target = rlang::env_label(target))
        }
      }
    ),
    track = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(...) {
          dots <- list(...)
          if (length(dots) == 0L) {
            stop("at least one object must be provided", call. = FALSE)
          }
          nms <- names(dots)
          if (is.null(nms) || anyDuplicated(nms) || any(nms == "")) {
            stop("all objects must be named with unique names", call. = FALSE)
          }
          if (!all(vapply(dots, S7_inherits, FALSE, class = shape))) {
            stop("all objects must be shape objects", call. = FALSE)
          }
          lgl <- logical(length(dots))
          for (i in seq_along(dots)) {
            path <- find_child(self@object,
              target = rlang::env_label(dots[[i]])
            )
            if (!is.null(path)) {
              lgl[i] <- TRUE
            } else {
              warning(
                sprintf("item '%s' is not a child of root object", nms[i])
              )
            }
          }
          attr(self, "tracked")[names(dots)[lgl]] <- dots[lgl]
          invisible(self)
        }
      }
    ),
    clone = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function() {
          obj_clone(self)
        }
      }
    ),
    restore = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function() {
          cloned <- self@clone()
          env <- parent.frame()
          nms <- names(cloned@tracked)
          for (nm in nms) {
            assign(nm, cloned@tracked[[nm]], envir = env)
          }
          cloned
        }
      }
    )
  ),
  constructor = function(object, tracked = list()) {
    new_object(env(), object = object, tracked = tracked)
  }
)

#' @export
method(print, tracker) <- function(x, ...) {
  cat("tracker object\n")
  cat("root object -> ", attr(S7_class(x@object), "name"), "\n", sep = "")
  if (length(x@tracked) > 0L) {
    cat("tracked objects:\n")
    for (i in seq_along(x@tracked)) {
      name <- names(x@tracked)[i]
      obj <- x@tracked[[i]]
      cat(sprintf("- %s -> %s\n", name, attr(S7_class(obj), "name")))
    }
  } else {
    cat("no tracked objects\n")
  }
  invisible(x)
}

#' @export
`$.tracker` <- function(obj, name) {
  if (name %in% names(obj@tracked)) {
    obj@tracked[[name]]
  } else {
    stop(sprintf("no object tracked with name '%s'", name), call. = FALSE)
  }
}

`.DollarNames.tracker` <- function(x, pattern = "") {
  grep(pattern = pattern, x = names(x@tracked), value = TRUE)
}

find_child <- function(node, target, depth = 0L) {
  depth <- depth + 1L
  children <- node@children
  nchildren <- length(children)
  # node_label <- rlang::env_label(node)
  labels <- vapply(children, rlang::env_label, FUN.VALUE = "")
  index <- match(target, labels, nomatch = 0L)
  if (index) {
    path <- integer(depth)
    path[depth] <- index
    return(path)
  }
  for (i in seq_len(nchildren)) {
    child <- children[[i]]
    res <- find_child(child, target, depth = depth)
    if (!is.null(res)) {
      res[depth] <- i
      return(res)
    }
  }
  NULL
}

method(obj_clone, tracker) <- function(obj) {
  nms <- names(obj@tracked)
  names(nms) <- nms
  paths <- lapply(
    nms,
    obj@find
  )
  paths <- paths[!vapply(paths, is.null, logical(1))]
  new_root <- obj_clone(obj@object)
  for (i in seq_along(paths)) {
    path <- paths[[i]]
    node <- new_root
    for (j in path) {
      node <- node@children[[j]]
    }
    paths[i] <- list(node)
  }
  tracker(object = new_root, tracked = paths)
}
