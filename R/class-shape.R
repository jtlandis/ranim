#' @include class-scalars.R
#' @include class-pos.R
#' @include class-transform.R
#' @include util-ease.R
#' @include class-time.R
#' @include util-action.R
NULL

#' @title Shape objects
#'
#' @description
#' A `shape` is the fundamental unit of the animation system. Shapes
#' can contain other shapes (children), have a position and size
#' (through a [transform] object), and carry a list of time-varying
#' [action] objects that drive animation.
#'
#' Shapes form a tree: each shape has at most one parent, and
#' zero or more children. Transformations are anchored at the
#' parent's global position, so moving or scaling a parent
#' implicitly affects all of its descendants.
#'
#' @param trans  a [transform] giving the local/global position,
#'   size, and anchor.
#' @param parent another `shape` or `NULL`.
#' @param children a list of child shapes.
#' @param actions a list of [action] objects to apply to this shape.
#' @param color a [class_color] object used by rendering methods.
#'
#' @section Properties:
#' * `trans`: a [transform] giving the local/global position,
#'   size, and anchor.
#' * `parent`: another `shape` or `NULL`.
#' * `children`: a list of child shapes.
#' * `global`: computed position in world coordinates.
#' * `actions`: a list of [action] objects to apply to this shape.
#' * `action`: function to attach new actions to this shape.
#' * `act`: function to advance animation state.
#' * `color`: a [class_color] object used by rendering methods.
#' * `size`: numeric size (derived from transform).
#'
#' @section Constructor:
#' `shape(trans = transform(), parent = NULL, children = list(),
#' actions = list(), color = "black")`
#'
#' Most users will not construct bare `shape` objects directly;
#' instead they work with concrete shapes like [rect()], [point()],
#' [polygon()], [text()], etc., which all extend `shape`.
#'
#' @export
shape <- S7::new_class("shape",
  parent = class_env,
  properties = list(
    trans = transform,
    parent = S7::new_property(
      class = class_any,
      default = NULL,
      setter = function(self, value) {
        if (is.null(value)) {
          # if we had a parent
          # remove self from the parent's children
          if (!is.null(self@parent)) {
            parent <- self@parent
            self@parent <- NULL
            for (i in seq_along(parent@children)) {
              if (identical(parent@children[[i]], self)) {
                attr(parent, "children") <- parent@children[-i]
                break
              }
            }
          }
        } else {
          if (!S7::S7_inherits(value, shape)) {
            stop("parent must be a shape object or NULL", call. = FALSE)
          }
          # set the parent
          self@parent <- value
          self@trans@anchor <- value@global
          # add self to the parent's children
          attr(value, "children") <- c(value@children, list(self))
        }
        invisible(self)
      },
      validator = function(value) {
        if (!is.null(value) && !S7_inherits(value, shape)) {
          return("parent must be a shape object or NULL")
        }
        NULL
      }
    ),
    global = new_property(
      class = class_pos,
      getter = function(self) {
        self@trans@global
      }
    ),
    children = new_property(
      class = class_list,
      validator = function(value) {
        if (!is.list(value) ||
          !all(vapply(value, function(x) S7_inherits(x, shape), logical(1)))) {
          return("children must be a list of shape objects")
        }
        NULL
      }
    ),
    child = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(child, offset = NULL) {
          if (!S7_inherits(child, shape)) {
            stop("`child` must be a shape object", call. = FALSE)
          }
          if (!is.null(offset)) {
            if (!is_scalar_pos(offset)) {
              stop("`offset` should be NULL or a pos")
            }
            child@parent <- self
            child@trans@offset <- offset
            ## anchor positions likely need to be updated
            update_trans(child)
          } else {
            # reset transform position to be
            # anchored with the parent
            global_pos <- child@global
            child@parent <- self
            child@trans@global <- global_pos
          }
          # if (!missing(apply_child)) {
          #   if (!is.function(apply_child)) {
          #     stop("apply_child must be a function", call. = FALSE)
          #   }
          #   child <- apply_child(child)
          # }
          self
        }
      }
    ),
    actions = new_property(
      class = class_list,
      default = list(),
      validator = function(value) {
        if (!is.list(value) ||
          !all(vapply(value, \(x) S7_inherits(x, action), logical(1)))) {
          return("actions must be a list of action objects")
        }
        NULL
      }
    ),
    action = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(func, time) {
          if (!S7_inherits(func, action)) {
            if (missing(time) ||
              (!S7_inherits(time, class_time) && !is.function(func))) {
              stop("`func` must be an action object or a function",
                call. = FALSE
              )
            }
            func <- action(func, time = time)
          }
          attr(self, "actions") <- c(self@actions, list(func))
          invisible(self)
        }
      }
    ),
    act = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(delta_time) {
          has_acted <- FALSE
          while (delta_time > 0 || !has_acted) {
            actions <- self@actions

            to_rm <- logical(length(actions))
            min_time <- min(self@left, delta_time)
            # calling actions could spawn children
            # cache here so that when min_time is
            # pushed only on current children
            children <- self@children
            for (i in seq_along(actions)) {
              action <- actions[[i]]
              action(self, min_time)
              if (action@is_done) {
                to_rm[i] <- TRUE
              }
            }

            if (any(to_rm)) {
              attr(self, "actions") <- actions[!to_rm]
            }
            child_acting <- FALSE
            for (i in seq_along(children)) {
              child <- children[[i]]

              child_acting <- child@act(min_time) || child_acting
            }
            delta_time <- delta_time - min_time
            has_acted <- TRUE
          }

          invisible(length(self@actions) > 0 ||
            child_acting ||
            (length(self@children) &&
              any(vapply(self@children, \(ch) length(ch@actions) > 0, FALSE))))
        }
      }
    ),
    color = class_color,
    size = new_property(
      class = class_numeric,
      getter = function(self) {
        self@trans@size
      }
    ),
    left = new_property(
      class = class_numeric,
      getter = function(self) {
        min(
          vapply(self@actions, prop, 1, name = "left"),
          vapply(self@children, prop, 1, name = "left"),
          Inf
        )
      }
    )
  ),
  constructor = function(trans = transform(),
                         parent = NULL,
                         children = list(),
                         actions = list(),
                         color = "black") {
    new_object(env(),
      trans = trans, parent = parent, children = children,
      color = class_color(color), # advance = advance,
      actions = actions
    )
  }
)


method(format, shape) <- function(x, indent = "", ...) {
  nkids <- length(x@children)
  this <- sprintf(
    "A shape: <%s> %s\n%s - %s\n%s %s",
    attr(S7_class(x), "name"),
    if (!is.null(x@parent)) "" else "no parent",
    indent, format(x@trans),
    indent, if (nkids == 0) {
      "- no children"
    } else {
      sprintf("- %i children:", nkids)
    }
  )
  if (nkids == 0) {
    return(this)
  }
  sprintf(
    "%s\n%s", this,
    paste(indent,
      "* ",
      vapply(
        x@children,
        function(kid) format(kid, paste0(indent, "    ")), character(1)
      ),
      collapse = "\n"
    )
  )
}

method(print, shape) <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

#' @export
method(obj_size, shape) <- function(obj) {
  obj@trans@size
}

#' @export
method(obj_anchor, shape) <- function(obj) {
  obj@trans@anchor
}

should_browse <- function(obj) {
  if (length(obj@children) == 0) {
    return(FALSE)
  }
  child <- obj@children[[1L]]
  if (length(actions <- child@actions) == 0) {
    return(FALSE)
  }
  actions <- actions[[1]]
  if (!S7_inherits(actions, act_series)) {
    return(FALSE)
  }
  actions <- actions@actions[[1]]
  if (!S7_inherits(actions, act_series)) {
    return(FALSE)
  }
  actions <- actions@actions
  if (length(actions) != 3L) {
    return(FALSE)
  }
  act <- actions[[3]]
  if (!S7_inherits(act, action)) {
    return(FALSE)
  }
  act@time@cycled
}

update_trans <- function(self) {
  children <- self@children
  if (length(children) == 0) {
    return(self)
  }
  global <- self@trans@global
  for (child in children) {
    child@trans@anchor <- global
    update_trans(child)
  }
  self
}

#' @export
method(obj_pos, shape) <- function(obj, local = FALSE) {
  if (local) {
    obj@trans@offset
  } else {
    obj@global
  }
}


#' @export
method(remaining_time, action) <- function(object, ...) {
  remaining_time(object@time, ...)
}

#' @export
method(remaining_time, shape) <- function(object, ...) {
  this_remaining <- if (length(object@actions)) {
    vapply(object@actions,
      FUN = method(remaining_time, action), FUN.VALUE = numeric(1), ...
    ) |>
      max()
  } else {
    0
  }
  children_remaining <- if (length(object@children)) {
    vapply(object@children, FUN = method(remaining_time, shape), FUN.VALUE = numeric(1), ...) |>
      max()
  } else {
    0
  }
  max(this_remaining, children_remaining)
}
