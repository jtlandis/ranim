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

shape <- new_class("shape",
  parent = class_env,
  properties = list(
    trans = transform,
    parent = new_property(
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
                parent@children <- parent@children[-i]
                break
              }
            }
          }
        } else {
          if (!S7_inherits(value, shape)) {
            stop("parent must be a shape object or NULL", call. = FALSE)
          }
          # set the parent
          self@parent <- value
          # add self to the parent's children
          value@children <- c(value@children, list(self))
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
      class = pos,
      getter = function(self) {
        self@trans@global
      }
    ),
    children = new_property(
      class = class_list,
      validator = function(value) {
        if (!is.list(value) || !all(vapply(value, function(x) S7_inherits(x, shape), logical(1)))) {
          return("children must be a list of shape objects")
        }
        NULL
      }
    ),
    child = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(shape, ..., offset = pos(), apply_child) {
          if (!inherits(shape, "S7_class")) {
            stop("shape must be a shape object", call. = FALSE)
          }
          child <- shape(
            trans = transform(anchor = self@global, offset = offset),
            ...,
            parent = self
          )
          if (!missing(apply_child)) {
            if (!is.function(apply_child)) {
              stop("apply_child must be a function", call. = FALSE)
            }
            child <- apply_child(child)
          }
          self
        }
      }
    ),
    color = class_color,
    size = new_property(
      class = scalar_num,
      getter = function(self) {
        self@trans@size
      }
    ),
    advance = new_property(
      class = new_union(NULL, class_function),
      default = NULL,
      getter = function(self) {
        force(self)
        function(time, delta) {
          fn <- attr(self, "advance")
          if (is.function(fn)) {
            fn(self, time, delta)
          }
          for (child in self@children) {
            child@advance(time, delta)
          }
        }
      },
      setter = function(self, value) {
        if (!is.null(value) && !is.function(value)) {
          stop("advance must be a function or NULL", call. = FALSE)
        }
        if (!is.null(value) && !identical(names(formals(value)), c("self", "time", "delta"))) {
          stop("advance function must have 'self', 'time', and 'delta' as arguments", call. = FALSE)
        }
        attr(self, "advance") <- value
        invisible(self)
      }
    )
  ),
  constructor = function(trans = transform(),
                         parent = NULL,
                         children = list(),
                         color = "black",
                         advance = NULL) {
    new_object(env(),
      trans = trans, parent = parent, children = children,
      color = class_color(color), advance = advance
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
      vapply(x@children, function(kid) format(kid, paste0(indent, "    ")), character(1)),
      collapse = "\n"
    )
  )
}

method(print, shape) <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

method(obj_size, shape) <- function(obj) {
  obj@trans@size
}

method(obj_anchor, shape) <- function(obj) {
  obj@trans@anchor
}
