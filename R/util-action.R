`!bang` <- quote(`!`)


is_splice <- function(x) {
  if (is.call(x) && identical(x[[1]], `!bang`)) {
    xsub <- x[[2]]
    if (is.call(xsub) && identical(xsub[[1]], `!bang`)) {
      xsub <- xsub[[2]]
      if (is.call(xsub) && identical(xsub[[1]], `!bang`)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

spliced <- function(x) {
  attr(x, "spliced") <- TRUE
  x
}

splice <- function(x) {
  is_spliced <- vapply(x, \(x) !is.null(attr(x, "spliced")), logical(1))
  if (!any(is_spliced)) {
    return(x)
  }
  n <- length(x)
  sizes <- rep(1L, n)
  sizes[is_spliced] <- vapply(x[is_spliced], length, integer(1))
  out <- vector("list", sum(sizes))
  k <- 1L
  for (i in seq_along(x)) {
    if (is_spliced[i]) {
      xx <- x[[i]]
      for (j in seq_along(xx)) {
        out[[k]] <- xx[[j]]
        k <- k + 1L
      }
      next
    }
    out[[k]] <- x[[i]]
    k <- k + 1L
  }
  out
}


#' replicates bquote except with rlang syntax
#' x <- quote(foo)
#' y <- as_expr2(mean(!!x))
#' print(y)
unquote <- function(x, where) {
  # browser()
  if (is.call(x)) {
    # if the first element is `!bang` then
    # we could be unquoting this.
    if (identical(x[[1]], `!bang`)) {
      xsub <- x[[2]]
      # if not a call and not `!bang` then
      # this is not a quoting call.
      if (is.call(xsub) && identical(xsub[[1]], `!bang`)) {
        xsub <- xsub[[2]]
        if (is.call(xsub) && identical(xsub[[1]], `!bang`)) {
          xsub <- xsub[[2]]
          x <- spliced(eval(xsub, where))
        } else {
          if (is.call(xsub)) {
            sub_call <- eval(xsub[[1]], where)
            if (!is.name(sub_call)) {
              stop(
                sprintf(
                  "issue unquoting `%s(...)`. `is.name(!!%s)` is not true",
                  as.character(xsub[[1]]), as.character(xsub[[1]])
                ),
                call. = FALSE
              )
            }
            x <- unquote(xsub, where = where)
            x[[1]] <- sub_call
          } else {
            x <- eval(xsub, where)
          }
        }
      } else {
        x[[2]] <- unquote(x[[2]], where = where)
      }
    } else {
      x <- x |>
        lapply(unquote, where = where) |>
        splice() |>
        as.call()
    }
  } else if (is.pairlist(x)) {
    x <- x |>
      lapply(unquote, where = where) |>
      splice() |>
      as.pairlist()
  }
  x
}

enexpr <- function(x) {
  # some arugment passed to this function
  subx <- substitute(x)
  env <- parent.frame()
  # potentually how it was passed to its function
  subx <- do.call(substitute, list(subx), envir = env)
  unquote(subx, where = parent.frame(2L))
}

expr <- function(x) {
  enexpr(x)
}



enexprs <- function(...) {
  subx <- substitute(list(...))
  env <- parent.frame(2L)
  unquote(subx, where = env)
}

exprs <- function(...) {
  as.list(enexprs(...)[-1])
}

list2 <- function(...) {
  subx <- substitute(list(...))
  is_spliced <- vapply(subx, is_splice, logical(1))[-1]
  if (!any(is_spliced)) {
    return(list(...))
  }
  n <- length(is_spliced)

  elements <- vector("list", n)
  env <- parent.frame()
  for (i in seq_along(elements)) {
    if (is_spliced[i]) {
      elements[[i]] <- eval(subx[[i + 1L]][[2L]][[2L]][[2L]], envir = env)
    } else {
      elements[[i]] <- ...elt(i)
    }
  }
  sizes <- rep(1L, n)
  sizes[is_spliced] <- vapply(elements[is_spliced], length, integer(1))
  out <- vector("list", sum(sizes))
  k <- 1L
  for (i in seq_along(elements)) {
    if (is_spliced[i]) {
      xx <- elements[[i]]
      for (j in seq_along(xx)) {
        out[[k]] <- xx[[j]]
        k <- k + 1L
      }
      next
    }
    out[[k]] <- elements[[i]]
    k <- k + 1L
  }
  out
  # eval(exprs, envir = parent.frame())
}

ensub <- function(expr, env) {
  if (missing(env)) {
    env <- parent.frame()
  }
  do.call(substitute, list(expr, env), envir = parent.frame())
}

expr_map <- new_class(
  "expr_map",
  properties = list(
    from = class_name,
    to = new_union(class_name, class_call),
    transform = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(expr) {
          expr <- enexpr(expr)
          env <- list(self@to)
          names(env) <- as.character(self@from)
          do.call(substitute, list(expr, env))
        }
      }
    )
  ),
  constructor = function(from, to) {
    from <- enexpr(from)
    to <- enexpr(to)
    new_object(S7_object(), from = from, to = to)
  }
)

action <- new_class(
  "action",
  parent = class_function,
  properties = list(
    time = time,
    is_done = new_property(
      class = class_logical,
      getter = function(self) {
        self@time@repeating < 0
      }
    )
  ),
  constructor = function(func,
                         time) {
    if (!is.function(func)) {
      stop("func must be a function", call. = FALSE)
    }
    if (!identical(names(formals(func)), c("obj", "time"))) {
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

act_series <- new_class(
  "act_series",
  parent = action,
  properties = list(
    actions = new_property(
      class = class_env,
      getter = function(self) {
        self@actions$.data
      }
    )
  ),
  constructor = function(...) {
    browser()
    dots <- list2(...)
    if (!all(
      vapply(dots, \(x) S7_inherits(x, action), logical(1))
    )) {
      stop("every object of a action series needs to be an action")
    }
    durations <- vapply(
      dots,
      \(x) 1 / x@time@time_scale * (x@time@repeating + 1),
      numeric(1)
    )
    actions <- env(
      list(
        .data = dots,
        .index = 1L
      )
    )
    new_object(
      action(
        function(obj, time) {
          if (time@repeating >= 0) {
            func <- actions$.data[[actions$.index]]
            if (func@is_done) {
              i <- actions$.index <- actions$.index + 1L
              func <- tryCatch(
                actions.data[[i]],
                error = function(cnd) {
                  identity
                }
              )
            }
            invisible(func(obj))
          }
        },
        time = time(duration = sum(durations))
      ),
      actions = actions
    )
  }
)

match_call <- function(obj) {
  env <- parent.frame()
  obj <- substitute(obj)
  call <- match.call(sys.function(2), sys.call(2), envir = parent.frame(3L))
  call
}

get_methods <- function(generic) {
  if (is.character(generic)) {
    generic <- match.fun(generic)
  }
  if (!is.function(generic)) {
    stop("generic must be a function", call. = FALSE)
  }
  UseMethod("get_methods")
}
get_methods.default <- function(generic) {
  on.exit(print(generic))
  stop("Un-reachable state for:")
}
# get_methods.function <- function(generic) {
#   met <- methods(generic)
#   `_empty_` <- function() {}
#   out <- vector("list", length(met))
#   info <- attr(met, "info")
#   for (i in seq_along(out)) {
#     if (isS4) {

#     }
#     if (!is.na(info[i, "from"])) {
#       ns <- getNamespace(info[i, "from"])
#       out[[i]] <- get(info[i, "generic"], envir = ns)
#     } else {
#       out[[i]] <- get(info[i, "generic"], envir = parent.frame())
#     }
#   }
#   lapply(met, function(m) {
#     tryCatch(match.fun(m), error = function(e) `_empty_`)
#   }) |>
#   Filter(function(f) !identical(f, `_empty_`), x = _)
# }

walk_table <- function(tbl) {
  nms <- ls(tbl, all.names = TRUE)
  lst <- list()
  for (nm in nms) {
    val <- tbl[[nm]]
    update <- if (is.environment(val)) {
      walk_table(val)
    } else {
      val
    }
    lst <- c(lst, update)
  }
  lst
}

get_methods.S7_generic <- function(generic) {
  walk_table(generic@methods)
}

all_formals <- function(func) {
  methods <- get_methods(func)
  lapply(methods, formals) |>
    lapply(names) |>
    Reduce(union, x = _)
}

#' @param func a function to turn into an action function
#' @param obj_arg name of argument to pass "obj" to
#' @param remap a expr_map object that describes an
#' argument for the returned function and how it may be
#' augmented within func().
into_action <- function(func, remap, obj_arg = names(formals(func))[1]) {
  action_func_ <- substitute(func)
  if (!is.function(func)) {
    stop("func must be a function", call. = FALSE)
  }
  from <- remap@from
  valid_args <- setdiff(all_formals(func), "...")
  action_func_ <- substitute(action_func_(obj, from, ...))
  names(action_func_)[2] <- obj_arg
  names(action_func_)[3] <- as.character(from)
  force_arg_ <- substitute(force(from))
  action_func_ <- remap@transform(!!action_func_)
  func <- substitute(
    function(arg, time, ...) {
      call <- match.call(expand.dots = FALSE)
      if (is.null(call$time)) {
        stop("time argument is required", call. = FALSE)
      }
      if (!S7_inherits(time, class_time)) {
        stop("time must be a 'class_time' object", call. = FALSE)
      }
      if (any(invalid <- !names(call$...) %in% valid_args)) {
        stop(sprintf(
          "Invalid arguments: %s",
          paste(names(call$...)[invalid], collapse = ", ")
        ), call. = FALSE)
      }
      force_arg_
      action(
        function(obj, time) {
          action_func_
        },
        time = time
      )
    }
  )
  func <- eval(func)
  from_ <- alist(from = , time = , ... = )
  names(from_)[1] <- as.character(from)
  formals(func) <- from_
  func
}
