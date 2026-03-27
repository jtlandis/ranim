#' @include class-time.R

#' Substitute expressions in an environment
#'
#' @param expr An expression to substitute.
#' @param env An environment in which to perform substitution.
#'
#' @return The substituted expression.
#'
#' @keywords internal
ensub <- function(expr, env) {
  if (missing(env)) {
    env <- parent.frame()
  }
  do.call(substitute, list(expr, env), envir = parent.frame())
}

class_expr_to <- new_union(class_name, class_call)

#' Expression mapping for action constructors
#'
#' `expr_map()` is a small helper class used by [`into_action()`] to
#' describe how arguments in a user-facing action constructor
#' are rewritten into calls to a lower-level implementation.
#'
#' Most users should not need to work with `expr_map` directly.
#'
#' @param ... Named arguments mapping parameter names to expressions.
#'
#' @return An object of class `expr_map`.
#'
#' @keywords internal
expr_map <- new_class(
  "expr_map",
  properties = list(
    from = new_property(
      class = class_list,
      validator = function(value) {
        valid <- vapply(value, is.name, logical(1))
        if (!all(valid)) {
          return("from must be a list of class_name objects")
        }
        NULL
      },
    ),
    to = new_property(
      class = class_list,
      validator = function(value) {
        valid <- vapply(value, is.name, logical(1)) |
          vapply(value, is.call, logical(1))
        if (!all(valid)) {
          return("from must be a list of class_name or class_call objects")
        }
        NULL
      },
    ),
    transform = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(expr) {
          expr <- rlang::enexpr(expr)
          env <- list(self@to)
          names(env) <- as.character(self@from)
          do.call(substitute, list(expr, env))
        }
      }
    )
  ),
  constructor = function(...) {
    to <- rlang::exprs(..., .named = TRUE, .ignore_empty = "all")
    from <- rlang::syms(names(to))
    new_object(S7_object(), from = from, to = to)
  }
)

#' Action objects
#'
#' `action()` wraps a simple `(obj, time)` function into an object that
#' can be scheduled and combined with other actions. Actions are
#' associated with [`time`] objects and are attached to [`shape`]
#' instances via the `@action` property.
#'
#' @param func A function with signature `function(obj, time)` that
#'   performs the animation. The `obj` parameter is the shape being
#'   animated, and `time` is a [`time`] object tracking progress.
#' @param time A [`time`] object controlling duration, easing, and
#'   repetition of the action.
#'
#' @section Methods:
#' * `$then(next_action)`: Chain another action to run after this one.
#'   Returns an [`act_series`].
#' * `$finally(final_action)`: Alias for `$then()`.
#' * `$reset()`: Reset the time state to the beginning.
#' * `$repeating(times)`: Set the number of repetitions.
#' * `$is_done`: Logical; whether the action is complete.
#' * `$left`: Remaining time in the current cycle.
#'
#' @return An object of class `action`.
#'
#' @seealso [`time`], [`shape`], [`act_series`], [`into_action()`]
#'
#' @export
action <- new_class(
  "action",
  parent = class_function,
  properties = list(
    time = time,
    is_done = new_property(
      class = class_logical,
      getter = function(self) {
        self@time@is_done
      }
    ),
    then = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(next_action) {
          if (!S7_inherits(next_action, action)) {
            stop("next_action must be an action", call. = FALSE)
          }
          act_series(obj_clone(self), next_action)
        }
      }
    ),
    finally = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(final_action) {
          if (!S7_inherits(final_action, action)) {
            stop("final_action must be an action", call. = FALSE)
          }
          act_series(obj_clone(self), final_action)
        }
      }
    ),
    reset = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function() {
          self@time@reset()
          invisible(self)
        }
      }
    ),
    repeating = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(times) {
          self@time@repeating <- times
          self
        }
      }
    ),
    left = new_property(
      class = class_numeric,
      getter = function(self) {
        self@time@left
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
      function(obj, delta_time) {
        delta_time <- enforce_zero(
          delta_time, time@step(delta_time)@delta_time
        )
        if (delta_time > 1e-15) {
          warning(sprintf("attempted to push time past limit by %.04f", delta_time))
        }
        func(obj, time)
        if (time@cycled && !time@is_done) {
          time@time <- 0
          time@value <- time@ease(time@time)
          time@cycled <- FALSE
        }
        delta_time
      },
      time = time
    )
  }
)

#' Enforce zero on near-zero values
#'
#' @param old_dt Original delta time.
#' @param new_dt New delta time.
#'
#' @return Numeric value, coerced to exactly 0 if nearly zero.
#'
#' @keywords internal
enforce_zero <- function(old_dt, new_dt) {
  val <- old_dt - new_dt
  if (abs(val) < 1e-12) {
    val <- 0
  }
  val
}

#' Capture action sequence as a function
#'
#' Internal helper that creates a function to execute a sequence
#' of actions with proper time tracking and cycling.
#'
#' @param actions An environment containing `.data` (list of actions)
#'   and `.index` (current action index).
#'
#' @return A function `function(obj, time)` that executes actions sequentially.
#'
#' @keywords internal
capture_actions <- function(actions) {
  force(actions)
  function(obj, time) {
    force(time)
    # cat(sprintf("series: %s\n", format(time)))

    curr_act <- actions$.data[[actions$.index]]
    # time_rem <- curr_act@time@remaining

    # the time that the caller elapsed
    delta_time <- time@delta_time
    delta_time <- curr_act(obj, delta_time)

    while (curr_act@is_done) {
      if ((i <- actions$.index + 1L) <= length(actions$.data)) {
        # next index is within range of actions
        actions$.index <- i
      } else if (!time@is_done) {
        # we are at the end of the list AND this timer is not
        # complete yet.

        # if there is still time left on parent time,
        # child may be accelerating.
        if (time@left > 1e-12) {
          time@step(time@left)
          if (time@is_done) {
            break
          }
        }
        for (action in actions$.data) {
          action@reset()
        }
        actions$.index <- 1L
        if (time@cycled && time@is_instant) {
          time@time <- 0
          time@value <- time@ease(time@time)
          time@cycled <- FALSE
          time@step(0)
        }
      } else {
        break
      }
      curr_act <- actions$.data[[actions$.index]]
      if (curr_act@time@is_instant) {
        curr_act(obj, 0)
      }
    }
    delta_time
  }
}

#' Sequences of actions
#'
#' `act_series()` combines multiple [`action`] objects into a
#' single composite action that runs them in sequence, optionally
#' repeating the entire series.
#'
#' @param ... One or more [`action`] objects to execute in sequence.
#' @param repeating Number of times to repeat the entire series
#'   (0 = run once, 1 = run twice, etc.).
#'
#' @return An object of class `act_series`, which itself is an [`action`].
#'
#' @details
#' An `act_series` executes each action in sequence, advancing to the
#' next only after the current action completes. The total duration
#' is the sum of all individual action durations.
#'
#' @examples
#' # Sequence: move for 1 second, then scale for 0.5 seconds
#' series <- act_series(
#'   translate(to = pos(5, 5), time = time(1)),
#'   scale2(size = 2, time = time(0.5))
#' )
#'
#' @seealso [`action`], [`time`], [`translate()`], [`scale2()`]
#'
#' @export
act_series <- new_class(
  "act_series",
  parent = action,
  properties = list(
    actions = new_property(
      class = class_env,
      getter = function(self) {
        self@actions$.data
      }
    ),
    is_done = new_property(
      class = class_logical,
      getter = function(self) {
        self@time@is_done &&
          all(vapply(self@actions, prop, name = "is_done", logical(1)))
      }
    ),
    reset = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function() {
          self@time@reset()
          for (action in self@actions) {
            action@reset()
          }
          invisible(self)
        }
      }
    ),
    then = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(next_action) {
          if (!S7_inherits(next_action, action)) {
            stop("next_action must be an action", call. = FALSE)
          }
          act_series(!!!obj_clone(self@actions), next_action)
        }
      }
    ),
    finally = new_property(
      class = class_function,
      getter = function(self) {
        force(self)
        function(final_action) {
          if (!S7_inherits(final_action, action)) {
            stop("final_action must be an action", call. = FALSE)
          }
          act_series(obj_clone(self), final_action)
        }
      }
    ),
    left = new_property(
      class = class_numeric,
      function(self) {
        action_env <- attr(self, "actions")
        # trigger time udpate
        # self@duration
        curr_action <- action_env$.data[[action_env$.index]]
        curr_action@left
      }
    )
  ),
  constructor = function(..., repeating = 0) {
    dots <- rlang::list2(...)
    if (!all(
      vapply(dots, \(x) S7_inherits(x, action), logical(1))
    )) {
      stop("every object of a action series needs to be an action")
    }
    durations <- vapply(
      dots,
      \(x) x@time@duration * (x@time@repeating + 1),
      numeric(1)
    )
    actions <- env(
      list(
        .data = dots,
        .index = 1L
      )
    )
    time <- time(duration = sum(durations), repeating = repeating)
    act_series_fn <- capture_actions(actions)
    new_object(
      action(
        act_series_fn,
        time = time
      ),
      actions = actions
    )
  }
)

#' Match function call
#'
#' @param obj Object for which to match the call.
#'
#' @return A call object.
#'
#' @keywords internal
match_call <- function(obj) {
  env <- parent.frame()
  obj <- substitute(obj)
  call <- match.call(sys.function(2), sys.call(2), envir = parent.frame(3L))
  call
}

#' Get methods for a generic
#'
#' @param generic A generic function or name.
#' @param name String name of the generic.
#'
#' @return A list of method implementations.
#'
#' @keywords internal
get_methods <- function(generic, name = deparse(substitute(generic))) {
  force(name)
  if (is.character(generic)) {
    name <- generic
  }
  generic <- match.fun(generic)
  if (!is.function(generic)) {
    stop("generic must be a function", call. = FALSE)
  }
  UseMethod("get_methods")
}
get_methods.default <- function(generic, name = NULL) {
  on.exit(print(generic))
  stop("Un-reachable state for:")
}
get_methods.function <- function(generic, name = NULL) {
  met <- methods(name)
  if (length(met) == 0) {
    # its just a simple function
    return(list(generic))
  }
  # Get method info
  info <- attr(met, "info")
  out <- vector("list", length(met))

  # Handle both S3 and S4 methods
  for (i in seq_along(met)) {
    method_name <- met[i]

    # Check if it's an S4 method
    if (!is.null(info) && !is.na(info[i, "isS4"]) && info[i, "isS4"]) {
      # S4 method - get the method definition
      tryCatch(
        {
          # Extract class name from method name
          class_name <- sub(paste0("^", info[i, "generic"], ","), "", method_name)
          class_name <- sub("-method$", "", class_name)
          class_sig <- strsplit(class_name, ",")[[1]]
          out[[i]] <- getMethod(f = class_name, signature = class_sig)
        },
        error = function(e) {
          out[[i]] <- NULL
        }
      )
    } else {
      # S3 method - try to get the function
      tryCatch(
        {
          class_name <- sub(
            paste0("^", info[i, "generic"], "\\."), "",
            method_name
          )
          out[[i]] <- getS3method(
            f = info[i, "generic"],
            class = class_name
          )
        },
        error = function(e) {
          # Fallback - try to find it in global environment or search path
          tryCatch(
            {
              out[[i]] <- get(method_name)
            },
            error = function(e2) {
              out[[i]] <- NULL
            }
          )
        }
      )
    }
  }

  # Filter out NULL entries and return
  Filter(Negate(is.null), out)
}

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

get_methods.S7_generic <- function(generic, name = NULL) {
  walk_table(generic@methods)
}

all_formals <- function(func, name) {
  methods <- get_methods(func, name)
  lapply(methods, formals) |>
    lapply(names) |>
    Reduce(union, x = _)
}

#' @param func a function to turn into an action function
#' @param obj_arg name of argument to pass "obj" to
#' @param remap a expr_map object that describes an
#' argument for the returned function and how it may be
#' augmented within func().
#' Turn a function into an action constructor
#'
#' `into_action()` converts an existing function into a higher-level
#' *action constructor* that returns [`action`] objects instead of
#' performing work immediately.
#'
#' This is used internally to build user-friendly verbs like
#' [`translate()`], [`scale2()`], and color transitions. It takes a
#' base function `func` and an [`expr_map`] describing how caller
#' arguments should be injected into `func`.
#'
#' @param func A function to wrap. It should normally take a shape-like
#'   object as its first argument.
#' @param remap An [`expr_map`] object describing how arguments
#'   in the returned action constructor are translated into
#'   arguments of `func`. Captured arguments are forced before
#'   the action is created to ensure proper scoping.
#' @param obj_arg Name of the argument in `func` that receives
#'   the animated object. Defaults to the name of the first
#'   formal argument of `func`.
#'
#' @return A new function that, when called with `time=` and any
#'   additional arguments, returns an [`action`] object.
#'
#' @examples
#' # This is how translate() is implemented:
#' # translate <- into_action(obj_translate,
#' #   expr_map(to = to, local = FALSE))
#'
#' @seealso [`action`], [`expr_map`]
#'
#' @export
into_action <- function(func, remap, obj_arg = names(formals(func))[1]) {
  action_func_ <- substitute(func)
  func_name <- as.character(action_func_)
  if (!is.function(func)) {
    stop("func must be a function", call. = FALSE)
  }
  from <- remap@from
  is_captured <- Map(
    function(from, to) {
      do.call(
        substitute,
        list(
          to,
          rlang::list2("{from}" := quote(..1))
        )
      )
    },
    from = remap@from,
    to = remap@to
  ) |>
    mapply(FUN = Negate(identical), x = _, y = remap@to)

  valid_args <- setdiff(
    all_formals(func, name = func_name),
    c("...", names(remap@to)[!is_captured])
  )

  action_func_ <- eval(substitute(
    rlang::expr(action_func_(
      !!!rlang::exprs("{obj_arg}" := obj, !!!remap@to),
      ...
    ))
  ))
  # names(action_func_)[2] <- obj_arg
  # names(action_func_)[3] <- as.character(from)
  force_arg_ <- rlang::call2(
    "{",
    !!!lapply(from[is_captured], \(x) call("force", x))
  )
  # action_func_ <- remap@transform(!!action_func_)
  func <- substitute(
    function() {
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
      force(time)
      action(
        function(obj, time) {
          force(time) # ensure time@step(delta_time) is evaluated
          action_func_
        },
        time = time
      )
    }
  )
  func <- eval(func)
  # from_ <- alist(from = , time = , ... = )
  # names(from_)[1] <- as.character(from)
  formals(func) <- rlang::pairlist2(
    !!!lapply(remap@to[is_captured], \(x) rlang::missing_arg()),
    time = ,
    ... =
    )
  func
}

#' @export
method(print, action) <- function(x, indent = "", ...) {
  cat(
    sprintf("%s%s\n", indent, format(x))
  )
  invisible(x)
}

#' @export
method(print, act_series) <- function(x, indent = "", ...) {
  cat(
    sprintf(
      "%s<action series> index: %i\n",
      indent, attr(x, "actions")$.index
    )
  )
  cat(
    sprintf("%s%s\n", indent, format(x@time))
  )
  for (action in x@actions) {
    print(action, indent = paste0(indent, "  "))
  }
  invisible(x)
}

#' @export
method(format, action) <- function(x, ...) {
  c(
    "<action>",
    format(substitute(func, environment(x))),
    sprintf("Time: %s", format(x@time)),
    if (x@time@is_done) {
      " - done"
    } else {
      ""
    }
  )
}
