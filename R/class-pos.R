#' Position vectors
#'
#' `pos()` creates a vector of (x, y) positions. Positions support
#' arithmetic operations with other positions and numeric scalars.
#'
#' @param x Numeric vector of x-coordinates. If `y` is missing,
#'   both x and y coordinates are set to `x`.
#' @param y Numeric vector of y-coordinates. Defaults to `x`.
#'
#' @return An object of class `pos` with recycled `x` and `y` components.
#'
#' @details
#' Position objects are built on the **vctrs** infrastructure and support:
#' - Field access: `pos$x` and `pos$y`
#' - Arithmetic: `+`, `-`, `*`, `/`, etc. (vectorized)
#' - Math functions: `sum()`, `mean()`, `prod()` on positions
#'
#' Use `positions(...)` to combine multiple `pos` objects into a
#' single vector.
#'
#' @examples
#' # Create individual positions
#' p1 <- pos(1, 2)
#' p2 <- pos(3, 4)
#'
#' # Access fields
#' p1$x
#' p1$y
#'
#' # Arithmetic
#' p1 + p2
#' p1 - p2
#' p1 * 2
#'
#' # Combine positions
#' positions(p1, p2)
#'
#' @export
pos <- function(x = numeric(), y = x) {
  if (!is.numeric(x)) {
    stop("x should be numeric", call. = FALSE)
  }
  if (!is.numeric(y)) {
    stop("y should be numeric", call. = FALSE)
  }
  new_raw_pos(vctrs::vec_recycle_common(x = x, y = y))
}

pos_x <- function(pos) {
  vctrs::field(pos, "x")
}

pos_y <- function(pos) {
  vctrs::field(pos, "y")
}

#' @export
`$.pos` <- function(x, name) {
  switch(name,
    x = pos_x(x),
    y = pos_y(x),
    stop(sprintf("unknown field '%s'", name), call. = FALSE)
  )
}

#' @export
`.DollarNames.pos` <- function(x, pattern) {
  if (nchar(pattern) == 0) {
    return(c("x", "y"))
  }
  switch(pattern,
    x = "x",
    y = "y",
    character()
  )
}

pos_op <- function(op, x, y) {
  args <- vctrs::vec_recycle_common(x, y)
  op_fn <- getExportedValue("base", op)
  xx <- vctrs::vec_data(args[[1L]])
  yy <- vctrs::vec_data(args[[2L]])
  new_raw_pos(
    list(
      x = op_fn(xx$x, yy$x),
      y = op_fn(xx$y, yy$y)
    )
  )
}

pos_num_op <- function(op, pos, num) {
  args <- vctrs::vec_recycle_common(pos, num)
  op_fn <- getExportedValue("base", op)
  xx <- vctrs::vec_data(args[[1L]])
  yy <- vctrs::vec_data(args[[2L]])
  new_raw_pos(
    list(
      x = op_fn(xx$x, yy),
      y = op_fn(xx$y, yy)
    )
  )
}

#' Combine position vectors
#'
#' `positions()` combines multiple [`pos`] objects or vectors
#' into a single position vector.
#'
#' @param ... One or more [`pos`] objects or numeric values to combine.
#'
#' @return A [`pos`] vector.
#'
#' @examples
#' p1 <- pos(1, 2)
#' p2 <- pos(3, 4)
#' positions(p1, p2)
#'
#' @export
positions <- function(...) {
  vctrs::vec_c(..., .ptype = new_raw_pos())
}

.pos_class <- c("pos", "vctrs_rcrd", "vctrs_vctr")


new_raw_pos <- function(lst = list(x = numeric(), y = numeric())) {
  class(lst) <- .pos_class
  lst
}

new_pos <- function(.data) {
  vctrs::vec_cast(.data, new_raw_pos())
}

class_pos <- S7::new_S3_class(.pos_class, constructor = new_pos)

#' Check if an object is a position vector
#'
#' @param x Object to check.
#'
#' @return Logical scalar.
#'
#' @export
is_pos <- function(x) inherits(x, "pos")

#' Check if an object is a scalar position
#'
#' `is_scalar_pos()` checks if `x` is a position with exactly
#' one element.
#'
#' @param x Object to check.
#'
#' @return Logical scalar.
#'
#' @export
is_scalar_pos <- function(x) length(x) == 1L && is_pos(x)

scalar_pos_prop <- S7::new_property(
  class = class_pos,
  validator = function(value) {
    if (length(value) != 1L) {
      return("should be length 1")
    }
    NULL
  }
)

#' @export
vec_ptype2.pos.pos <- function(x, y, ...) new_raw_pos()


#' @export
vec_cast.pos.double <- function(x, to, ...) pos(x)


#' @export
vec_cast.pos.integer <- function(x, to, ...) pos(x)

# new_pos <- new_generic("new_pos", ".data")

# method(new_pos, class_numeric) <- function(.data, ...) {
#   n <- length(.data)
#   switch(n,
#     return(pos(.data, .data)),
#     return(pos(.data[1L], .data[2L]))
#   )
#   stop("new position can only be constructed from a length 1 or 2 numeric")
# }

# method(new_pos, pos) <- function(.data, ...) {
#   .data
# }


#' @export
format.pos <- function(x, ...) {
  sprintf(
    "(%s, %s)", format(vctrs::field(x, "x")),
    format(vctrs::field(x, "y"))
  )
}


#' @method vec_arith pos
#' @export
vec_arith.pos <- function(op, x, y, ...) {
  UseMethod("vec_arith.pos", y)
}

#' @method vec_arith.pos default
#' @export
vec_arith.pos.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}


do_pos_op <- function(op, x, y, ...) {
  switch(op,
    "&" = ,
    "|" = ,
    "!" = vctrs::stop_incompatible_op(op, x, y),
    pos_op(op, x, y)
  )
}

#' @method vec_arith.pos pos
#' @export
vec_arith.pos.pos <- function(op, x, y, ...) {
  do_pos_op(op, x, y, ...)
}

#' @method vec_arith.numeric pos
#' @exportS3Method
vec_arith.numeric.pos <- function(op, x, y, ...) {
  switch(op,
    "&" = ,
    "|" = ,
    "!" = vctrs::stop_incompatible_op(op, x, y),
    pos_num_op(op, num = x, pos = y)
  )
}

#' @method vec_arith.pos numeric
#' @export
vec_arith.pos.numeric <- function(op, x, y, ...) {
  switch(op,
    "&" = ,
    "|" = ,
    "!" = vctrs::stop_incompatible_op(op, x, y),
    pos_num_op(op, pos = x, num = y)
  )
}


#' @export
vec_math.pos <- function(.fn, .x, ...) {
  switch(.fn,
    sum = new_raw_pos(lapply(vctrs::vec_data(.x), sum)),
    mean = new_raw_pos(lapply(vctrs::vec_data(.x), mean)),
    prod = new_raw_pos(lapply(vctrs::vec_data(.x), prod)),
    any = ,
    all = stop(sprintf("cannot perform %s on pos vectors", .fn)),
    new_raw_pos(vctrs::vec_math_base(.fn, .x, ...))
  )
}

# method(`+`, list(pos, pos)) <- function(e1, e2) {
#   pos(e1@x + e2@x, e1@y + e2@y)
# }

# method(`+`, list(pos, class_numeric)) <- function(e1, e2) {
#   pos(e1@x + e2, e1@y + e2)
# }

# method(`+`, list(class_numeric, pos)) <- function(e1, e2) {
#   pos(e1 + e2@x, e1 + e2@y)
# }

# method(`-`, list(pos, pos)) <- function(e1, e2) {
#   pos(e1@x - e2@x, e1@y - e2@y)
# }

# method(`-`, list(pos, class_numeric)) <- function(e1, e2) {
#   pos(e1@x - e2, e1@y - e2)
# }

# method(`-`, list(class_numeric, pos)) <- function(e1, e2) {
#   pos(e1 - e2@x, e1 - e2@y)
# }


# method(`*`, list(pos, pos)) <- function(e1, e2) {
#   pos(e1@x * e2@x, e1@y * e2@y)
# }

# method(`*`, list(pos, class_numeric)) <- function(e1, e2) {
#   pos(e1@x * e2, e1@y * e2)
# }

# method(`*`, list(class_numeric, pos)) <- function(e1, e2) {
#   pos(e1 * e2@x, e1 * e2@y)
# }

# method(`/`, list(pos, pos)) <- function(e1, e2) {
#   pos(e1@x / e2@x, e1@y / e2@y)
# }

# method(`/`, list(pos, class_numeric)) <- function(e1, e2) {
#   pos(e1@x / e2, e1@y / e2)
# }

# method(`/`, list(class_numeric, pos)) <- function(e1, e2) {
#   pos(e1 / e2@x, e1 / e2@y)
# }
