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

is_pos <- function(x) inherits(x, "pos")

is_scalar_pos <- function(x) length(x) == 1L && is_pos(x)

scalar_pos_prop <- new_property(
  class = class_pos,
  validator = function(value) {
    if (length(value) != 1L) {
      return("should be length 1")
    }
    NULL
  }
)

vec_ptype2.pos.pos <- function(x, y, ...) new_raw_pos()

vec_cast.pos.double <- function(x, to, ...) pos(x)
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

format.pos <- function(x, ...) {
  sprintf(
    "(%s, %s)", format(vctrs::field(x, "x")),
    format(vctrs::field(x, "y"))
  )
}

vec_arith.pos <- function(op, x, y, ...) {
  UseMethod("vec_arith.pos", y)
}
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

vec_arith.pos.pos <- do_pos_op

vec_arith.numeric.pos <- function(op, x, y, ...) {
  switch(op,
    "&" = ,
    "|" = ,
    "!" = vctrs::stop_incompatible_op(op, x, y),
    pos_num_op(op, num = x, pos = y)
  )
}

vec_arith.pos.numeric <- function(op, x, y, ...) {
  switch(op,
    "&" = ,
    "|" = ,
    "!" = vctrs::stop_incompatible_op(op, x, y),
    pos_num_op(op, pos = x, num = y)
  )
}

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
