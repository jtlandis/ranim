pos <- new_class("pos",
  properties = list(
    x = scalar_num,
    y = scalar_num
  ),
  constructor = function(x = 0, y = 0) {
    new_object(S7_object(), x = scalar(x), y = scalar(y))
  }
)

new_pos <- new_generic("new_pos", ".data")

method(new_pos, class_numeric) <- function(.data, ...) {
  n <- length(.data)
  switch(n,
    return(pos(.data, .data)),
    return(pos(.data[1L], .data[2L]))
  )
  stop("new position can only be constructed from a length 1 or 2 numeric")
}

method(new_pos, pos) <- function(.data, ...) {
  .data
}

method(format, pos) <- function(x, ...) {
  sprintf("(%s, %s)", format(x@x), format(x@y))
}

method(print, pos) <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

method(`+`, list(pos, pos)) <- function(e1, e2) {
  pos(e1@x + e2@x, e1@y + e2@y)
}

method(`+`, list(pos, class_numeric)) <- function(e1, e2) {
  pos(e1@x + e2, e1@y + e2)
}

method(`+`, list(class_numeric, pos)) <- function(e1, e2) {
  pos(e1 + e2@x, e1 + e2@y)
}

method(`-`, list(pos, pos)) <- function(e1, e2) {
  pos(e1@x - e2@x, e1@y - e2@y)
}

method(`-`, list(pos, class_numeric)) <- function(e1, e2) {
  pos(e1@x - e2, e1@y - e2)
}

method(`-`, list(class_numeric, pos)) <- function(e1, e2) {
  pos(e1 - e2@x, e1 - e2@y)
}


method(`*`, list(pos, pos)) <- function(e1, e2) {
  pos(e1@x * e2@x, e1@y * e2@y)
}

method(`*`, list(pos, class_numeric)) <- function(e1, e2) {
  pos(e1@x * e2, e1@y * e2)
}

method(`*`, list(class_numeric, pos)) <- function(e1, e2) {
  pos(e1 * e2@x, e1 * e2@y)
}

method(`/`, list(pos, pos)) <- function(e1, e2) {
  pos(e1@x / e2@x, e1@y / e2@y)
}

method(`/`, list(pos, class_numeric)) <- function(e1, e2) {
  pos(e1@x / e2, e1@y / e2)
}

method(`/`, list(class_numeric, pos)) <- function(e1, e2) {
  pos(e1 / e2@x, e1 / e2@y)
}
