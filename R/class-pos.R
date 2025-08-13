pos <- new_class("pos",
  properties = list(
    x = scalar_num,
    y = scalar_num
  ),
  constructor = function(x = 0, y = 0) {
    new_object(S7_object(), x = scalar(x), y = scalar(y))
  }
)

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
