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

transform <- new_class("transform",
  properties = list(
    anchor = pos,
    offset = pos,
    global = new_property(
      class = pos,
      getter = function(self) {
        self@anchor + self@offset
      },
      setter = function(self, value) {
        if (!S7_inherits(value, pos)) {
          stop("value must be a pos object", call. = FALSE)
        }
        self@offset <- value - self@anchor
        invisible(self)
      }
    )
  ),
  constructor = function(anchor = pos(), offset = pos()) {
    new_object(S7_object(), anchor = anchor, offset = offset)
  }
)

method(format, transform) <- function(x, ...) {
  sprintf("transform: %s + %s -> %s", format(x@anchor), format(x@offset), format(x@global))
}

method(print, transform) <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}
