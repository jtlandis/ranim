scalar_dbl <- new_class("scalar_dbl",
  parent = class_double,
  validator = function(self) {
    if (length(self) != 1 || is.na(self)) {
      return("must be a single non-missing numeric value")
    }
    NULL
  }
)
scalar_int <- new_class("scalar_int",
  parent = class_integer,
  validator = function(self) {
    if (length(self) != 1 || is.na(self)) {
      return("must be a single non-missing numeric value")
    }
    NULL
  }
)

scalar <- new_generic("scalar", "value", function(value) S7_dispatch())
method(scalar, class_double) <- function(value) {
  scalar_dbl(value)
}

method(scalar, class_integer) <- function(value) {
  scalar_int(value)
}

scalar_num <- new_union(scalar_int, scalar_dbl)

method(scalar, scalar_num) <- function(value) {
  value
}

method(`+`, list(scalar_num, scalar_num)) <- function(e1, e2) {
  scalar(S7_data(e1) + S7_data(e2))
}

method(`-`, list(scalar_num, scalar_num)) <- function(e1, e2) {
  scalar(S7_data(e1) - S7_data(e2))
}

method(`*`, list(scalar_num, scalar_num)) <- function(e1, e2) {
  scalar(S7_data(e1) * S7_data(e2))
}

method(`/`, list(scalar_num, scalar_num)) <- function(e1, e2) {
  scalar(S7_data(e1) / S7_data(e2))
}

method(`^`, list(scalar_num, scalar_num)) <- function(e1, e2) {
  scalar(S7_data(e1)^S7_data(e2))
}

method(`==`, list(scalar_num, scalar_num)) <- function(e1, e2) {
  S7_data(e1) == S7_data(e2)
}

method(`!=`, list(scalar_num, scalar_num)) <- function(e1, e2) {
  S7_data(e1) != S7_data(e2)
}
method(`>`, list(scalar_num, scalar_num)) <- function(e1, e2) {
  S7_data(e1) > S7_data(e2)
}
method(`<`, list(scalar_num, scalar_num)) <- function(e1, e2) {
  S7_data(e1) < S7_data(e2)
}
method(`>=`, list(scalar_num, scalar_num)) <- function(e1, e2) {
  S7_data(e1) >= S7_data(e2)
}
method(`<=`, list(scalar_num, scalar_num)) <- function(e1, e2) {
  S7_data(e1) <= S7_data(e2)
}
