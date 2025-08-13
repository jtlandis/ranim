positions <- new_class(
  "positions",
  parent = class_list,
  properties = list(
    center = new_property(
      class = pos,
      getter = function(self) {
        n <- length(self)
        if (n == 0) {
          return(pos(0, 0))
        }
        xs <- vapply(self, function(p) S7_data(p@x), numeric(1))
        ys <- vapply(self, function(p) S7_data(p@y), numeric(1))
        pos(mean(xs), mean(ys))
      }
    )
  ),
  validator = function(self) {
    if (!all(vapply(self, function(x) S7_inherits(x, pos), logical(1)))) {
      return("all elements must be 'pos' objects")
    }
    NULL
  },
  constructor = function(...) {
    pts <- rlang::list2(...)
    new_object(pts)
  }
)

method(format, positions) <- function(x, ...) {
  pts <- vapply(x, format, character(1))
  paste0("positions: [", paste(pts, collapse = ", "), "]")
}

method(print, positions) <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

method(`[`, positions) <- function(x, i, ...) {
  positions(!!!NextMethod())
}

apoly <- new_class(
  "apoly",
  parent = shape,
  properties = list(
    points = new_property(
      class = positions
    )
  ),
  constructor = function(...,
                         trans = transform(pos(), pos()),
                         parent = NULL,
                         children = list(),
                         color = "black", advance = NULL) {
    S7::new_object(shape(
      trans = trans, parent = parent,
      children = children, color = class_color(color), advance = advance
    ), points = positions(...))
  }
)


method(render, apoly) <- function(shape) {
  pos <- shape@global
  pts <- shape@points
  xs <- vapply(pts, function(p) S7_data(p@x), numeric(1))
  ys <- vapply(pts, function(p) S7_data(p@y), numeric(1))
  graphics::polygon(
    x = xs + S7_data(pos@x),
    y = ys + S7_data(pos@y),
    col = shape@color
  )
  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}

method(obj_scale, list(apoly, pos)) <-
  function(obj, around, ...,
           size, scale = size / obj_size(obj),
           local = FALSE, recursive = TRUE) {
    obj@points <- positions(
      !!!lapply(
        obj@points,
        scale_local_pos,
        scale = scale
      )
    )
    obj_scale(
      obj = super(obj, shape),
      around = around,
      ...,
      scale = scale,
      local = local,
      recursive = recursive
    )
  }
