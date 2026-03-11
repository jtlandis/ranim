# positions <- new_class(
#   "positions",
#   parent = class_list,
#   properties = list(
#     center = new_property(
#       class = pos,
#       getter = function(self) {
#         n <- length(self)
#         if (n == 0) {
#           return(pos(0, 0))
#         }
#         xs <- vapply(self, function(p) S7_data(p@x), numeric(1))
#         ys <- vapply(self, function(p) S7_data(p@y), numeric(1))
#         pos(mean(xs), mean(ys))
#       }
#     )
#   ),
#   validator = function(self) {
#     if (!all(vapply(self, function(x) is_pos(x), logical(1)))) {
#       return("all elements must be 'pos' objects")
#     }
#     NULL
#   },
#   constructor = function(...) {
#     pts <- rlang::list2(...)
#     new_object(pts)
#   }
# )

# method(format, positions) <- function(x, ...) {
#   pts <- vapply(x, format, character(1))
#   paste0("positions: [", paste(pts, collapse = ", "), "]")
# }

# method(print, positions) <- function(x, ...) {
#   cat(format(x), "\n")
#   invisible(x)
# }

# method(`[`, positions) <- function(x, i, ...) {
#   positions(!!!NextMethod())
# }

apoly <- new_class(
  "apoly",
  parent = shape,
  properties = list(
    points = class_pos
  ),
  constructor = function(...,
                         trans = transform(),
                         parent = NULL,
                         children = list(),
                         actions = list(),
                         color = "black") {
    S7::new_object(shape(
      trans = trans, parent = parent,
      children = children, color = class_color(color),
      actions = actions
    ), points = positions(...))
  }
)


method(render, apoly) <- function(shape) {
  pos <- shape@points + shape@global
  graphics::polygon(
    x = vctrs::field(pos, "x"),
    y = vctrs::field(pos, "y"),
    col = shape@color
  )
  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}

method(obj_scale, list(apoly, class_pos)) <-
  function(obj, around, ...,
           size,
           target_size = obj_size(obj) + size,
           scale = target_size / obj_size(obj),
           local = FALSE, recursive = TRUE) {
    obj@points <- scale_local_pos(
      obj@points,
      scale = scale
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

method(obj_rotate, list(apoly, class_pos)) <-
  function(obj, around,
           ...,
           radians = degrees * pi / 180,
           degrees = radians * 180 / pi,
           local = FALSE,
           recursive = TRUE) {
    obj@points <- rotate_local_pos(
      obj@points,
      radians = radians
    )
    obj_rotate(
      obj = super(obj, shape),
      around = around,
      ...,
      radians = radians,
      local = local,
      recursive = recursive
    )
  }

method(get_positions, apoly) <- function(obj) {
  lst <- get_positions(obj@children)
  points <- obj@points + obj@global
  positions(obj@global, points, lst)
}
