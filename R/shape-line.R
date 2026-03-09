arrow <- new_class(
  "arrow",
  properties = list(
    side = new_property(
      class = class_character,
      default = "end",
      validator = function(value) {
        m <- match(value, c("start", "end", "both"), nomatch = 0L)
        if (m == 0L) {
          return("should be `start`, `end`, or `both`")
        }
        NULL
      }
    ),
    length = new_property(
      class = class_numeric,
      default = 0.15,
      validator = scalar_num_prop$validator
    ),
    angle = new_property(
      class = class_numeric,
      default = 45,
      validator = scalar_num_prop$validator
    )
  )
)

stroke_prop <- new_property(
  class = class_character,
  default = "solid",
  validator = function(value) {
    if (length(value) != 1L) {
      return("should be length 1.")
    }
    m <- match(
      value,
      c(
        "blank", "solid", "dashed", "dotted",
        "dotdash", "longdash", "twodash"
      ),
      nomatch = 0L
    )
    if (m == 0L) {
      v <- strsplit(value)[[1L]]
      if (!all(grepl("[0-9]", v))) {
        return(
          sprintf(
            "should be one of: %s, or custom pattern i.e. `13`",
            paste(c(
              "blank", "solid", "dashed", "dotted",
              "dotdash", "longdash", "twodash"
            ), collapse = ", ")
          )
        )
      }
    }
    NULL
  }
)

line <- new_class(
  "line",
  parent = shape,
  properties = list(
    points = class_pos,
    stroke = stroke_prop,
    weight = new_property(
      class = S7::class_numeric,
      default = -1
    ),
    arrow = new_property(
      class = new_union(NULL, arrow),
      default = NULL
    )
  ),
  constructor = function(...,
                         trans = transform(),
                         parent = NULL,
                         children = list(),
                         actions = list(),
                         color = "black",
                         weight = -1,
                         arrow = NULL,
                         stroke = "solid") {
    S7::new_object(shape(
      trans = trans, parent = parent,
      children = children, color = class_color(color),
      actions = actions
    ), points = positions(...), weight = weight, arrow = arrow, stroke = stroke)
  }
)

dist0 <- function(x0, x1, y0, y1) {
  sqrt(
    sum(c(x1 - x0, y1 - y0)^2)
  )
}

method(render, line) <- function(shape) {
  pos <- shape@global
  pts <- shape@points
  points <- pos + pts
  vals <- graphics::xspline(
    x = vctrs::field(points, "x"),
    y = vctrs::field(points, "y"),
    shape = shape@weight,
    draw = FALSE
  )
  graphics::lines(vals$x, vals$y,
    lwd = shape@trans@size,
    lty = shape@stroke
  )
  if (!is.null(arr <- shape@arrow)) {
    render_arrow(arr, vals, shape)
  }
  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}

render_arrow <- function(arr, vals, shape) {
  switch(arr@side,
    "end" = {
      n <- length(vals$x)
      x1 <- vals$x[n]
      y1 <- vals$y[n]
      i <- 1L
      while (dist0(
        x0 <- vals$x[n - i], x1,
        y0 <- vals$y[n - i], y1
      ) < 0.01) {
        i <- i + 1L
      }
      graphics::arrows(x0, y0, x1, y1,
        code = 2L,
        angle = arr@angle, length = arr@length,
        lwd = shape@trans@size
      )
    },
    "start" = {
      x0 <- vals$x[1L]
      y0 <- vals$y[1L]
      i <- 2L
      while (dist0(
        x0, x1 <- vals$x[i],
        y0, y1 <- vals$y[i]
      ) < 0.01) {
        i <- i + 1L
      }
      graphics::arrows(x0, y0, x1, y1,
        angle = arr@angle, code = 1L, length = arr@length,
        lwd = shape@trans@size
      )
    },
    "both" = {
      n <- length(vals$x)
      x1 <- vals$x[n]
      y1 <- vals$y[n]
      i <- 1L
      while (dist0(
        x0 <- vals$x[n - i], x1,
        y0 <- vals$y[n - i], y1
      ) < 0.01) {
        i <- i + 1L
      }
      graphics::arrows(x0, y0, x1, y1,
        code = 2L,
        angle = arr@angle, length = arr@length,
        lwd = shape@trans@size
      )

      x0 <- vals$x[1L]
      y0 <- vals$y[1L]
      i <- 2L
      while (dist0(
        x0, x1 <- vals$x[i],
        y0, y1 <- vals$y[i]
      ) < 0.01) {
        i <- i + 1L
      }
      graphics::arrows(x0, y0, x1, y1,
        angle = arr@angle, code = 1L, length = arr@length,
        lwd = shape@trans@size
      )
    }
  )
}

method(obj_scale, list(line, class_pos)) <-
  function(obj, around, ...,
           size,
           target_size = obj_size(obj) + size,
           scale = target_size / obj_size(obj),
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

method(obj_rotate, list(line, class_pos)) <-
  function(obj, around,
           ...,
           radians = degrees * pi / 180,
           degrees = radians * 180 / pi,
           local = FALSE,
           recursive = TRUE) {
    obj@points <- positions(
      !!!lapply(
        obj@points,
        rotate_local_pos,
        radians = radians
      )
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

method(get_positions, line) <- function(obj) {
  lst <- lapply(obj@children, get_positions) |>
    unlist()
  points <- lapply(obj@points, \(p, g) p + g, g = obj@global)
  positions(obj@global, !!!points, !!!lst)
}
