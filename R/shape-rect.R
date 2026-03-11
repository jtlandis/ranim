rect <- new_class("rect",
  parent = shape,
  properties = list(
    width = scalar_num_prop,
    height = scalar_num_prop,
    border = new_property(
      class = new_union(class_color, class_logical),
      default = NA
    )
  )
)


method(render, rect) <- function(shape) {
  pos <- shape@global
  x <- vctrs::field(pos, "x")
  y <- vctrs::field(pos, "y")
  w <- shape@width / 2
  h <- shape@height / 2
  graphics::rect(
    xleft = x - w,
    ybottom = y - h,
    xright = x + w,
    ytop = y + h,
    col = shape@color,
    border = shape@border
  )

  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}


method(obj_scale, list(rect, class_pos)) <-
  function(obj, around, ...,
           size,
           target_size = obj_size(obj) + size,
           scale = target_size / obj_size(obj),
           local = FALSE, recursive = TRUE) {
    obj@height <- obj@height * scale
    obj@width <- obj@width * scale

    obj_scale(
      obj = super(obj, shape),
      around = around,
      ...,
      scale = scale,
      local = local,
      recursive = recursive
    )
  }


method(get_positions, rect) <- function(obj) {
  pos <- obj@global
  x <- vctrs::field(pos, "x")
  y <- vctrs::field(pos, "y")
  w <- obj@width / 2
  h <- obj@height / 2
  positions(pos(c(x + w, x - w), c(y + h, y - h)), get_positions(obj@children))
}


calc_grid <- function(
  obj,
  ncol = 1,
  nrow = 1,
  padding = 0.05,
  xpadding = padding,
  ypadding = padding,
  margin = 0.5,
  xmargin = margin,
  ymargin = margin
) {
  # browser()
  xt <- get_extent(obj)
  .xspace <- diff(xt$x)
  .x_pad <- .xspace * xpadding
  .yspace <- diff(xt$y)
  .y_pad <- .yspace * ypadding
  nxbtwn <- ncol - 1L
  if (nxbtwn == 0L) {
    xmarg <- .x_pad
  } else {
    xmarg <- xmargin * .x_pad
  }
  xbtwn_tot <- .x_pad - xmarg
  nybtwn <- nrow - 1L
  if (nybtwn == 0L) {
    ymarg <- .y_pad
  } else {
    ymarg <- ymargin * .y_pad
  }
  ybtwn_tot <- .y_pad - ymarg
  .width <- (.xspace - .x_pad) / ncol
  .height <- (.yspace - .y_pad) / nrow
  if (nxbtwn == 0) {
    xbtwn <- 0
  } else {
    xbtwn <- xbtwn_tot / nxbtwn
  }
  ybtwn <- ybtwn_tot / (nrow - 1)
  if (nybtwn == 0) {
    ybtwn <- 0
  } else {
    ybtwn <- ybtwn_tot / nybtwn
  }
  .x_move <- xbtwn + .width
  .y_move <- -(ybtwn + .height)

  .x_start <- xt$x[1] + ((.width + xmarg) / 2)
  .y_start <- xt$y[2] - ((.height + ymarg) / 2)

  x <- .x_start + ((seq_len(ncol) - 1L) * .x_move)
  y <- .y_start + ((seq_len(nrow) - 1L) * .y_move)

  p <- vector("list", ncol)
  for (i in seq_len(ncol)) {
    p[[i]] <- pos(x[i], y)
  }
  attr(p, "features") <- c(
    height = .height, width = .width,
    xmarg = xmarg, xbtwn = xbtwn,
    ymarg = ymarg, ybtwn = ybtwn
  )
  p
}
