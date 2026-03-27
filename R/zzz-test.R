# library(S7)

# source("R/class-scalars.R")
# source("R/class-pos.R")
# source("R/class-transform.R")
# source("R/class-color.R")
# source("R/util-ease.R")
# source("R/class-env.R")
# source("R/class-time.R")
# source("R/util-action.R")
# source("R/class-shape.R")
# source("R/scale.R")
# source("R/rotate.R")
# source("R/render.R")
# source("R/util-actions.R")
# source("R/render-helpers.R")
# source("R/shape-point.R")
# source("R/shape-rect.R")
# source("R/shape-polygon.R")
# source("R/class-window.R")
# source("R/clone.R")


s1 <- point(
  trans = transform(offset = pos(5, 5))
)@action(
  action(
    function(obj, time) {
      # print(time)
      obj_rotate(obj, pos(0, 0), degrees = 360 * time@delta)
      if (time@time < .5) {
        obj_scale(obj, size = 2 * time@delta, recursive = FALSE)
        obj@color <- lerp_colors("black", "pink", time@time / 0.5)
      } else {
        obj_scale(obj, size = -2 * time@delta, recursive = FALSE)
        obj@color <- lerp_colors("pink", "black", (time@time - 0.5) / 0.5)
      }
    },
    time = time(10)
  )
)

render_frame(s1, -10:10, -10:10)
while (s1@act()) render(s1)

s1 <- point(
  trans = transform(offset = pos(5, 5))
)@action(
  action(
    function(obj, time) {
      # print(time)
      obj_rotate(obj, pos(0, 0), degrees = 360 * time@delta)
      if (time@time < .5) {
        obj_scale(obj, size = 2 * time@delta, recursive = FALSE)
        obj@color <- lerp_colors("black", "pink", time@time / 0.5)
      } else {
        obj_scale(obj, size = -2 * time@delta, recursive = FALSE)
        obj@color <- lerp_colors("pink", "black", (time@time - 0.5) / 0.5)
      }
    },
    time = time(10)
  )
)

render_frame(s1, -10:10, -10:10)
while (s1@act()) render(s1)

s1@
child(
  offset = pos(1, 1),
  point(
    advance =
    )@child(
    offset = pos(-1, 1),
    point(
      advance = function(self, time, delta) {
        obj_rotate(self, degrees = 720 * delta, local = TRUE)
        if (time@time < 0.5) {
          self@color <- lerp_colors("purple", "yellow", time / 0.5)
        } else {
          self@color <- lerp_colors("yellow", "purple", (time - 0.5) / 0.5)
        }
      }
    )
  )
)@child(shape(), offset = pos(5, -4.1))@action(
  action(
    function(self, time) {
      obj_rotate(self, degrees = 720 * time@delta, local = TRUE)
      if (time < 0.5) {
        self@color <- lerp_colors("cyan", "orange", time@value / 0.5)
      } else {
        self@color <- lerp_colors("orange", "cyan", (time@va - 0.5) / 0.5)
      }
    }
  )
)

render_frame(s1, -10:10, -10:10)

for (i in seq_len(1000)) {
  s1@advance(time = i / 1000, delta = 1 / 1000)
  Sys.sleep(0.008)
  # png(sprintf("frames/frame_%04i.png", i))
  render_frame(s1, -10:10, -10:10)
  # dev.off()
}
img_files <- list.files(path = "frames", pattern = "frame_.*\\.png", full.names = TRUE)
magick::image_read(img_files) |>
  magick::image_animate(fps = 100) |>
  magick::image_write("animation3.gif")

w <- window(bl = pos(-10, -10), tr = pos(10, 10))
stuff <- shape()@action(
  translate(to = pos(-6, -6), time(duration = 2))
)
points <- point(
  color = "red",
  trans = transform(pos(1, 1))
)@child(
  point(color = "blue"),
  offset = pos(3, 0)
)@child(
  point(color = "yellow"),
  offset = pos(0, 3)
)
tri <- apoly(
  pos(0),
  pos(2, 0),
  pos(0, 2),
  trans = transform(pos(1, 1))
)@child(
  apoly(pos(0), pos(1, 0), pos(0, 1),
    color = "cyan"
  ),
  offset = pos(1, 1)
)
stuff@child(points)@child(tri)
# stuff@action(
#   act_series(
#     rotate(
#       360,
#       time(
#         duration = 3,
#         ease = ease(ecubic)
#       )
#     ),
#     scale(2, time(1.4, ease = ease(ecos))),
#     scale(-2, time(2, ease = ease(ecubic, ecubic)))
# ))
# render_frame(stuff, -10:10, -10:10)
anim(w@child(stuff), fps = 100)
i <- 0L
ii <- 0L
time <- Sys.time()
while (stuff@act(0.001)) {
  d <- Sys.time() - time
  if (d > 1) {
    time <- Sys.time()
    cat(sprintf("FPS %i, time: %s\n", i - ii, d))
    ii <- i
  }
  png(sprintf("frames/frame_%04i.png", i))
  render_frame(stuff, -10:10, -10:10)
  i <- i + 1L
  dev.off()
}

img_files <- list.files(path = "frames", pattern = "frame_.*\\.png", full.names = TRUE)
magick::image_read(img_files) |>
  magick::image_animate(fps = 100) |>
  magick::image_write("animation4.gif")


stuff@advance <- function(self, time, delta) {
  obj_rotate(self, degrees = 720 * delta)
  obj_scale(self, size = delta * ifelse(time < 0.5, -4, 4))
}

action(function(obj, time) {
  obj_rotate(obj, degrees = 360 * time@delta)
}, time = time(ease = ease(ecos, esin)))

for (i in seq_len(800)) {
  stuff@advance(time = i / 800, delta = 1 / 800)
  png(sprintf("frames/fframe_%04i.png", i))
  render_frame(stuff, -5:5, -5:5)
  dev.off()
}

img_files <- list.files(path = "frames", pattern = "fframe_.*\\.png", full.names = TRUE)
magick::image_read(img_files) |>
  magick::image_animate(fps = 100) |>
  magick::image_write("animation3.gif")

render_frame(stuff, -1:5, -1:5)


source("R/zzz-load.R")

acts <- scale(2, time(5, ease = ease(ecubic)), recursive = FALSE)@
then(
  rotate(360, time(3), local = TRUE)
)
spin <- rotate(180, time = time(5, repeating = 1), local = TRUE)
acts@time@repeating <- 1L
w <- window(
  bl = pos(-1, -1),
  tr = pos(1, 1)
)@action(
  act_series(
    acts,
    scale(-4, time = time(2, ease = ease(ecubic)), recursive = FALSE)@
    then(
      action(
        \(obj, time) {
          cat("Resetting spin\n")
          cat(format(time), "\n")
          obj@children[[4]]@action(
            spin@reset()
          )
        },
        time = time(0)
      )
    ),
    repeating = 2
  )
)@child(
  apoly(
    pos(-3, -3),
    pos(3, -3),
    pos(3, 3),
    pos(-3, 3),
    color = "red"
  )
)@child(
  apoly(
    pos(-2, -2),
    pos(2, -2),
    pos(2, 2),
    pos(-2, 2),
    color = "blue"
  )
)@child(
  apoly(
    pos(-1, -1),
    pos(1, -1),
    pos(1, 1),
    pos(-1, 1),
    color = "green"
  )@action(
    colors(
      colors = c("green", "steelblue", "green"),
      2,
      Inf
    )
  )
)

p <- point(
  trans = transform(offset = pos(.5, 0)),
  color = "red"
)

w@child(p)
p@action(spin)

wc <- obj_clone(w)
.w <- anim(w, fps = 100)
count <- 0L
while (w@act()) {
  count <- count + 1L
  render(w)
}

mv_right <- duplicate(time(0))@then(offset(by = pos(1.2, 0), time(1)))@repeating(5L)

green_rect <- rect(color = "#a6f839")@action(
  spawn_sibling(rect(color = "#a6f839"), time(0))@then(
    offset(by = pos(0, -1.2), time(1))
  )@repeating(4)
)
# rect(trans = transform(pos(-8.5, 8.5)), color = "#a6f839")@action(
#   act_series(
#     spawn_sibling(green_rect, offset = pos(0, 0), time(0))@then(
#       offset(by = pos(1.2, 0), time(1)))@repeating(5),
#     spawn_sibling(green_rect, time(0))
#   )
# )

w <- window(bl = pos(-10, -10), tr = pos(10, 10))@child(
  shape(trans = transform(pos(-8, 8)))@action(
    spawn_sibling(
      time = time(0),
      rect(color = "#a6f839", width = .9, height = .9)@action(
        spawn_sibling(
          time = time(0),
          rect(color = "#a6f839", width = .9, height = .9)
        )@then(
          offset(by = pos(0, -1), time = time(.5))
        )@repeating(3)
      )
    )@then(
      offset(by = pos(1, 0), time = time(.7))
    )@repeating(5)
  )
)
# w@action(
#   spawn(rect()@action(
#     offset(by = pos(4, 0), time = time(5))
#   )@action(
#     offset(by = pos(0, 5), time = time(5))
#   )@action(
#     offset(by = pos(-4, 0), time = time(5))
#   ), pos(0,0), time = time(0))
# )

.w <- anim(w)
.w@children <- .w@children[-1L]
pal <- scales::pal_gradient_n(scales::pal_viridis(1, 0, 1, 1, "D")(6))
set.seed(42)
lapply(.w@children, \(child) {
  child$start <- child@color
  child$val <- runif(1)
  child$end <- pal(child$val)
  child@action(
    color(child$start, child$end, time = time(3, ease = ease(ecubic)))
  )
})
.w <- anim(.w)
.c <- obj_clone(.w)
.w@children <- .w@children[vapply(.w@children, \(obj) {
  pos <- obj_pos(obj)
  c(pos@x, pos@y)
}, numeric(2)) |>
  t() |>
  as.data.frame() |>
  do.call(order, args = _)]
row_order <- vapply(.w@children, \(x) x$val, 1) |>
  matrix(5, 6) |>
  dist() |>
  hclust() |>
  _$order
xtent <- get_extent(.w@children)
target_y <- do.call(seq, args = as.list(xtent$ylim))[row_order]
col_order <- vapply(.w@children, \(x) x$val, 1) |>
  matrix(5, 6) |>
  t() |>
  dist() |>
  hclust() |>
  _$order
target_x <- do.call(seq, args = as.list(xtent$xlim))[col_order]
mapply(
  \(child, target) {
    child$new_y <- target
    child@action(
      offset(by = pos(0, target - obj_pos(child)@y), time = time(3))
    )
  },
  child = .w@children,
  target = vctrs::vec_rep(target_y, 6)
)
# .w2 <- anim(.w)
mapply(
  \(child, target) {
    child$new_y <- target
    child@action(
      offset(by = pos(target - obj_pos(child)@x, 0), time = time(3))
    )
  },
  child = .w@children,
  target = vctrs::vec_rep_each(target_x, 5)
)
.w3 <- anim(.w)
