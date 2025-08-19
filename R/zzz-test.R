library(S7)

source("R/class-scalars.R")
source("R/class-pos.R")
source("R/class-transform.R")
source("R/class-color.R")
source("R/util-ease.R")
source("R/class-env.R")
source("R/class-time.R")
source("R/util-action.R")
source("R/class-shape.R")
source("R/scale.R")
source("R/rotate.R")
source("R/render.R")
source("R/util-actions.R")
source("R/render-helpers.R")
source("R/shape-point.R")
source("R/shape-rect.R")
source("R/shape-polygon.R")
source("R/class-window.R")
source("R/clone.R")




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
    time = time(10, mode = "inc")
  )
)

render_frame(s1, -10:10, -10:10)
while (s1@act()) render(s1)

s1@
child(
  offset = pos(1, 1),
  point(
    advance = function(self, time, delta) {
      obj_rotate(self, degrees = 720 * delta, local = TRUE)
      if (time < 0.5) {
        self@color <- lerp_colors("cyan", "orange", time / 0.5)
      } else {
        self@color <- lerp_colors("orange", "cyan", (time - 0.5) / 0.5)
      }
    }
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
)@child(shape(), offset = pos(5, -4.1))

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


stuff <- shape()
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
  pos(),
  pos(2, 0),
  pos(0, 2),
  trans = transform(pos(1, 1))
)@child(
  apoly(pos(), pos(1, 0), pos(0, 1),
    color = "cyan"
  ),
  offset = pos(1, 1)
)
stuff@child(points)@child(tri)
stuff@action(
  act_series(
    rotate(
      360,
      time(
        duration = 3,
        ease = ease(ecubic)
      )
    ),
    scale(2, time(1.4, ease = ease(ecos))),
    scale(-2, time(2, ease = ease(ecubic, ecubic)))
))
render_frame(stuff, -10:10, -10:10)
i <- 0L
ii <- 0L
time <- Sys.time()
while (stuff@act()) {
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

w <- window(
  bl = pos(-1, -1),
  tr = pos(1, 1)
)@action(
  act_series(
    action(
      function(obj, time) {
        cat(sprintf("child1: %s\n", format(time)))
        obj_scale(obj, size = 2 * time@delta, recursive = FALSE)
      },
      time(10, ease = ease(ecubic))
    ),
    action(
      function(obj, time) {
        cat(sprintf("child2: %s\n", format(time)))
        obj_rotate(obj, degrees = 360 * time@delta, local = TRUE)
      },
      time(5)
    )
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
  )
)

p <- point(
  trans = transform(offset = pos(.5, 0)),
  color = "red"
)

w@child(p)
spin <- rotate(180, time = time(5, repeating = 1), local = TRUE)
p@action(spin)

wc <- obj_clone(w)
anim(w)
count <- 0L
while (w@act()) {
  count <- count + 1L
  render(w)
}
