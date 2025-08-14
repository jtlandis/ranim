library(S7)

source("R/class-scalars.R")
source("R/class-pos.R")
source("R/class-transform.R")
source("R/class-color.R")
source("R/class-shape.R")
source("R/scale.R")
source("R/rotate.R")
source("R/render.R")
source("R/render-helpers.R")
source("R/shape-point.R")
source("R/shape-rect.R")
source("R/shape-polygon.R")




s1 <- point(
  trans = transform(offset = pos(5, 5)),
  advance = function(self, time, delta) {
    obj_rotate(self, pos(0, 0), degrees = 360 * delta)
    if (time < .5) {
      obj_scale(self, size = 2 * delta, recursive = FALSE)
      self@color <- lerp_colors("black", "pink", time / 0.5)
    } else {
      obj_scale(self, size = -2 * delta, recursive = FALSE)
      self@color <- lerp_colors("pink", "black", (time - 0.5) / 0.5)
    }
  }
)
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
        if (time < 0.5) {
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

stuff@advance <- function(self, time, delta) {
  obj_rotate(self, degrees = 720 * delta)
  obj_scale(self, size = delta * ifelse(time < 0.5, -4, 4))
}

action(function(obj, time) {
  obj_rotate(obj, degrees = 360 * time@delta)
}, time = time(steps = 100,))

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
