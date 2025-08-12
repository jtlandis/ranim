library(S7)

source("R/class-scalars.R")
source("R/class-pos.R")
source("R/class-color.R")
source("R/class-shape.R")
source("R/rotate.R")
source("R/render.R")
source("R/render-helpers.R")
source("R/shape-point.R")
source("R/shape-rect.R")
source("R/shape-polygon.R")




s1 <- shape(
  trans = transform(offset = pos(5, 5)),
  advance = function(self, time, delta) {
    rotate(self, pos(0, 0), 360 * delta)
    if (time < .5) {
      self@color <- lerp_colors("black", "pink", time / 0.5)
    } else {
      self@color <- lerp_colors("pink", "black", (time - 0.5) / 0.5)
    }
  }
)
s1@
child(
  shape, pos(1, 1),
  advance = function(self, time, delta) {
    rotate_local(self, angle = 720 * delta)
    if (time < 0.5) {
      self@color <- lerp_colors("cyan", "orange", time / 0.5)
    } else {
      self@color <- lerp_colors("orange", "cyan", (time - 0.5) / 0.5)
    }
  },
  apply_child = \(child) {
    child@child(shape, pos(-1, 1),
      advance = function(self, time, delta) {
        rotate_local(self, angle = 720 * delta)
        if (time < 0.5) {
          self@color <- lerp_colors("purple", "yellow", time / 0.5)
        } else {
          self@color <- lerp_colors("yellow", "purple", (time - 0.5) / 0.5)
        }
      })
  })@
child(shape, pos(5, -4.1))

render_frame(s1, -10:10, -10:10)

for (i in seq_len(1000)) {
  s1@advance(time = i / 1000, delta = 1 / 1000)
  Sys.sleep(0.008)
  render_frame(s1, -10:10, -10:10)
  # dev.off()
}
img_files <- list.files(path = "frames", pattern = "frame_.*\\.png", full.names = TRUE)
magick::image_read(img_files) |>
  magick::image_animate(fps = 100) |>
  magick::image_write("animation2.gif")
