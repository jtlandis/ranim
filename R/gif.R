#' Compute total animation duration
#'
#' This helper repeatedly advances an object until no actions remain,
#' summing the time consumed at each step. It is mainly used by
#' [`render_gif()`] when `calc = TRUE` to pre-calculate the exact
#' number of frames needed.
#'
#' @param obj A [`shape`] object to analyze.
#'
#' @return Numeric duration in time units.
#'
#' @examples
#' # Create a simple animated shape
#' w <- window(bl = 0, tr = pos(10, 10))
#' w@action(translate(to = pos(5, 5), time = time(2)))
#' duration <- calc_duration(w)
#'
#' @seealso [`render_gif()`]
#'
calc_duration <- function(obj) {
  obj <- obj_clone(obj)
  time_step <- obj@left
  i <- time_step
  while (obj@act(time_step)) {
    time_step <- obj@left
    i <- i + time_step
  }
  i
}

#' Render an animation as a GIF
#'
#' `render_gif()` advances an animated [`shape`] over time, rendering
#' each frame to a PNG, and then combines the frames into an animated
#' GIF using the **magick** package.
#'
#' @param obj A [`shape`] object to animate.
#' @param file Output file path for the GIF.
#' @param fps Frames per second for the output GIF (and for internal
#'   rendering if `obj` uses FPS-based timing).
#' @param keep_frames If `TRUE`, keep the intermediate PNG frames
#'   in a temporary directory; otherwise they are deleted after
#'   creating the GIF.
#' @param quite Logical; if `FALSE` (default), progress is printed
#'   to the console. (Note: argument is spelled `quite` for
#'   historical reasons and may be renamed in future versions.)
#' @param width,height Size of the graphics device. Default 6 inches
#'   by 4 inches.
#' @param units Units for `width` and `height`. Can be `"in"` (inches),
#'   `"px"` (pixels), `"cm"`, or `"mm"`. Passed to [grDevices::png()].
#' @param dpi Resolution in dots per inch, used when `units` is not
#'   `"px"`. Default 100.
#' @param loop Number of times the GIF should loop. `0` means loop
#'   infinitely; pass a positive integer to loop that many times.
#' @param clone If `TRUE` (default), work on a deep copy of `obj`;
#'   this preserves the original object state. If `FALSE`, `obj` is
#'   modified in place.
#' @param calc If `TRUE`, first compute the exact duration of the
#'   animation via [`calc_duration()`] and preallocate the correct
#'   number of frames. If `FALSE` (default), render up to a maximum
#'   of 1000 frames.
#'
#' @return The animated `shape` (possibly a clone), invisibly.
#'
#' @details
#' The function creates a temporary directory, renders each frame
#' of the animation as a PNG file, then uses **magick** to combine
#' them into a GIF. On exit, temporary PNG files are deleted
#' (unless `keep_frames = TRUE`).
#'
#' @examples
#' \dontrun{
#' # Create a simple animation
#' w <- window(bl = 0, tr = pos(10, 10))
#' w@action(translate(to = pos(5, 5), time = time(2)))
#'
#' # Render to GIF
#' render_gif(w, "animation.gif", fps = 25, width = 8, height = 6)
#' }
#'
#' @seealso [`anim()`], [`render()`]
#'
#' @export
render_gif <- function(
  obj, file, fps = 100, keep_frames = FALSE,
  quite = FALSE,
  width = 6, height = 4, units = "in",
  dpi = 100,
  loop = 0,
  clone = TRUE,
  calc = FALSE
) {
  if (clone) {
    obj <- obj_clone(obj)
  }

  clock <- simple_clock(delta_time = 1 / fps)

  tmp <- tempdir(TRUE)
  total_frames <- if (calc) {
    full_dur <- calc_duration(obj)
    (fps * full_dur) + 2L
  } else {
    1000
  }
  nzero <- floor(log10(total_frames)) + 1L
  template <- sprintf("%s/frame_%%0%ii.png", tmp, nzero)
  report_template <- sprintf(
    "rendering frame %%0%ii\r",
    nzero
  )
  if (length(.files <- list.files(
    path = tmp,
    pattern = "frame_.*\\.png", full.names = TRUE
  ))) {
    file.remove(.files)
  }
  if (!keep_frames) {
    on.exit(file.remove(img_files))
  } else {
    message(
      sprintf("frames for %s are at %s", file, tmp)
    )
  }
  i <- 0L
  if (!quite) cat(sprintf(report_template, i))
  png(sprintf(template, i),
    height = height, width = width,
    res = dpi, units = units
  )
  render(obj)
  dev.off()
  i <- i + 1L
  while (obj@act(clock@delta_time)) {
    if (!quite) cat(sprintf(report_template, i))
    png(sprintf(template, i), height = height, width = width, res = dpi, units = units)
    render(obj)
    dev.off()
    i <- i + 1L
  }
  if (!quite) cat(sprintf(report_template, i))
  png(sprintf(template, i),
    height = height, width = width,
    res = dpi, units = units
  )
  render(obj)
  dev.off()
  i <- i + 1L
  if (!quite) cat("\ncompiling gif...")
  img_files <- list.files(
    path = tmp,
    pattern = "frame_.*\\.png", full.names = TRUE
  )

  magick::image_read(img_files) |>
    magick::image_animate(fps = fps, loop = loop) |>
    magick::image_write(file)
  if (!quite) cat(" done!\n")
  invisible(obj)
}
