render_gif <- function(
  obj, file, fps = 100, keep_frames = FALSE,
  quite = FALSE,
  width = 6, height = 4, units = "in",
  dpi = 100
) {
  obj <- obj_clone(obj)

  clock <- simple_clock(delta_time = 1 / fps)

  tmp <- tempdir(TRUE)
  full_dur <- remaining_time(obj)
  total_frames <- fps * full_dur
  nzero <- floor(log10(total_frames)) + 1L
  template <- sprintf("%s/frame_%%0%ii.png", tmp, nzero)
  report_template <- sprintf("rendering frame %%0%ii/%i\r", nzero, total_frames)
  if (!keep_frames) {
    on.exit(file.remove(img_files))
  } else {
    message(
      sprintf("frames for %s are at %s", file, tmp)
    )
  }
  i <- 1L
  while (obj@act(clock@delta_time)) {
    if (!quite) cat(sprintf(report_template, i))
    png(sprintf(template, i), height = height, width = width, res = dpi, units = units)
    render(obj)
    dev.off()
    i <- i + 1L
  }
  if (!quite) cat("\ncompiling gif...")
  img_files <- list.files(
    path = tmp,
    pattern = "frame_.*\\.png", full.names = TRUE
  )

  magick::image_read(img_files) |>
    magick::image_animate(fps = fps) |>
    magick::image_write(file)
  if (!quite) cat(" done!\n")
  invisible(obj)
}
