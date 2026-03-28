.onLoad <- function(libname, pkgname) {
  # Standard package load hook.
  #
  # We intentionally keep this minimal and rely on the NAMESPACE
  # to import required symbols from S7 and other dependencies.
  #
  # If you need package-wide initialisation in the future (e.g.,
  # registering S3/S4 methods, setting options, etc.), this is the
  # appropriate place to do it.
  vctrs::s3_register("base::format", "pos")
  invisible()
}

.onAttach <- function(libname, pkgname) {
  # Quiet startup by default. Uncomment to add a startup message:
  #
  # packageStartupMessage("Loaded 'ranim' (hierarchical shape-based animations).")

  invisible()
}
