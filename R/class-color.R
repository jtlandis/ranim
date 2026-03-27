#' Color objects
#'
#' `class_color()` creates color objects that are validated and can be
#' used throughout the animation system. Colors can be specified as
#' hex strings (e.g., `"#FF0000"`), R color names (e.g., `"red"`),
#' or as numeric RGB(A) vectors.
#'
#' @param color A color specification. Can be:
#'   - A character string: hex code (starting with `"#"`) or named color
#'   - A numeric vector of length 3 (RGB, values 0-255)
#'   - A numeric vector of length 4 (RGBA, values 0-255)
#'
#' @return An object of class `color` (inherits from character).
#'
#' @details
#' Colors are validated to ensure they are either valid hex codes
#' or recognized R color names. Numeric inputs are converted to
#' hex via [grDevices::rgb()].
#'
#' @examples
#' # Named color
#' class_color("red")
#'
#' # Hex color
#' class_color("#FF0000")
#'
#' # RGB numeric (0-255)
#' class_color(c(255, 0, 0))
#'
#' # RGBA numeric
#' class_color(c(255, 0, 0, 128))
#'
#' @export
class_color <- S7::new_class(
  "color",
  parent = S7::class_character,
  validator = function(self) {
    if (length(self) != 1) {
      return("color must be a single character string")
    }
    if (substr(self, 1, 1) != "#" && !self %in% grDevices::colors()) {
      return("color must be a valid color name or hex code")
    }
    NULL
  },
  constructor = function(color) {
    if (is.numeric(color)) {
      if (length(color) == 3) {
        color <- rgb(color[1], color[2], color[3], maxColorValue = 255)
      } else if (length(color) == 4) {
        color <- rgb(color[1], color[2], color[3], alpha = color[4], maxColorValue = 255)
      } else {
        color <- rgb(color, maxColorValue = 255)
      }
    }
    if (!is.character(color)) {
      stop("color must be a character string")
    }
    S7::new_object(color)
  }
)


lerp_colors <- function(color1, color2, amount) {
  # Convert colors to RGB triplets (assuming hex format input)
  rgb1 <- col2rgb(color1, alpha = TRUE)
  rgb2 <- col2rgb(color2, alpha = TRUE)
  if (amount < 0 || amount > 1) {
    stop("amount must be between 0 and 1")
  }
  # Calculate interpolated RGB components
  # r_lerped <- rgb1[1] + (rgb2[1] - rgb1[1]) * amount
  # g_lerped <- rgb1[2] + (rgb2[2] - rgb1[2]) * amount
  # b_lerped <- rgb1[3] + (rgb2[3] - rgb1[3]) * amount
  lerped <- rgb1 + (rgb2 - rgb1) * amount
  # Combine and convert back to hex color string
  class_color(lerped)
}
