label_prop <- new_property(
  class = class_character,
  validator = function(value) {
    if (length(value) != 1L) {
      return("expected length 1")
    }
    NULL
  }
)

.fonts <- c("plain", "bold", "italic", "emphasis")
font_prop <- new_property(
  class = class_character,
  validator = function(value) {
    if (length(value) != 1L) {
      return("expected length 1")
    }
    m <- match(value, .fonts, nomatch = 0L)
    if (m == 0L) {
      return(
        sprintf("should be one of %s", paste0(.fonts, collapse = ", "))
      )
    }
    NULL
  }
)

#' Text shape
#'
#' `text()` creates a text shape object. Text is rendered as labels at
#' a specified position, with control over font, size, and color.
#'
#' @param label Character string; the text to display.
#' @param trans A [`transform`] object giving position, size, and angle.
#'   Defaults to `transform()` (origin, size 1, no rotation).
#'   - `trans@size` controls the character expansion (`cex`)
#'   - `trans@angle` controls text rotation (`srt`)
#'   - `trans@global` specifies the (x, y) position
#' @param parent An optional parent [`shape`]. If provided, this shape
#'   becomes a child of the parent.
#' @param children Optional list of child [`shape`] objects.
#' @param actions Optional list of [`action`] objects to apply.
#' @param color A color specification (see [`class_color`]). Default `"black"`.
#' @param hadj Horizontal text justification; 0 = left, 0.5 = center, 1 = right.
#'   Default 0.5 (centered).
#' @param vadj Vertical text justification; 0 = bottom, 0.5 = center, 1 = top.
#'   Default 0.5 (centered).
#' @param font Font style: one of `"plain"`, `"bold"`, `"italic"`, `"emphasis"`.
#'   Default `"plain"`.
#'
#' @section Properties:
#' * `label`: Character string; the text content.
#' * `hadj`: Horizontal adjustment (0-1).
#' * `vadj`: Vertical adjustment (0-1).
#' * `font`: Font style.
#' * All properties inherited from [`shape`]: `trans`, `parent`,
#'   `children`, `actions`, `color`, etc.
#'
#' @return An object of class `text` (inherits from [`shape`]).
#'
#' @examples
#' # Create text at (5, 5)
#' t <- text(
#'   label = "Hello",
#'   color = "blue",
#'   trans = transform(pos(5, 5))
#' )
#'
#' # Larger, bold text
#' t <- text(
#'   label = "World",
#'   font = "bold",
#'   trans = transform(pos(5, 5), size = 2)
#' )
#'
#' @seealso [`rect()`], [`point()`], [`polygon()`], [`shape`]
#'
#' @export
text <- new_class(
  "text",
  # trans@size -> cex
  # trans@angle -> srt
  # trans@global -> pos(x, y)
  #
  parent = shape,
  properties = list(
    label = label_prop,
    hadj = scalar_num_prop,
    vadj = scalar_num_prop,
    font = font_prop
  ),
  constructor = function(
    label,
    trans = transform(),
    parent = NULL,
    children = list(),
    actions = list(),
    color = "black",
    hadj = 0.5,
    vadj = 0.5,
    font = "plain"
  ) {
    new_object(
      shape(
        trans = trans, parent = parent, children = children,
        actions = actions, color = color
      ),
      label = label,
      hadj = hadj,
      vadj = vadj,
      font = font,
      family = new_property(
        class = new_union(NULL, class_character),
        validator = function(value) {
          if (is.character(value) && length(value) != 1L) {
            return("should be length 1")
          }
          NULL
        }
      )
    )
  }
)


#' @export
method(render, text) <- function(shape) {
  trans <- shape@trans
  pos <- trans@global

  text(
    x = vctrs::field(pos, "x"),
    y = vctrs::field(pos, "y"),
    labels = shape@label,
    cex = trans@size,
    srt = trans@angle,
    adj = c(shape@hadj, shape@vadj),
    col = shape@color,
    font = shape@font,
    family = shape@family %||% par("family")
  )
  if (length(shape@children)) {
    for (child in shape@children) {
      render(child)
    }
  }
  invisible(shape)
}
