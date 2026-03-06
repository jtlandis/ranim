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

text <- new_class(
  "text",
  # trans@size -> cex
  # trans@angle -> srt
  # trans@global -> pos(x, y)
  #
  parent = shape,
  properties = list(
    label = label_prop,
    hadj = scalar_num,
    vadj = scalar_num,
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
      hadj = scalar(hadj),
      vadj = scalar(vadj),
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



method(render, text) <- function(shape) {
  trans <- shape@trans
  text(
    x = trans@pos@x,
    y = trans@pos@y,
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
