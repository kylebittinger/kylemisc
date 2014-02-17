#' Kyle's personal ggplot theme
#' 
#' Very similar to theme_classic, but removes some decoration from facet 
#' labels.
#' @export
theme_kyle <- function() {
  theme_classic() + theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12))
}