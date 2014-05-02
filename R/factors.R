#' Remove specified levels from a factor
#' 
#' Like droplevels, but only deletes specified levels.  If values are set to 
#' these levels, they are converted to NA.
#' 
#' @param x Factor variable
#' @param levels_to_remove Character vector of levels to remove.
#' @return A factor with the specified levels removed.
#' @export
remove_levels <- function (x, levels_to_remove) {
  x[x %in% levels_to_remove] <- NA
  new_levels <- levels(x)[!(levels(x) %in% levels_to_remove)]
  factor(x, levels=new_levels)
}


#' Rename levels of a factor
#' 
#' @param x Factor variable
#' @param orig_names Original level names
#' @param new_names New level names
#' @return A factor with the specified levels renamed.
#' @export
rename_levels <- function (x, orig_names, new_names) {
  idx <- match(orig_names, levels(x))
  levels(x)[idx] <- new_names
  x
}


#' Add new levels to a factor
#' 
#' @param x Factor variable
#' @param levels_to_add Character vector of new levels
#' @return A factor with the specified levels added.
#' @export
add_levels <- function (x, levels_to_add) {
  factor(x, levels=c(levels(x), levels_to_add))
}


#' Merge one factor into another, preserving order of levels
#' 
#' @param x The "dominant" factor, values from `x` will be used if they 
#'   are not NA.
#' @param y The factor to be merged into `x`, values from this factor will
#'   be used wherever `x` is NA.
#' @return The merged factor.
#' @export
merge_factors <- function (x, y) {
  xy <- as.character(x)
  xy[is.na(xy)] <- as.character(y)[is.na(xy)]
  levs <- c(levels(x), levels(y)[!(levels(y) %in% levels(x))])
  factor(xy, levels=levs)
}


#' Combine factors by pasting values
#' 
#' Needs work
#' 
#' @param x,y Factors to be combined.
#' @param sep Character string to separate the levels of x and y.
#' @return A new factor with values created by pasting the values of `x`
#'   and `y` together.
#' @export
paste_factors <- function (x, y, sep=" ") {
  xc <- as.character(x)
  yc <- as.character(y)
  y_empty <- is.na(y) | (yc == "")
  x_empty <- is.na(x) | (xc == "")
  factor(
    ifelse(
      y_empty, 
      xc, 
      ifelse(
        x_empty,
        yc,
        paste(xc, yc, sep=sep))))
}
