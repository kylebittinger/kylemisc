#' Apply a function over a list or vector using names
#' 
#' @param X A list or vector.
#' @param FUN A function to be applied to each element.  The first two 
#'   arguments provided to `FUN` are the element name and element value.
#'  @param ... Additional arguments to FUN.
#' @return A list with names like X.
#' @export
lapply_names <- function (X, FUN, ...) {
  mapply(FUN, names(X), X, ..., SIMPLIFY=F)
}
