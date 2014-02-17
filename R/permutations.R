#' Apply a function over a list or vector using names
#' 
#' @param X A list or vector.
#' @param FUN A function to be applied to each element.  The first two 
#'   arguments provided to `FUN` are the element name and element value.
#' @return A list with names like X.
#' @export
lapply_names <- function (X, FUN, ...) {
  mapply(FUN, names(X), X, ..., SIMPLIFY=F)
}

#' Shuffle a vector within each group
#'
#' @param x The vector to be shuffled.
#' @param g A vector of groups.
#' @return A version of x where elements have been 
#'   shuffled within each group.
#' @export
shuffle_within_groups <- function (x, g) {
  ave(x, g, FUN=sample)
}

#' Shuffle a vector, but swap whole groups
#' @param x The vector to be shuffled.
#' @param g A vector of groups.
#' @return A version of x where elements have been 
#'   shuffled with each group.
#' @export
shuffle_between_groups <- function (x, g) {
  g <- as.factor(g)
  x <- as.factor(x)
  # What is the value of x for every unique value of g?
  x_per_g <- tapply(x, g, `[`, 1)
  # This is what we want to shuffle.
  x_per_g_shuffled <- sample(x_per_g)
  # Names are also shuffled, but we want to associate shuffled values 
  # with the original names.
  names(x_per_g_shuffled) <- names(x_per_g)
  # Now we need to get the new values of x for each value of g.  The unique
  # values of g are stored in the names, the shuffled values of x are stored 
  # in the vector x_per_g_shuffled.
  new_x <- x_per_g_shuffled[as.character(g)]
  # The new x is a numeric vector, cast back to a factor.
  factor(new_x, labels=levels(x))
}

#' Adonis for nested experimental designs
#' 
#' @param formula The model to be tested.
#' @param data The data frame within which the model is evaluated.
#' @param block_var The variable defining the nested groups.
#' @param between_block_vars Variables that are constant within each block.
#' @param within_block_vars Variables that vary within each block.
#' @param nterms The total number of terms in the model.  I could figure this 
#'   out from the formula, but I did not want to spend time implementing this 
#'   feature, so you must provide the number of terms manually.  Sorry.
#' @param nperm Number of permutations to run.
#' @return An object describing the fit, similar to the output of `lm`.
#' @export
block_adonis <- function(
  formula, data, block_var, 
  between_block_vars=c(), within_block_vars=c(), 
  nterms=1, nperm=9999) {
  
  # Initial result, without permutations
  res0 <- adonis(formula, data=data, permutations=nterms)
  
  # Get the column of statistics from an analysis of variance 
  # table provided in a set of test results 
  get_stats <- function (test_result) {
    test_result$aov.tab[1:nterms,4]
  }
  
  # Observed value of statistics
  f0 <- get_stats(res0)
  
  # Values of statistics after permutations
  f_perm <- replicate(nperm, {
    data1 <- data
    for (v in between_block_vars) {
      data1[[v]] <- shuffle_between_groups(data1[[v]], data1[[block_var]])
    }
    for (v in within_block_vars) {
      data1[[v]] <- shuffle_within_groups(data1[[v]], data1[[block_var]])
    }
    res1 <- adonis(formula, data=data1, permutations=nterms)
    get_stats(res1)
  })

  # Compute the p-values
  # The functions sweep and apply work differently if a vector is provided 
  # rather than a matrix, so we have to handle the two cases separately.
  if (nterms > 1) {
    f_greater <- sweep(cbind(f_perm, f0), 1, f0, ">=")
    pvals <- apply(f_greater, 1, function (x) sum(x) / length(x))
  } else {
    f_greater <- c(f_perm, f0) >= f0
    pvals <- sum(f_greater) / length(f_greater)
  }
  
  # Insert new p-values into original results
  res0$aov.tab[1:nterms,6] <- pvals
  res0
}
