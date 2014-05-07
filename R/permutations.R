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
  x_vals_per_g <- tapply(x, g, unique, simplify=F)
  # Check that x has one unique value for each value of g. If not, raise an 
  # error with an informative message.
  if (!all(sapply(x_vals_per_g, length) == 1)) {
    stop(
      "Multiple values detected within grouping factor.\n\n", 
      "  Value of g: value(s) of x\n",
      "  -----------------------\n",
      paste0("  ", names(x_vals_per_g), ": ", x_vals_per_g, collapse="\n"))
  }
  # Values of x for each value of g.
  x_per_g <- unlist(x_vals_per_g)
  # Shuffled values of x for each value of g.
  x_per_g_shuffled <- sample(x_per_g)
  # Names are also shuffled, but we want to associate shuffled values 
  # with the original names.
  names(x_per_g_shuffled) <- names(x_per_g)
  # Now we need to get the new values of x for each value of g.  The unique
  # values of g are stored in the names, the shuffled values of x are stored 
  # in the vector x_per_g_shuffled.
  x_per_g_shuffled[as.character(g)]
}

#' Adonis for nested experimental designs
#' 
#' Variables within the model, as well as the block-level variable, must be in 
#' the data frame provided.
#' 
#' @param formula The model to be tested.
#' @param data The data frame within which the model is evaluated.
#' @param block_var The name of the variable defining the nested groups.
#' @param nperm Number of permutations to run.
#' @return An object describing the fit, similar to the output of `lm`.
#' @export
block_adonis <- function(formula, data, block_var, nperm=999) {
  
  # Save the LHS in the current environment.  Otherwise, adonis raises 
  # an error when it tries to look up the variable in the parent frame.
  lhs_name <- deparse(formula[[2]])
  assign(lhs_name, eval(formula[[2]], data, parent.frame()))
  
  # All variables as an unevaluated list
  vars <- attr(terms(formula), "variables")
  # Method used by model.frame to get variable names
  var_names <- sapply(vars, function (v) paste(deparse(v, width.cutoff=500)))
  # First element is the call to `list`, second element is LHS
  # Remove both to get variable names on RHS
  rhs_names <- var_names[-c(1, 2)]
  
  # Total number of terms in the model
  # We grab this from the number of term labels, but could also use the number 
  # of columns in the "factors" attribute, which stores a matrix of variables 
  # by terms.  We can't use the "factors" attribute for the variable names, 
  # however, because it doesn't remove backticks.
  nterms <- length(attr(terms(formula), "term.labels"))
  
  # Initial result
  # We need to run permutations for at least the number of terms or adonis 
  # raises an error.
  res0 <- adonis(formula, data=data, permutations=nterms)
  
  # Get the column of statistics from a test result 
  get_stats <- function (test_result) {
    test_result$aov.tab[1:nterms,4]
  }
  
  # Observed value of statistics
  f0 <- get_stats(res0)
  
  # Values of F statistics after permutations
  f_perm <- replicate(nperm, {
    data1 <- data
    for (v in rhs_names) {
      data1[[v]] <- shuffle_between_groups(data1[[v]], data1[[block_var]])
    }
    res1 <- adonis(formula, data=data1, permutations=nterms)
    get_stats(res1)
  })

  # Compute the p-values
  # The functions sweep and apply work differently if a vector is provided 
  # (nterms = 1) rather than a matrix (nterms > 1), so we have to handle the 
  # two cases separately.
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
