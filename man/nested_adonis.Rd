\name{nested_adonis}
\alias{nested_adonis}
\title{Adonis for nested experimental designs}
\usage{
nested_adonis(formula, data, block_var, between_block_vars = c(),
  within_block_vars = c(), nperm = 999)
}
\arguments{
  \item{formula}{The model to be tested.}

  \item{data}{The data frame within which the model is
  evaluated.}

  \item{block_var}{The variable defining the nested
  groups.}

  \item{between_block_vars}{Variables that are constant
  within each block.}

  \item{within_block_vars}{Variables that vary within each
  block.}

  \item{nperm}{Number of permutations to run.}
}
\value{
An object describing the fit, similar to the output of
`lm`.
}
\description{
Adonis for nested experimental designs
}

