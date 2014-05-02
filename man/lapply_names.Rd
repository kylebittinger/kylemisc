\name{lapply_names}
\alias{lapply_names}
\title{Apply a function over a list or vector using names}
\usage{
lapply_names(X, FUN, ...)
}
\arguments{
  \item{X}{A list or vector.}

  \item{FUN}{A function to be applied to each element.  The
  first two arguments provided to `FUN` are the element
  name and element value.}

  \item{...}{Additional arguments to FUN.}
}
\value{
A list with names like X.
}
\description{
Apply a function over a list or vector using names
}

