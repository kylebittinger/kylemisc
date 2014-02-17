\name{remove_levels}
\alias{remove_levels}
\title{Remove specified levels from a factor}
\usage{
remove_levels(x, levels_to_remove)
}
\arguments{
  \item{x}{Factor variable}

  \item{levels_to_remove}{Character vector of levels to
  remove.}
}
\value{
A factor with the specified levels removed.
}
\description{
Like droplevels, but only deletes specified levels.  If
values are set to these levels, they are converted to NA.
}

