\name{get_promise}
\alias{get_promise}
\alias{getpromise_in}
\title{Fetch promises bound to variables.}
\usage{
  getpromise(\dots) getpromise_in(envir)(\dots)
}
\arguments{
  \item{...}{Variable names (unevaluated). Arguments may be
  named; these names determine the names on the dots list
  (and not the variable names)}

  \item{envir}{for getpromise_in, the environment to search
  in.}
}
\value{
  a \code{\link{dots}} object containing the promises that
  are bound to those variables in the calling environment.
}
\description{
  Fetch promises bound to variables.
}
\note{
  The tags on the dots object are determined by argument
  names; variable names are discarded.
}
\author{
  Peter Meilstrup
}

