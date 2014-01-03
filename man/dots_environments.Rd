\name{dots_environments}
\alias{dots_environments}
\alias{environments}
\alias{environments....}
\alias{environments<-}
\alias{environments<-....}
\title{Extract or manipulate environments contained in dots lists.}
\usage{
dots_environments(...)

environments(x)

\method{environments}{...}(x)

environments(x) <- value

\method{environments}{...}(x) <- value
}
\arguments{
  \item{...}{Any arguments.}

  \item{value}{A list of environments to apply.}

  \item{x}{a \{code{\link{dots}} object.}

  \item{value}{A new list of environments to apply.}
}
\value{
A named list of environment objects. The mutator
\code{environments<-} constructs a new list of unevaluated
promises with the same expressions but different
environments.
}
\description{
\code{environments} works on a dots list created by
\{code{\link{dots}} w, while \code{dots_environments} works
on arguments you pass in.#'
}

