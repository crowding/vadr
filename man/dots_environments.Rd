\name{dots_environments}
\alias{dots_environments}
\alias{environments}
\alias{environments<-}
\title{Extract the environments attached to each item of a dots list.}
\usage{
dots_environments(...)

environments(x) <- value
}
\arguments{
  \item{...}{Any arguments.}

  \item{value}{A list of environments to apply.}

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
\{code{\link{dots}}, while \code{dots_environments} works
on arguments you pass in.#'
}

