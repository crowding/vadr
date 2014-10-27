\name{env2dots}
\alias{env2dots}
\title{Convert an environment into a ... object, without forcing promises.}
\usage{
env2dots(envir, include_missing = FALSE)
}
\arguments{
  \item{envir}{An environment.}

  \item{include_missing}{Whether to include "missing"
  bindings in the dotslist.}
}
\value{
A \link{dots} object.
}
\description{
All bindings in the environment will be copied into a new
\code{\dots} list. Bindings that are promises will be added
to the \dots list without forcing, while other bindings
will be wrapped in an already-evaluated promise.  If `...`
exists in the environment, all bindings it contains will be
added to the \dots list. The output will not be in any
particular order.
}

