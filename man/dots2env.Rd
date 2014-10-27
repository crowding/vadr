\name{dots2env}
\alias{dots2env}
\title{Convert an a ... object into an environment, without forcing promises.}
\usage{
dots2env(dots, envir = NULL, parent = arg_env(dots, environment()),
  hash = (length(dots) > 100), size = max(29L, length(dots)))
}
\arguments{
  \item{dots}{The dots object to convert.}

  \item{envir}{if not NULL, an environment to populate. If
  NULL, a new environment will be created.}

  \item{parent}{If creating a new environment, the parent
  to use.}

  \item{size}{The size of the new environment.}

  \item{hash}{Whether the new environment should use a
  hashtable.}
}
\value{
An environment object.
}
\description{
All named entries in the dots object will be bound to
variables in the given environment. Unnamed entries will be
appended to any existing value of `...` in the order in
which they appear. Promises will not be forced.
}

