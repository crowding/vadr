\name{dots_info}
\alias{dots_info}
\title{Show information about a dot-dot-dot object.}
\usage{
  dots_info(...)
}
\arguments{
  \item{...}{Any number of arguments. Usually, you will
  pass in the ... from the body of a function, e.g.
  \code{dots_info(...)}. Techinically this creates a copy
  of the dots list, but it should be identical.}
}
\value{
  A data frame, with one row for each element of
  \code{...}, and columns: \itemize{ \item{"name"}{The name
  of each argument, or "" if present.} \item{"envir"}{The
  enviroment the promise came from.} \item{"expr"}{The
  expression attached to the promise.} \item{"eval"}{TRUE
  if the promise has been evaluated.} \item{"value"}{The
  value attached to the promise, or NULL if not evaluated.}
  \item{"seen"}{TRUE if the promise has already been "seen"
  during promise evaluation. This flag is set by R during
  evaluation to detect cyclic references.} }
}
\description{
  This unpacks the contents of a dot-dot-dot (or
  \code{<...>}) object, returning the results in a data
  frame. A \code{<...>} object is a pairlist of promises,
  usually bound to the special name \code{"..."} and, when
  bound to that name, given special dispensation by the R
  interpreter when appearing in the argument list of a
  call. Dots objects are normally opaque to R code, but you
  can obtain a \code{<...>} inside a function by using
  \code{get("...")}.
}
\note{
  The "envir" column is a list of lists, because
  print.data.frame still can't handle a column that
  contains environments (even using \link{AsIs}).
}
\author{
  Peter Meilstrup
}
\seealso{
  dots_names dots_missing
}

