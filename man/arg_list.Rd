\name{get_dots}
\alias{[.arg_list}
\alias{arg_env}
\alias{arg_expr}
\alias{get_dots}
\title{...}
\usage{
get_dots(names, envir = argenv(names))

arg_list[envir](...)

arg_env(name, envir = arg_env(name, environment()))

arg_expr(name, envir = arg_env(name, environment()))
}
\arguments{
  \item{names}{A character vector or list of names.}

  \item{obj}{N/A.}

  \item{envir}{Where to look for the named arguments.}

  \item{name}{A single argument name; not evaluated.}
}
\description{
\code{arg_get} fetches arguments from a named list.

arg_list may be told to look in a particular environment by
supplying the environment in brackets.

\code{arg_env} determines the lexical scope of an argument
(which must be an un-evaluated promise).

\code{arg_expr} fetches the expression attached to an
argument in the given environment. The effect is similar to
\code{substitute(name)} but more specific.
}

