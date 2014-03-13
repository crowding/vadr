\name{arg_env}
\alias{arg_env}
\alias{arg_expr}
\title{Get environment or expression from a named argument.}
\usage{
arg_env(name, envir = arg_env(name, environment()))

arg_expr(name, envir = arg_env(name, environment()))
}
\arguments{
  \item{name}{A single argument name; not evaluated.}
}
\description{
\code{arg_env} determines the lexical scope of an argument
(which must be an un-evaluated promise).

\code{arg_expr} fetches the expression attached to an
argument in the given environment. The effect is similar to
\code{substitute(name)} but more specific.
}

