\name{find_macros}
\alias{find_macros}
\title{List all macros.}
\usage{
find_macros(what, where = arg_env(what))
}
\arguments{
  \item{what}{a list of names to try. If not specified,
  searches all attached namespaces.}

  \item{where}{A frame to search.}
}
\value{
a list of macro functions, the list having names according
to the names they are bound to. If given, this overrides
both include.enclos and include.search.
}
\description{
Attempts to find all macros on the search path.
}
\author{
Peter Meilstrup
}

