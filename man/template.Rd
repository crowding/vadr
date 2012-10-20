\name{template}
\alias{template}
\title{Perform template substitutions on a quoted R expressions.}
\usage{
  template(expr, .envir = parent.frame())
}
\arguments{
  \item{expr}{A language object.}

  \item{.envir}{An environment to evaluate the backquoted
  expressions in.}
}
\value{
  A language object.
}
\description{
  This is an extended version of the
  \code{\link{backquote}} utility.  'template' quotes its
  first argument, then scans for terms erapped in
  \code{.()}, \code{...()}, or names that match
  \code{`.()`} The wrapped expressions or names are
  evaluated in the given environment.  Expressions wrapped
  in \code{...()} will be interpolated into the argument
  list in which they occur. Names wrapped in `.()` will be
  substituted and coerced to name.
}
\details{
  The contents of a name matching `.( )` will be parsed and

  This can used recursively to build interesting code
  transformations.
}
\examples{
#Basic substitution
default <- 1
template( function(x, y = .(default)) x+y )
#function(x, y = 1) x + y

# interpolating substitution:
paste.before <- alist("hello", "cool")
paste.after <- alist("!", "Now is", date())
template(cat(...(paste.before), "world", ...(paste.after), '\\n'))
#cat("hello", "cool", "world", "!", "Now is", date(), "\\n")

# Name substitution:
element_to_access <- "x"
template(function(data) data$`.(element_to_access)`)
#function(data) data$x

argument.name <- "x"
template(
  function(`.(argument.name)`)
  cat(.(argument.name), " is ", `.(argument.name)`, "\\n")
)
#function(x) cat("x", " is ", x, "\\n"))

# Note that in the argument list to a function, function argument
# names are names, and defaults are values; that is
function(x=1, y) x+y
# is equivalent to
function(...(alist(x=1, y=))) x+y
# or
function(...(list(x=1, y=missing.value()))) x+y

# Building a function with an arbitrary list of arguments:
argnames <- letters[1:4]
template(function(.=...(setNames(missing.value(length(argnames)), argnames))) {
  list(...(lapply(argnames, as.name)))
})
#function(a, b, c, d) list(a, b, c, d)

#' The poor .() function is overloaded. You can escape it thus:
dfname <- "baseball"
template(ddply(.(as.name(dfname)), .(quote(.(id, team))), identity))
#ddply(baseball, .(id.team), identity)
}
\author{
  Peter Meilstrup
}
\seealso{
  macro bquote substitute recpitulating.env
}

