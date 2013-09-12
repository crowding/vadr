\name{qq}
\alias{qe}
\alias{qq}
\alias{template}
\title{Quasiquotation. Perform template substitutions on a quoted R expressions.}
\usage{
  qq(...)
}
\arguments{
  \item{expr}{A language object.}
}
\value{
  For \code{qq}, A language object; for \code{qe},
  evaluates the expression in the calling environment.
}
\description{
  This is an extended version of the \code{\link{bquote}}
  utility.  'qq' quotes its first argument, then scans for
  terms wrapped in \code{.()}, \code{..()}, or names that
  match \code{`.()`} The wrapped expressions or names are
  evaluated in the given environment.  Expressions wrapped
  in \code{..()} will be interpolated into the argument
  list in which they occur. Names wrapped in `.()` will be
  substituted and coerced to name.
}
\details{
  Invocations of qq() can be nested within the \code{.()}
  sections and they should work as promised.
}
\examples{
#Basic substitution
default <- 1
qq( function(x, y = .(default)) x+y )
#function(x, y = 1) x + y

# splicing substitution:
paste.before <- alist("hello", "cool")
paste.after <- alist("!", "Now is", date())
qq(cat(...(paste.before), "world", ...(paste.after), '\\n'))
#cat("hello", "cool", "world", "!", "Now is", date(), "\\n")

# Name substitution:
element_to_access <- "x"
qq(function(data) data$`.(element_to_access)`)
#function(data) data$x

argument.name <- "x"
qq(
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
function(...(list(x=1, y=missing_value()))) x+y

# Building a function with an arbitrary list of arguments:
argnames <- letters[1:4]
qq(function(.=...(setNames(missing_value(length(argnames)), argnames))) {
  list(...(lapply(argnames, as.name)))
})
#function(a, b, c, d) list(a, b, c, d)

#' The poor .() function is overloaded. You can escape it thus:
dfname <- "baseball"
qq(ddply(.(as.name(dfname)), .(quote(.(id, team))), identity))
#ddply(baseball, .(id.team), identity)
}
\author{
  Peter Meilstrup
}
\seealso{
  macro bquote substitute quoting.env
}

