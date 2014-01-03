\name{bind}
\alias{<-.bind}
\alias{[<-.bind}
\alias{bind}
\alias{bind<-}
\title{Unpack a list and assign to multiple variables.}
\format{N/A}
\usage{
bind[key=varName, ...] <- list(key=value, ...)
}
\arguments{
  \item{...}{a list of assignments, key on left, target
  variable on right. That is, \code{bind[a=x] <- c(a=1)}
  creates a variable named \code{x}, not \code{a}. It is
  somewhat counterintuitive but this is the only way that
  matches R's argument binding syntax.}

  \item{*envir*}{The environment to bind in (defaults to
  the caller).}
}
\value{
a "bind" object, since it is invoked via a subset on
"bind".
}
\description{
This is a "destructuring bind" for R. It can be used to
unpack structured lists into different variables, or
achieve the effect of multiple return values from a
function.
}
\details{
Element to variable matching should match R's argument
binding rules, with the modification that arguments to the
right of the ... will be matched positionally to elements
at the end of the unpacked sequence. Calls to bind() can be
nested to unpack nested structures.

You may leave an argument blank as in \code{bind[,
skipKey=, ...=rest] <- seq} to skip an element. (Here the
first element of \code{seq} and the one tagged "skipKey"
are both skipped and the rest are gathered in the output
variable \code{rest}.)

Note that the assigned-to variable is on the \emph{right}
side of each \code{=} in the argument list. This is
admittedly awkward but is the best way to remain consistent
with R's argument-binding semantics.
}
\note{
This will incidentally create a local variable named "bind"
in your environment. On the other hand if you have an
object already named "bind" and not of class "bind" this
method won't be found, so it's merely annoying and not
destructive. It's not clear how to avoid this and still use
an assignment operator to do the binding. (I could write a
simple function, but I strongly prefer there to be a
\code{<-} anywhere that there is a modification to the
environment.)

Nonlocal assignments (\code{<<-}) are not supported and
will behave as local assignments.
}
\examples{
#match by position
bind[x, y] <- c("foo", "bar")

#match by name
bind[a=x, b=y] <- c(b="bar", a="foo")

# one often wants to unpack the first and/or last, and rest of a list.
bind[first, ...=rest, last] <- letters

record <- list("Marilyn", "Monroe", dob=list("June", 1, 1926),
               profession="film star", "born Norma Jean Baker",
               donotuse="garbage", "1947 California Artichoke Queen",
               list("August", 5, 1962))
bind[first, last,
     dob=bind[month, day, year],
     donotuse=, ...=notes, death] <- record
}
\author{
Peter Meilstrup
}

