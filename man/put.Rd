\name{put}
\alias{put}
\title{Modify part of a value.}
\usage{
put(...)
}
\arguments{
  \item{it}{A value.}

  \item{subset}{An expression; this is interpreted
  literally if the symbol \code{it} is used, otherwise
  \code{it} is injected as far down the leftmost arguments
  of the expression as possible. (Thus \code{names} is
  interpreted as {names(it)}, and \code{names[1]} as
  \code{names(it)[1]}.)}
}
\value{
The modified value.
}
\description{
The macro \code{put} provides syntax for modifying part of
an object in a functional context (i.e. creating a modified
value without necessarily modifying without binding the
result to a name.) Unlike \code{link{<-}}, the value of the
expression is the modified object, not the value that was
injected. This is particularly useful in combination with
'chain.'
}
\details{
Normal subassignment in R is effectively a macro, one which
turns a statement like

\code{names(x)[1] <- "head"}

into something like

\code{x <- `names<-`(x, `[<-`(names(x), "head", 1))}

However even this explanation is misleading, because the
value returned from a subassignment is the value applied,
not the value assigned. Consider calling a function with a
modification of an existing value:

\code{do_something(names(x)[1] <- "head")}

This actually does the equivalent of
\code{do_something("head")}, rather than pass a modified
\code{x} to \code{do_something}.

Using \code{put}, one can write:

\code{do_something(put(x, names[1], "head"))}

\put() and friends are particularly useful in conjunction
with \code{\link{chain}}.
}
\examples{
put(1:10, names, letters[1:10])
x <- 1:10
put(x, it[1], 4)
put(x, names[4], 'a')
x #x is unchanged
}
\author{
Peter Meilstrup
}
\seealso{
chain
}

