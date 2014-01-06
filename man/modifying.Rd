\name{put}
\alias{alter}
\alias{inject}
\alias{put}
\title{Modify part of a value.}
\usage{
put(it, subset, value)

alter(it, subset, ...)

inject(it, subset, ...)
}
\arguments{
  \item{it}{A value.}

  \item{subset}{A subassignment target expression; this is
  interpreted literally if the symbol \code{it} is used,
  otherwise \code{it} is injected as far down the leftmost
  arguments of the expression as possible. (Thus
  \code{names} is interpreted as \code{names(it)}, and
  \code{names[1]} as \code{names(it)[1]}.)}

  \item{value}{The value to assign}

  \item{...}{A \code{\link{chain}} of code
  transformations.}
}
\value{
The modified value.
}
\description{
The macro \code{put} provides syntax for modifying part of
an object in a functional context (i.e. creating a modified
value without necessarily modifying without binding the
result to a name.) Unlike \code{\link{<-}}, the value of
the expression is the modified object, not the value that
was injected. This is particularly useful in combination
with 'chain.'

\code{alter} takes the selected subset of \code{it}, then
filters it through additional functions in the manner of
\code{\link{chain}}, then replaces the subset with the
result, returning the modified object.

\code{inject} takes the entire object, filters it through a
chain, then places the result in the specified subset.
}
\details{
Normal subassignment in R is effectively a macro, one which
turns a statement like

\code{names(x)[1] <- "head"}

into something like

\code{x <- `names<-`(x, `[<-`(names(x), "head", 1))}

However even this explanation is misleading, because the
value returned from a subassignment is the value applied,
not the value assigned. Consider if you wanted to call a
function with a modification of an existing value:

\code{do_something(names(x)[1] <- "head")}

Aside from changing the value of \code{x} this actually
doesn't pass the value to \code{do_something}, but rather
performs the equivalent of \code{do_something("head")}.

In this situation, using \code{put}, one can write:

\code{do_something(put(x, names[1], "head"))}

code{put} and friends are particularly useful in
conjunction with \code{\link{chain}}.

\code{x \%<~\% alter(names[5], toupper)} is equivalent to:

\code{names(x)[5] <- toupper(names(x)[5])}

\code{x <- inject(1:10, names, letters[.], toupper)} is
equivalent to:

\code{x <- 1:10; names(x) <- toupper(letters[x])}
}
\examples{
put(1:10, names, letters[1:10])
x <- 1:10
put(x, it[1], 4)
put(x, names[4], 'a')
x #x is unchanged
x <- alter(structure(1:10, names=letters[1:10]), names)
y <- alter(x, names[5], toupper, str_dup(3))
x <- inject(1:10, names[1:5], letters[.], rev)
}
\author{
Peter Meilstrup
}
\seealso{
chain %<~%
}

