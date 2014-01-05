\name{put}
\alias{<-}
\alias{That}
\alias{\%<~\%}
\alias{\code{\link{chain}},}
\alias{\code{alter}}
\alias{\code{it},}
\alias{\code{names(x)[5]}
\alias{\code{x}
\alias{additional}
\alias{alter}
\alias{alter(names[5],}
\alias{equivalent}
\alias{filters}
\alias{functions}
\alias{in}
\alias{is}
\alias{is,}
\alias{it}
\alias{manner}
\alias{modified}
\alias{object.}
\alias{of}
\alias{put}
\alias{replaces}
\alias{result,}
\alias{returning}
\alias{selected}
\alias{subset}
\alias{takes}
\alias{the}
\alias{then}
\alias{through}
\alias{to}
\alias{toupper(names(x)[5])}}
\alias{toupper)}}
\alias{with}
\title{Modify part of a value.}
\usage{
put(...)

alter(...)
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
}
\examples{
put(1:10, names, letters[1:10])
x <- 1:10
put(x, it[1], 4)
put(x, names[4], 'a')
x #x is unchanged
x <- alter(structure(1:10, names=letters[1:10]), names)
y <- alter(x, names[5], toupper, str_dup(3))
}
\author{
Peter Meilstrup
}
\seealso{
chain %<~%

chain
}

