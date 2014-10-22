\name{chain}
\alias{[}
\alias{[.chain}
\alias{[.mkchain}
\alias{\%|>\%}
\alias{chain}
\alias{mkchain}
\title{Chain the output of one expression into the input of another.}
\usage{
mkchain[...](...)

chain[...](., ...)

mkchain(...)

chain(., ...)

. \%|>\% func
}
\arguments{
  \item{.}{For \code{chain} the first parameter in
  parentheses is the data to run through the chain.}

  \item{...}{Parameters in square brackets give the
  placeholder name and default arguments. Parameters in
  parentheses are function names or partial calls.}

  \item{func}{\code{\%|>\%} is a shortcut for a chain of
  one step.}
}
\value{
For \code{mkchain} return the constructed function. For
\code{chain}, apply the chain to the dataset given in the
first argument and return the result.
}
\description{
\code{chain} provides a different way to write computations
that pass a value through a chain of transformations.
}
\details{
For instance, suppose that you have a path \code{P} defined
by a M-by-2 array of coordinates and you want to find the
total length of the line segments connecting each point in
sequence. My stream of thought for this goes something like
"okay, take the difference between rows, square, sum along
columns, square root, and sum." You could write:

\code{length <- sum(sqrt(rowSums(apply(P, 2, diff)^2)))}

However this must be read "inside-out" to follow the
computation. I find it easier to follow if written this
way:

\code{length <- chain(P, apply(2,diff), .^2, rowSums, sqrt,
sum)}

which can be read from left to right, noting that the
output of each expression becomes the input of the next.

Note that some arguments above are the names of functions,
and others are expressions. \code{chain} applies whichever
interpretation appears most appropriate: bare words are
taken to be functions, expressions containing the
placeholder name (by default \code{.}) evaluate to
expressions, and expressions that do not contain the
placeholder have a placeholder injected at the first
argument. Thus \code{apply(2,diff)} is interpreted as
\code{apply(.,2,diff)}, with the \code{.} coming from the
output of the previous step. This tends to work well
because of the typical convention in R of the dataset being
the first argument to any function. The above is equivalent
to:

\code{length <- chain(P, apply(.,2,diff), .^2, rowSums(.),
sqrt(.), sum(.))}

If you want to keep an intermediate value along the chain
for use, you can name the arguments, as in

\code{alphabetize <- mkchain(values=., names, order,
values[.])}.

You can also use a different placeholder than \code{"."} by
supplying it in brackets, as in \code{chain[x](x^2, mean,
sqrt)}. This is useful for nested invocations of
\code{\link{chain}} or if another package has a use for
\code{"."}. When used with \code{\link{mkchain}}, you can
specify other arguments and defaults, as in
\code{mkchain[., pow=2](.^pow, mean, .^(1/pow))}.

More than the occasional use of temporary names and
alternate placeholder names might indicate \code{chain} is
not helping clarity :)

Note that subassignments, for example \code{chain(letters,
names(.) <- toupper(.))} return the rvalue, which is not
usually what you want (here it will return the upcased
characters, not the object with upcased names.) Instead use
\code{\link{put}}, as in \code{ chain(letters, put(.,
names, toupper(.))}, or even better in this case,
\code{chain(letters, \link{inject}(names, toupper))}.
}
\note{
\code{chain} is a bit like the \code{->} macro of Clojure,
or the \code{|>} operator in Elixir.
}
\examples{
# In help("match_df", package="plyr") there is this example:
library(plyr)
data(baseball)

longterm <- subset(count(baseball, "id"), freq > 25)
bb_longterm <- match_df(baseball, longterm, on="id")
bb_longterm[1:5,]

# Rewriting the above using chain:
chain(b=baseball, count("id"), subset(freq>25),
      match_df(b, ., on="id"), head(5))
}
\author{
Peter Meilstrup
}
\seealso{
put
}

