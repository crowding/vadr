\name{mkchain}
\alias{[.chain}
\alias{[.mkchain}
\alias{mkchain}
\title{Chain the output of one expression into the input of another.}
\usage{
  mkchain(...)
}
\arguments{
  \item{.}{For \code{chain} the first parameter is the data
  to run through the chain.}

  \item{...}{The remainder of parameters in \code{chain}
  are a sequence of transforming expressions.}
}
\value{
  For \code{mkchain} return the constructed function. For
  \code{chain}, apply the chain to the dataset given in the
  first argument and return the result.
}
\description{
  Many times in R programming you will want to take a
  dataset and do a sqeuence of simple things to it.
  \code{chain} aims to make this kind of code simpler and
  more compact.
}
\details{
  For instance, suppose that you have a path \code{P}
  defined by a M-by-2 array of coordinates and you want to
  find the total length of the line segments connecting
  each point in sequence. My stream of though for this goes
  something like "okay , take the difference between rows,
  square, sum along columns, square root, and sum." You
  could write:

  \code{length <- sum(sqrt(rowSums(apply(P, 2, diff)^2)))}

  However this must be read "inside-out" to follow the
  computation. I find it easier to follow if written this
  way:

  \code{length <- chain(P, apply(2,diff), .^2, rowSums,
  sqrt, sum)}

  which can be read from left to right, noting that the
  output of each expression becomes the input of the next.

  Note that some arguments above are the names of
  functions, and others are expressions. \code{chain}
  applies whichever interpretation appears most
  appropriate: bare words are taken to be functions,
  expressions containing the placeholder name (by default
  \code{.}) evaluate to expressions, and expressions that
  do not contain the placeholder have a placeholder
  injected at the first argument. Thus \code{apply(2,diff)}
  is interpreted as \code{apply(.,2,diff)}, with the
  \code{.} coming from the output of the previous step.
  This tends to work well because of the typical convention
  in R of the dataset being the first argument to any
  function. The above is equivalent to:

  \code{length <- chain(P, apply(.,2,diff), .^2,
  rowSums(.), sqrt(.), sum(.))}

  If you want to keep an intermediate value along the chain
  for use, you can name the arguments, as in
  \code{alphabetize <- mkchain(values=., names, order,
  values[.])}.

  You can also use a different placeholder than \code{"."}
  by supplying it in brackets, as in \code{chain[x](x^2,
  mean, sqrt)}. This can make things less confusing for
  nested invocations of \code{\link{chain}} or if another
  package has a use for \code{"."}. When used with
  \code{link{mkchain}}, you can specify other arguments and
  defaults, as in \code{mkchain[., pow=2](x^pow, mean,
  sqrt)}

  Note that using subassignments, for example
  \code{chain(x, names(.) <- toupper(.))} usually return
  the rvalue, which is not usually what you want (here it
  will return the upcased characters, not the object with
  upcased names.) Currently you can cope with
  subassignments by saying things like \code{mkchain(data,
  `names<-`(., toupper(names(.))))}. Some better way to
  cope with subassignments would be nice.
}
\note{
  \code{chain} is a bit like the \code{->} macro of
  Clojure.
}
\examples{
# In help("match_df", package="plyr") there is this example:
data(baseball)

longterm <- subset(count(baseball, "id"), freq > 25)
bb_longterm <- match_df(baseball, longterm, on="id")
bb_longterm[1:5,]

# Rewriting the above using chain:
chain(baseball, count("id"), subset(freq>25),
      match_df(baseball, ., on="id"), head(5))
}
\author{
  Peter Meilstrup
}

