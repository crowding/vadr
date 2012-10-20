\name{mkchain}
\alias{chain}
\alias{mkchain}
\title{Chain the output of one expression into the input of another.}
\usage{
  mkchain(..., .dwim = TRUE, .envir = parent.frame())
}
\arguments{
  \item{data}{The data to run through the chain.}

  \item{...}{A sequence of expressions.}

  \item{.dwim}{If set to FALSE, will only evaluate
  expressions literally. If set to \code{TRUE} (the
  default), will attempt to guess what you mean. when you
  leave the dot out of the expressions, as described
  above.}

  \item{.envir}{The environement that should be parent of
  the returned function. Defaults to the parent frame of
  \code{mkchain}}
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
  more compact. For instance, suppose that you have a path
  \code{P} defined by a M-by-2 array of coordinates and you
  want to find the total length of the line segments
  connecting each point in sequence. You could write:
}
\details{
  \code{length <- sum(sqrt(rowSums(apply(P, 2, diff).^2)))}

  However this must be read "inside-out" to follow the
  computation. It will be easier to read if written this
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
  function.

  If the argument \code{.dwim} is set to FALSE, arguments
  are only interpreted as expressions with placeholders.
  Thus we could write:

  \code{length <- chain(P, apply(.,2,diff), .^2,
  rowSums(.), sqrt(.), sum(.))}

  Note that nested invocations of \code{chain} will almost
  never do what you want, as the placeholder will be
  interpreted by the outer chain before being interpreted
  by the inner chain. A facility for specifying the
  placeholder may be added in the future.

  It is also permissible to use an assignment like \code{x
  <- func(.)} to store an intermediate result along the
  chain. The assignment will happen in the function's
  private scope.

  Note that subassignments usually return the rvalue, which
  is not usually what you want in a chain. Currently you
  can cope with subassignments by saying things like
  \code{`[<-`(., index, value)}. Some better way to cope
  with subassignments is desirable.
}
\note{
  \code{chain} is a bit like the arrow monad of Haskell or
  the \code{->} macro of Clojure.
}
\examples{
# In help(match_df, package="plyr") there is this example:
data(baseball)
longterm <- subset(count(baseball, "id"), freq > 25)
bb_longterm <- match_df(baseball, longterm, on="id")
bb_longterm[1:5,]

# Rewriting the above using chain:
chain(df=baseball, count("id"), subset(freq>25), match_df(df, on="id"), head(5))
}
\author{
  Peter Meilstrup
}

