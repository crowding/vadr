#' @include macro.R
NULL

#generate the function for both forms of chain
chain_function <- function(args) function(transforms) {
  var <- as.name(names(args)[[1]])
  transforms <- transforms[!is.missing(transforms)]
  names <- names(transforms) %||% rep("", length(transforms))
  assignments <- qqply(.(var) <- .(chain.dwim(x, var))
                       )(x=transforms, var=rep(list(var), length(transforms)))
  named <- names != ""
  assignments[named] <- qqply(`.(x)` <- .(y)
                              )(x=names[named], y=assignments[named])
  qq(function(.=..(args)) { ..(assignments); .(var) })
}

## helper function that "guesses" the correct form of the arguments to
## chain if they do not contain dots.
chain.dwim <- function(expr, dot=quote(.)) {
  if(as.character(dot) %in% all.names(expr)) {
    expr
  } else {
    switch(mode(expr),
           name={
             if(identical(expr, missing_value())) expr
             else call(as.character(expr),dot)
           },
           call=as.call(c(expr[[1]], dot, as.list(expr[-1]))),
           `function`=as.call(list(expr, dot)),
           stop("don't know how to chain into a '", mode(expr), "'"))
  }
}

#' @name chain
#' @S3method "[" mkchain
#' @rdname chain
#' @usage mkchain[...](...)
#' @param ... Parameters in square brackets give the placeholder name
#' and default arguments.
`[.mkchain` <-
  macro(function(...) {
    args <- do.call(quote_args, list(...)[-1])
    macro(function(...) chain_function(args)(list(...)))
  })

#' @S3method "[" chain
#' @rdname chain
#' @usage chain[...](., ...)
`[.chain` <- macro(function(...) {
  args <- do.call(quote_args, list(...)[-1])
  macro(function(...) {
    transforms <- list(...)
    arg <- transforms[[1]]
    if ((names(transforms[1]) %||% "") != "") {
      transforms[[1]] <- qq(.(as.name(names(transforms)[[1]])) <-
                                  .(as.name(names(args)[[1]])))
    } else {
      transforms <- transforms[-1]
    }
    qq(.(chain_function(args)(transforms))(.(arg)))
  })
})

#' Chain the output of one expression into the input of another.
#'
#' \code{chain} provides a different way to write computations that
#' pass a value through a chain of transformations.
#'
#' For instance, suppose that you have a path \code{P} defined by a
#' M-by-2 array of coordinates and you want to find the total length of
#' the line segments connecting each point in sequence. My stream of
#' thought for this goes something like "okay, take the difference
#' between rows, square, sum along columns, square root, and sum." You
#' could write:
#'
#' \code{length <- sum(sqrt(rowSums(apply(P, 2, diff)^2)))}
#'
#' However this must be read "inside-out" to follow the
#' computation. I find it easier to follow if written this way:
#'
#' \code{length <- chain(P, apply(2,diff), .^2, rowSums, sqrt, sum)}
#'
#' which can be read from left to right, noting that the output of
#' each expression becomes the input of the next.
#'
#' Note that some arguments above are the names of functions, and
#' others are expressions. \code{chain} applies whichever
#' interpretation appears most appropriate: bare words are taken to
#' be functions, expressions containing the placeholder name (by
#' default \code{.}) evaluate to expressions, and expressions that do
#' not contain the placeholder have a placeholder injected at the
#' first argument. Thus \code{apply(2,diff)} is interpreted as
#' \code{apply(.,2,diff)}, with the \code{.} coming from the output
#' of the previous step. This tends to work well because of the
#' typical convention in R of the dataset being the first argument to
#' any function. The above is equivalent to:
#'
#' \code{length <- chain(P, apply(.,2,diff), .^2, rowSums(.), sqrt(.), sum(.))}
#'
#' If you want to keep an intermediate value along the chain for use,
#' you can name the arguments, as in
#'
#' \code{alphabetize <- mkchain(values=., names, order, values[.])}.
#'
#' You can also use a different placeholder than \code{"."} by
#' supplying it in brackets, as in \code{chain[x](x^2, mean, sqrt)}.
#' This is useful for nested invocations of \code{\link{chain}} or if
#' another package has a use for \code{"."}. When used with
#' \code{\link{mkchain}}, you can specify other arguments and
#' defaults, as in \code{mkchain[., pow=2](.^pow, mean, .^(1/pow))}.
#'
#' More than the occasional use of temporary names and alternate
#' placeholder names might indicate \code{chain} is not helping
#' clarity :)
#'
#' Note that subassignments, for example \code{chain(letters, names(.)
#' <- toupper(.))} return the rvalue, which is not usually what you
#' want (here it will return the upcased characters, not the object
#' with upcased names.) Instead use \code{\link{put}}, as in \code{
#' chain(letters, put(., names, toupper(.))}, or even better in this
#' case, \code{chain(letters, \link{inject}(names, toupper))}.
#'
#' @param . For \code{chain} the first parameter in parentheses is the
#' data to run through the chain.
#' @param ... Subsequent parameters in parentheses are
#' function names or calls.
#' @return For \code{mkchain} return the constructed function. For
#' \code{chain}, apply the chain to the dataset given in the first
#' argument and return the result.
#' @seealso put
#' @note \code{chain} is a bit like the \code{->} macro of Clojure,
#' or the \code{|>} operator in Elixir.
#' @aliases chain mkchain [.chain [.mkchain %|>%
#' @author Peter Meilstrup
#' @rdname chain
#' @export
#' @examples
#' # In help("match_df", package="plyr") there is this example:
#' library(plyr)
#' data(baseball)
#'
#' longterm <- subset(count(baseball, "id"), freq > 25)
#' bb_longterm <- match_df(baseball, longterm, on="id")
#' bb_longterm[1:5,]
#'
#' # Rewriting the above using chain:
#' chain(b=baseball, count("id"), subset(freq>25),
#'       match_df(b, ., on="id"), head(5))
mkchain <- function(...) NULL

#' @export
#' @rdname chain
#' @usage chain(., ...)
chain <- function(...) NULL

#' @export
#' @rdname chain
#' @usage . \%|>\% func
#' @param func \code{\%|>\%} is a shortcut for a chain of one step.
`%|>%` <- function(...) NULL

#mkchain(...) is the same as mkchain[.](...)
#which is done by making mkchain itself the value of mkchain[.]
#(setting its class to mkchain)
#Can define but can't actually execute the macro at package build time,
#because it uses a binary that's not linked in yet.
#so have to do it at load time:
.onLoad_chain <- function(libname, pkgname) {
  mkchain <- `[.mkchain`(force, .)
  class(mkchain) <- c("mkchain", class(mkchain))
  mkchain <<- mkchain

  chain <- `[.chain`(force, .)
  class(chain) <- c("chain", class(chain))
  chain <<- chain
  `%|>%` <<- chain
}

