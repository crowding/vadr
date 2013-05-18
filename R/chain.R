#generate the function y both forms of chain
chain_function <- function(args) {function(transforms) {
  var <- as.name(names(args)[[1]])
  names <- names(transforms) %||% ""
  template(function(.=...(args)) {
    ...(
      Map(transforms, names, f=function(a,n)
          if (n == "") template(.(var) <- .(chain.dwim(a, var)))
          else template(.(as.name(n)) <- .(var) <- .(chain.dwim(a, var))))
      )
    .(var)
  })
}}

## helper function that "guesses" the correct form of the arguments to
## chain if they do not contain dots.
chain.dwim <- function(expr, dot=quote(.)) {
  if(as.character(dot) %in% all.vars(expr)) {
    expr
  } else {
    switch(mode(expr),
           name=call(as.character(expr),dot),
           call=as.call(c(expr[[1]], dot, as.list(expr[-1]))),
           stop("don't know how to chain into a '", mode(expr), "'"))
  }
}

#' @S3method "[" mkchain
`[.mkchain` <-
  macro(function(...) {
    args <- do.call(quote_args, list(...)[-1])
    macro(function(...) chain_function(args)(list(...)))
  })

#' @S3method "[" chain
`[.chain` <- macro(function(...) {
  args <- do.call(quote_args, list(...)[-1])
  macro(function(...) {
    transforms <- list(...)
    arg <- transforms[[1]]
    if ((names(transforms[1]) %||% "") != "") {
      transforms[[1]] <- template(.(as.name(names(transforms)[[1]])) <-
                                  .(as.name(names(args)[[1]])))
    } else {
      transforms <- transforms[-1]
    }
    template(.(chain_function(args)(transforms))(.(arg)))
  })
})

#' Chain the output of one expression into the input of another.
#'
#' Many times in R programming you will want to take a dataset and do
#' a sequence of simple things to it. \code{chain} aims to make this
#' kind of code simpler and more compact.
#'
#'
#' For instance, suppose that you have a path \code{P} defined by a
#' M-by-2 array of coordinates and you want to find the total length of
#' the line segments connecting each point in sequence. My stream of
#' though for this goes something like "okay, take the difference
#' along columns, square, sum along rows, square root, and sum." You
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
#' \code{alphabetize <- mkchain(values=., names, order, values[.])}.
#'
#' You can also use a different placeholder than \code{"."} by
#' supplying it in brackets, as in \code{chain[x](x^2, mean, sqrt)}.
#' This can make things less confusing for nested invocations
#' of \code{\link{chain}} or if another package has a use for
#' \code{"."}. When used with \code{link{mkchain}}, you can specify
#' other arguments and defaults, as in
#' \code{mkchain[., pow=2](x^pow, mean, sqrt)}
#'
#' Note that using subassignments, for example
#' \code{chain(x, names(.) <- toupper(.))} usually return the rvalue,
#' which is not usually what
#' you want (here it will return the upcased characters, not the object with
#' upcased names.) Currently you can cope with subassignments by saying
#' things like \code{mkchain(data, `names<-`(., toupper(names(.))))}.
#' Some better way to cope with subassignments would be nice.
#'
#' @param . For \code{chain} the first parameter is the data to run
#' through the chain.
#' @param ... The remainder of parameters in \code{chain} are a
#' sequence of transforming expressions.
#' @return For \code{mkchain} return the constructed function. For
#' \code{chain}, apply the chain to the dataset given in the first
#' argument and return the result.
#' @note \code{chain} is a bit like the \code{->} macro of Clojure.
#' @aliases mkchain [.chain [.mkchain
#' @author Peter Meilstrup
#' @export
#' @examples
#' # In help("match_df", package="plyr") there is this example:
#' data(baseball)
#'
#' longterm <- subset(count(baseball, "id"), freq > 25)
#' bb_longterm <- match_df(baseball, longterm, on="id")
#' bb_longterm[1:5,]
#'
#' # Rewriting the above using chain:
#' chain(baseball, count("id"), subset(freq>25),
#'       match_df(baseball, ., on="id"), head(5))
mkchain <- function(...) NULL

#' @export
chain <- function(...) NULL

#mkchain(...) is the same as mkchain[.](...)
#which is done by making mkchain itself the value of mkchain[.]
#(setting its class to mkchain)
#Can define but can't actually execute the macro at package build time,
#so have to do it at load time:
.onLoad <- function(libname, pkgname) {
  mkchain <- `[.mkchain`(force, .)
  class(mkchain) <- c("mkchain", class(mkchain))
  mkchain <<- mkchain

  #' @export
  chain <- `[.chain`(force, .)
  class(chain) <- c("chain", class(chain))
  chain <<- chain
}
