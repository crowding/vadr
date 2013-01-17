chain.body <- function(..., .dwim=TRUE) {
  args <- list(...)
  #ideally we want macro hygeine for names like "_data"
  template({
    `_data` <- .(args[[1]])
    ...(
      lapply(args[-1], function(x) template(`_data` <- .(chain.dwim(x, quote(`_data`)))))
      )
    ammoc(`_data`, rm("_data"))
  })
}

## helper function that "guesses" the correct form of the arguments to
## chain if they do not contain dots.
chain.dwim <- function(expr, dot=quote(.))
  if("." %in% all.names(expr)) {
    substitute.nq(expr, list(.=dot))
  } else {
    switch(mode(expr),
           name=call(as.character(expr),dot),
           call=as.call(c(expr[[1]], dot, as.list(expr[-1]))),
           stop("don't know how to chain into a '", mode(expr), "'"))
  }

#' Chain the output of one expression into the input of another.
#'
#' Many times in R programming you will want to take a dataset and do
#' a sqeuence of simple things to it. \code{chain} aims to make this
#' kind of code simpler and more compact. For instance, suppose that
#' you have a path \code{P} defined by a M-by-2 array of coordinates
#' and you want to find the total length of the line segments
#' connecting each point in sequence. You could write:
#'
#' \code{length <- sum(sqrt(rowSums(apply(P, 2, diff).^2)))}
#'
#' However this must be read "inside-out" to follow the
#' computation. It will be easier to read if written this way:
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
#' any function.
#'
#' If the argument \code{.dwim} is set to FALSE, arguments are only
#' interpreted as expressions with placeholders. Thus we could write:
#'
#' \code{length <- chain(P, apply(.,2,diff), .^2, rowSums(.), sqrt(.), sum(.))}
#'
#' Note that nested invocations of \code{chain} will almost never do
#' what you want, as the placeholder will be interpreted by the outer
#' chain before being interpreted by the inner chain. A facility for
#' specifying the placeholder may be added in the future.
#'
#' It is also permissible to use an assignment like \code{x <-
#' func(.)} to store an intermediate result along the chain. The
#' assignment will happen in the function's private scope.
#'
#' Note that subassignments usually return the rvalue, which is not
#' usually what you want in a chain. Currently you can cope with
#' subassignments by saying things like \code{`[<-`(., index,
#' value)}. Some better way to cope with subassignments is desirable.
#'
#' @param data The data to run through the chain.
#' @param ... A sequence of expressions.
#' @param .dwim If set to FALSE, will only evaluate expressions
#' literally. If set to \code{TRUE} (the default), will attempt to
#' guess what you mean. when you leave the dot out of the
#' expressions, as described above.
#' @param .envir The environement that should be parent of the
#' returned function. Defaults to the parent frame of \code{mkchain}
#' @return For \code{mkchain} return the constructed function. For
#' \code{chain}, apply the chain to the dataset given in the first
#' argument and return the result.
#' @note \code{chain} is a bit like the \code{->} macro of Clojure.
#' @aliases chain
#' @author Peter Meilstrup
#' @export
#' @examples
#' # In help(match_df, package="plyr") there is this example:
#' data(baseball)
#' longterm <- subset(count(baseball, "id"), freq > 25)
#' bb_longterm <- match_df(baseball, longterm, on="id")
#' bb_longterm[1:5,]
#'
#' # Rewriting the above using chain:
#' chain(baseball, count("id"), subset(freq>25), match_df(baseball, ., on="id"), head(5))
mkchain <- macro(function(...)
                 template(function(`_data`)
                          .(chain.body(quote(`_data`), ...))))

##' @export
chain <- macro(chain.body)

