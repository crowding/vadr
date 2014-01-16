#' @include macro.R
NULL

# 


#' Augmented assignment.
#'
#' Assigns to a variable the result of a transformation applied to that
#' variable.
#' @param x A variable to assign to,
#' @param y An expression, interpreted in the manner of \code{\link{chain}}.
#' @return The rvalue of the assignment.
#' @rdname augment
#' @aliases %<~%
#' @usage x %<~% y
#' @author Peter Meilstrup
#' @seealso chain put alter
#' @export
#' @examples
#' library(stringr)
#' x <- 'a'
#' x %<~% toupper
#' x %<~% str_dup(3)
#' x %<~% rep(3)
#' x %<~% .[2:3]
`%<~%` <- macro(function(x, y) {
  if ('.' %in% all.names(y)) {
    qq(.(x) <- (function(.) .(y))(.(x)))
  } else if(is.call(y)) {
    qq(.(x) <- .(y[[1]])(.(x), ..(as.list(y)[-1])))
  } else {
    qq(.(x) <- .(y)(.(x)))
  }
})

#' Modify part of a value.
#'
#' The macro \code{put} provides syntax for modifying part of an
#' object in a functional context (i.e. creating a modified value
#' without necessarily modifying without binding the result to a
#' name.) Unlike \code{\link{<-}}, the value of the expression is the
#' modified object, not the value that was injected. This is
#' particularly useful in combination with 'chain.'
#'
#' Normal subassignment in R is effectively a macro, one which turns a
#' statement like
#'
#' \code{names(x)[1] <- "head"}
#'
#' into something like
#'
#' \code{x <- `names<-`(x, `[<-`(names(x), "head", 1))}
#'
#' However even this explanation is misleading, because the value
#' returned from a subassignment is the value applied, not the value
#' assigned. Consider if you wanted to call a function with a
#' modification of an existing value:
#'
#' \code{do_something(names(x)[1] <- "head")}
#'
#' Aside from changing the value of \code{x} this actually doesn't
#' pass the value to \code{do_something}, but rather performs the
#' equivalent of \code{do_something("head")}.
#'
#' In this situation, using \code{put}, one can write:
#'
#' \code{do_something(put(x, names[1], "head"))}
#'
#' code{put} and friends are particularly useful in conjunction with
#' \code{\link{chain}}.
#'
#' @rdname modifying
#' @param it A value.
#' @param subset A subassignment target expression; this is
#' interpreted literally if the symbol \code{it} is used, otherwise
#' \code{it} is injected as far down the leftmost arguments of the
#' expression as possible. (Thus \code{names} is interpreted as
#' \code{names(it)}, and \code{names[1]} as \code{names(it)[1]}.)
#' @param value The value to assign
#' @return The modified value.
#' @author Peter Meilstrup
#' @seealso chain %<~%
#' @aliases put
#' @usage put(it, subset, value)
#' @export
#' @examples
#' put(1:10, names, letters[1:10])
#' x <- 1:10
#' put(x, it[1], 4)
#' put(x, names[4], 'a')
#' x #x is unchanged
put <- macro(function(it, subset, value) {
  if (nargs()==2) {
    target <- assignment_target(it)
    # Some of my projects use this 2-argument syntax. Not officially supported.
    qq( (function(`.(target)`) {
      .(it) <- .(subset);
      .(target)
    })( .(target) ))
  } else {
    qq( (function(it)
         {
           .(address_expand(quote(it), subset)) <- .(value)
           it
         }
         )( .(it) )
       )
  }
})

#' a title line here is ignored but expected by roxygen
#'
#' \code{alter} takes the selected subset of \code{it},
#' then filters it through additional functions in the manner of
#' \code{\link{chain}}, then replaces the subset with the result,
#' returning the modified object.
#'
#' \code{x \%<~\% alter(names[5], toupper)} is equivalent to:
#'
#' \code{names(x)[5] <- toupper(names(x)[5])}
#' @rdname modifying
#' @aliases alter
#' @usage alter(it, subset, ...)
#' @param ... A \code{\link{chain}} of code transformations.
#' @export
#' @examples
#' x <- alter(structure(1:10, names=letters[1:10]), names)
#' y <- alter(x, names[5], toupper, str_dup(3))
alter <- macro(function(it, subset, ...) {
  addr <- address_expand(quote(it), subset)
  qq(
    (function(it) {
      .(addr) <-
          .(chain_function(alist(`.`=))(list(...)))(.(addr))
      it
    })( .(it) ))
  ## }
})

#' a title line here is ignored but expected by roxygen
#'
#' \code{inject} takes the entire object, filters it through a chain, then
#' places the result in the specified subset.
#'
#' \code{x <- inject(1:10, names, letters[.], toupper)} is equivalent to:
#'
#' \code{x <- 1:10; names(x) <- toupper(letters[x])}
#'
#' @rdname modifying
#' @usage inject(it, subset, ...)
#' @aliases inject
#' @export
#' @examples
#' x <- inject(1:10, names[1:5], letters[.], rev)
inject <- macro(function(it, subset, ...) {
  addr <- address_expand(quote(it), subset)
  qq(
    (function(it) {
      .(addr) <-
          .(chain_function(alist(`.`=))(list(...)))(it)
      it
    })( .(it) ))
})

address_expand <- function(arg, address) {
  if (as.character(arg) %in% all.names(address)) {
    address
  } else {
    address_inject <- function(x) {
      if (is.call(x)) {
        x <- as.list(x)
        if (length(x) >= 2) {
          as.call(c(x[1], address_inject(x[[2]]), x[c(-1,-2)]))
        }
        else {
          as.call(c(as.list(x), list(arg)))
        }
      } else {
        if (!is.language(x)) {
          stop("that doesn't look like an assignment target")
        }
        as.call(list(x, arg))
      }
    }
    address_inject(address)
  }
}

assignment_target <- function(x) {
  switch(
      class(x),
      name=x,
      character=as.name(x),
      call = switch(class(x[[1]]),
                    name = assignment_target(x[[2]]),
                    character = assignment_target(x[[2]]),
                    stop("that doesn't look like an assignment target")),
      stop("that doesn't look like an assignment target"))
}
