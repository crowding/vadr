##' A very compact way to define a function.
##'
##' \code{fun} captures its first argument unevaluated and turns it
##' into a function. Every name used in the expression becomes an
##' argument, unless it looks like a function call. If you don't
##' intend to capture a particular variable, you can not provide it,
##' and it will use a default value that pulls from the enclosing
##' scope. For example:
##'
##' \code{ > f <- fun(x/y)
##' > f(10,2)
##' [1] 5
##' > f
##' function (x = eval(quote(x),<environment>),
##'           y = eval(quote(y),<environment>) {
##'   x/y
##' }
##'
##' "\code{fun}" is used with "\code{dm_ply}" the way that "\code{with}" is
##' used with  "\link[plyr]{d_ply}".
##'
##' "\code{...}" is supported in the function definitions and should
##' behave as you expect.
##'
##' @param expr The expression to use as the function's body.
##'
##' @param .all.names Defaults to \code{FALSE}, in which case the
##' formal arguments of the function are only the parts of the
##' expression that "look like" variables (i.e. names that do not head
##' calls). If set to \code{TRUE}, all symbols are wrapped in formal
##' arguments. This includes all things that R treats as calls, like
##' \code{`+`}, \code{`(`} and \code{`{`}. Note that "in the order of
##' appearance" means in the s-expression order, so that
##' \code{"fun(x+y, .all.names=TRUE)"} will have arguments named "+",
##' "x" and "y" in that order.
##'
##' @return A newly constructed function.
##'
##' @note Since it doesn't know which symbols you intend to be
##' arguments and which you intend to take from the enclosing
##' environment, it captures all symbols in defaults; therefore it
##' won't work as a closure that reflects changes in the enclosing
##' environment.
##'
##' @author Peter Meilstrup
##'
##' @export
fun <- function(expr, .all.names=FALSE, .envir=arg_env(expr, environment())) {
  f <- do.call("funmacro", list(substitute(expr), as.logical(.all.names)))
  environment(f) <- .envir
  f
}

#this should be de-extracted and summarizer/mutator/etc ought to be
#deleted in the next pass.
funmacro <- macro(function(expr, .all.names) {
  names <- all.names(expr, functions=.all.names, unique=TRUE)
  funmacro <-
      qq(function(
        .=..( qqply(
          `.(name)`=eval(quote(`.(name)`), parent.env(environment()))
          )(name=names) )
        ) .(expr) )
})

##' Create a function of several arguments returning a data frame.
##'
##' For example, mutate(a=b/c, b=a+mean(c)) returns a function taking
##' arguments 'a', 'b', 'c', and ellipsis, and returns a data frame
##' with columns "a", "b", "c" and any others provided in ...
##'
##' The contents of the arguemnts determine the new function's
##' arguments in the same way that \link{fun} does. The arguments are
##' evaluated in order. Any symbols not provided in the environment
##' will be taken from the data frame.
##'
##' \code{fsummarise} is intended to be used with \link{dm_ply} in a
##' similar way that \code{summarize} is used with \link[plyr]{d_ply}
##'
##' @param ... A series of named arguments.
##' @param .envir The environment to create the function in; defaults
##' to the caller.
##' @return The newly created function.
##' @seealso mutator fun dm_ply
##' @aliases summarizer
##' @export
##' @author Peter Meilstrup
summariser <- function(..., .envir=parent.frame()) {
  summary_exprs <- eval(substitute(alist(...)));
  varnames <- names(summary_exprs)
  outnames <- varnames

  #prefix variables used for missing names with '_missing_ to avoid
  #collisions (?)
  if (is.null(varnames)) {
    varnames <- rep("", length(summary_exprs))
    outnames <- varnames
  }
  missing_names <- varnames == ""
  if (any(missing_names)) {
    names <- unname(unlist(lapply(match.call(expand.dots = FALSE)$...,
                                  deparse)))
    outnames[missing_names] <- names[missing_names]
    varnames[missing_names] <-
      paste('_missing_', outnames[missing_names], sep="")
  }

  #cook up a function
  bind_list <- lapply(varnames, as.name)
  names(bind_list) <- outnames
  bind_cmd <- substitute(plyr::quickdf(x),
                         list(x=as.call(c(as.name("list"), bind_list))))
  summary_command <- mapply(function(name, x)
                            substitute(name <- x, list(name=as.name(name), x=x)),
                            varnames, summary_exprs)
  body <- as.call(c(list(quote(`{`)), summary_command, list(bind_cmd)))
  #use the same variable-capturing semantics as "fun"
  f <- do.call("fun", list(body, .envir=.envir))
  #but strip all the "_missing_s". We might want to strip the vars
  #that only appear as lvalues too?
  formals(f)[varnames[missing_names]] <- NULL
  f
}

##' @export
summarizer <- summariser

##' Create a function of several arguments returning a data frame.
##'
##' For example, mutate(a=b/c, b=a+mean(c)) returns a function taking
##' arguments 'a', 'b', 'c', and ellipsis, and returns a data frame
##' with columns "a", "b", "c" and any others provided in ...
##'
##' The contents of the arguemnts determine the new function's
##' arguments in the same way that \link{fun} does. The arguments are
##' evaluated in order. Any symbols not provided in the environment
##' will be taken from the data frame.
##'
##' The function \code{fmutate} is intended to be used with
##' \link{dm_ply} in a similar way that \code{mutate} is used with
##' \link[plyr]{d_ply}
##'
##' @param ... A series of named arguments.
##' @param .envir The environment to create the function in; defaults
##' to the caller.
##' @return The newly created function.
##' @seealso summariser fun dm_ply
##' @export
##' @author Peter Meilstrup
mutator <- function(..., `_envir`=parent.frame()) {
  stop("not written")
}
