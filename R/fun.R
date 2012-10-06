## this is the name that gets represents the "default" of an ellipsis
## in a formal arguments list. I don't know how else to get it;
## as.character(empty.name) is "" but as.name("") and quote(``) both
## complain of zero length variable names.  It is wrapped in its own
## accessor function because otherwise roxygen chokes on it!
empty.name <- function() formals(function(...)list())$...

##' A very compact way to define a function. Generally, you can write
##' \code{fun(x+y)} where you would have written \code{function(x,y) x+y}.
##'
##' "\code{fun}" is used with "\code{dm_ply}" the way that "\code{with}" is
##' used with  "\link[plyr]{d_ply}".
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
##' @family function creation
##' @author Peter Meilstrup
##'
##' @export
fun <- function(..., .all.names=FALSE, .envir=parent.frame()) {
  require(codetools)
  exprs <- eval(substitute(alist(...)))
  if (length(exprs) > 1) {
    expr <- substitute({...});
  } else {
    expr <- substitute(...)
  }
  if (.all.names) {
    varnames <- all.names(expr, unique=TRUE)
  } else {
    varnames <- all.names(expr, functions=FALSE, unique=TRUE)
  }
  args <- lapply(varnames,
                 #should "env" be parent.env(environment())?
                 function(x) substitute(eval(quote(x), env),
                                        list(x=as.name(x), env=.envir)))
  names(args) <- varnames
  if("..." %in% varnames) {
    args$... <- empty.name()
  }
  f <- eval(substitute(function() expr, list(expr=expr)))
  attr(f, "srcref") <- NULL
  formals(f) <- args
  environment(f) <- .envir
  f
}

##' Create a function of several arguments returning a data frame.
##'
##' For example, mutate(a=b/c, b=a+mean(c)) returns a function taking
##' arguments 'a', 'b', 'c', and ellipsis, and returns a data frame
##' with columns "a", "b", "c" and any others provided in \code{"..."}.
##'
##' \code{fsummarise} is intended to be used with \link{dm_ply} in a
##' similar way that \code{summarize} is used with \link[plyr]{d_ply}
##'
##' @template fun
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
    names <- unname(unlist(lapply(match.call(expand = FALSE)$...,
                                  deparse)))
    outnames[missing_names] <- names[missing_names]
    varnames[missing_names] <-
      paste('_missing_', outnames[missing_names], sep="")
  }

  #cook up a function
  bind_list <- lapply(varnames, as.name)
  names(bind_list) <- outnames
  bind_cmd <- substitute(quickdf(x),
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

##' Create a function of several arguments returning a data frame. The
##' function evaluates all expressions with respect to all arguments
##' given, then returns the data frame
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
##' @template fun
##' @seealso summariser fun dm_ply
##' @export
##' @author Peter Meilstrup
mutator <- function(..., `_envir`=parent.frame()) {
  #plyr's "mutate" doesn't do what summarise did about making up new
  #column names is none are provided, so in mimicry of that, out
  #mutator simpler. But one difference is that mutate() doesn't
  #evaluate the unnamed arguments arguments, but this does. (you might
  #want to insert a debugging print() after all, or use <- instead of
  #==)
  eIn <- eval(substitute(alist(...)));
  nIn <- names(eIn)

  if (is.null(nIn)) {
    nIn <- rep("", length(eIn))
  }
  eOut <- eIn
  for (i in seq_along(eIn)) {
    if (nIn[[i]] != "") {
      eOut[[i]] <- substitute(out <- expr,
                            list(out=as.name(nIn[[i]]), expr=eIn[[i]]))
    }
  }

  #we call fun() on this body, just to get the right formal arguments.
  body1 <- as.call(c(quote({})[[1]], eIn, quote(quickdf(...))))
  f <- do.call("fun", list(body1, .envir=`_envir`))

  #but the actual body we use is a bit different.
  nOut <- setdiff(union(nIn, names(formals(f))), "")
  nOut <- structure(lapply(nOut, as.name), names=nOut)
  names(nOut)[names(nOut)=="..."] <- ""
  retcall <- as.call(c(quote(list),nOut))
  retcall <- as.call(c(quote(quickdf), retcall))
  body2 <- as.call(c(quote({})[[1]], eOut, retcall))
  body(f) <- body2
  f
}
