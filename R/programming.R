#mostly an un-exported collection of juink in here.

#' Evaluate the first argument; if null, evaluate and return the
#' second argument.
#'
#' @param a The first argument to evaluate.
#' @param b The second argument to evaluate. Only evaluated if A evaluates to
#' NULL.
#' @return the value of \code{a} if not null, else \code{b}
#' @author Peter Meilstrup
#' @name shortcutting-or
#' @aliases %||%
#' @export "%||%"
`%||%` <- function(a, b) if(is.null(a)) b else a

pad.missing.cells <- function(data, factors) {
  ##add a row containing NA for each combination of factors that is not represented.
  ##This is a workaround for a bug in current reshape / ggplot.
  chain( factors
       , combinations <- llply(., function(x)unique(data[,x,drop=FALSE]))
       , llply(nrow)
       , llply(seq)
       , do.call(expand.grid, .)
       , mapply(function(x, y) x[y,,drop=FALSE], combinations, .)
       , do.call(cbind,.)
       , merge(data, all.x=TRUE)
       )
}

mutate.where <- function(x, subset, ...) {
  ##a combination of mutate and subset.
  ##mutate those rows where subset evaluates to true, returning the entire modified data frame.
  r <- eval(arg_expr(subset), x, arg_env(subset))
  if (!is.logical(r))
    stop("'subset' must evaluate to logical")
  r <- r & !is.na(r)
  cols <- dots(...)
  cols <- cols[names(cols) != ""]
  .data <- x[r,]
  assignments <- unpack(cols)
  exprs <- expressions(cols)
  mapply(function(expr, env, name) {
    .data[[name]] <<- eval(cols[[name]], .data, env)
  })
  for (col in names(cols)) {
    x[r,col] <- .data[,col]
  }
  x
}

keep.if <- function(x, expr, enclos=arg_env(expr, environment())) {
  ##keep a subset if the expression evaluates to true. Use with ddply.
  force(enclos)
  if (eval(substitute(expr), x, enclos))
    x
  else
    x[c(),, drop=FALSE]
}

#fake out command line scripts for debugging...
run.command <- function(command) {
  blargs <- strsplit(command, ' ')[[1]]
  trace(commandArgs, at=3, tracer=substitute(args <-  c("--slave","--args", blargs[-1]), list(blargs=blargs)))
  on.exit(untrace(commandArgs))
  source(blargs[[1]])
}

prefixing.assign <- function(prefix='', l=list(), env=arg_env(l, environment())) {
  force(env)
  for (n in names(l)) {
    assign(paste(prefix,n,sep=""),eval(substitute(l$n,list(n=n))),envir=env)
  }
}

almost.unique <- function(values, thresh = 0.0001) {
  values <- sort(values, na.last=TRUE)
  index <- chain(values, diff, . > thresh, cumsum, c(0,.))
  tapply(values, index, mean)
}

cluster.to.unique <- function(values, thresh=0.0001) {
  chain(values,
       .[ord <- order(.)],
       diff, .>thresh, cumsum, c(0,.),
       tapply(values[ord], ., function(x) {x[] <- mean(x); x}),
       unlist,
       .[inverse.permutation(ord)])
}

inverse.permutation <- function(perm) {
  ##if X is a vector expressing a permutation, for example the output
  ##of ORDER(), returns the inverse of that permutation.
  perm[perm] <- 1:length(perm)
  return(perm)
}

## As we can never remember how to use "substitute" on a non-quoted expression.
## I don't think this use of do.call is officially supported but it seems to work.
substitute.nq <- function(expr,...) {
  envir <- arg_env(expr, environment())
  do.call(substitute, list(expr,...), envir=envir)
}

load.as.list <- function(...) {
  a <- environment()
  load(envir=a, ...)
  as.list(a)
}
