
##' @section Function Application:
##' The columns of \code{.data} will be matched against the named
##' arguments of the function \code{.fun}. Any arguments that do not
##' match names will be ignored. For example, if \code{.data} has
##' columns "X", "Y", and "Z", but \code{.fun} only has arguments "X"
##' and "Y", then only "X" and "Y" will be used.  If \code{.fun} has
##' an ellipsis argument, all columns of the data frame will be used.
##'
##' The functions \code{fun}, \code{mutator} and \code{summariser}
##' provide some convenient shortcuts to make multiple-argument worker
##' functions. In most cases you can replace d*ply calls using
##' \code{link{with}}, \code{link{summarize}} and \code{link{mutate}}
##' with equivalent calls using \code{link{fun}},
##' \code{link{summarizer}} and \code{link{mutator}}.
##'
##' @param .data data frame to be processed
##' @param .variables variables to split data frame by, as quoted
##'   variables, a formula or character vector
##' @param .drop should combinations of variables that do not appear in the
##'   input data be preserved (FALSE) or dropped (TRUE, default)
##' @param .parallel if \code{TRUE}, apply function in parallel, using parallel
##'   backend provided by foreach
##' @family data frame input
##' @param .fun multiple argument function to apply to each piece.
##' @param ... other arguments passed on to \code{.fun}
##' @param .progress name of the progress bar to use, see
##'   \code{\link{create_progress_bar}}
##' @keywords manip
##' @references Hadley Wickham (2011). The Split-Apply-Combine Strategy 
##'   for Data Analysis. Journal of Statistical Software, 40(1), 1-29. 
##'   \url{http://www.jstatsoft.org/v40/i01/}.   
##'
##' @seealso fun mutator summariser
##' @family multiarg worker
