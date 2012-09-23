##' Tools for compact programming.
##'
##' This package collects a few otherwise unrelated tools around the
##' theme "these things make my code shorter."
##'
##' Of note are:
##'
##' \itemize{
##'
##' \item \link[chain], a function for saving typing by carrying the
##' output of one function into the input of the next, akin to UNIX
##' shell pipelines, the arrow monad of Haskell or the \code{->} macro
##' of Clojure.
##'
##' \item \link[dbind], an R-ish implementation of "destructuring bind" or
##' parallel argument assignment.
##'
##' \item \link[index], which provides some "missing"
##' indexing operations on \code{data.frame}s;
##'
##' \item \link[fun], which is the shortest possible way to define a
##' throwaway function;
##'
##' \item \link[dm_ply] which meshes with the \link[plyr] package to
##' provide intuitive and compact, but ultimately faster ways of
##' split-apply-combine operations on data frames.
##'
##' }
##'
##' @name ptools
##' @docType package
##' @import plyr codetools
##' @author Peter Meilstrup
NULL
