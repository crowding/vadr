##' Tools for compact programming.
##'
##' This package collects a few otherwise unrelated tools around the
##' theme of compact coding -- things that save you keystrokes.
##'
##' Of note are:
##'
##' \itemize{
##'
##' \item{\code{\link{chain}}, a function for saving typing by carrying the
##' output of one function into the input of the next, akin to UNIX
##' shell pipelines, the arrow monad of Haskell or the \code{->} macro
##' of Clojure.}
##'
##' \item{\code{\link{dbind}}, an R-ish implementation of
##' "destructuring bind" or parallel argument assignment.}
##'
##' \item{\code{\link{index}}, which provides some "missing"
##' indexing operations on \code{data.frame}s;}
##'
##' \item{\code{\link{fun}}, which is the shortest possible way to define a
##' throwaway function;}
##'
##' \item{\code{\link{dm_ply}} which meshes with the
##' \code{\link{plyr}} package to provide intuitive, compact, and fast
##' ways of split-apply-combine operations on data frames.}
##'
##' }
##'
##' @name ptools
##' @docType package
##' @import plyr codetools
##' @author Peter Meilstrup
NULL
