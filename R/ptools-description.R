#' Make R the language you wish R was like.
#'
#' This package implements workalikes for the author's (and perhaps
#' your) favorite features from other languages, making R programs
#' shorter and more expressive. The centerpiece is an efficient
#' facility for macro-like functions. Built on that are a nice
#' destructuring-bind for parallel assignment of variables, a pipline
#' macro similar to the "->" macro of Clojure, and other utilities.
#'
#' Of note are:
#'
#' \itemize{
#'
#' \item{\code{\link{chain}}, a function for saving typing by carrying the
#' output of one function into the input of the next, akin to UNIX
#' shell pipelines, the arrow monad of Haskell or the \code{->} macro
#' of Clojure.}
#'
#' \item{\code{\link{dbind}}, an R-ish implementation of
#' "destructuring bind" or parallel argument assignment.}
#'
#' \item{\code{\link{index}}, which provides some "missing"
#' indexing operations on \code{data.frame}s;}
#'
#' \item{\code{\link{fun}}, which is the shortest possible way to define a
#' throwaway function;}
#'
#' \item{\code{\link{dm_ply}} which meshes with the \code{\link{plyr}}
#' package to provide a faster alternative to ddply that is still
#' intuitive and compact.}
#'
#' }
#'
#' @name ptools
#' @docType package
#' @import plyr codetools
#' @author Peter Meilstrup
NULL
