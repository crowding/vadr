#' Make R the language you wish R was like.
#'
#' This package implements workalikes for the author's (and perhaps
#' your) favorite features from other languages, making R programs
#' shorter and more expressive. The centerpiece is an efficient
#' facility for Common Lisp-style macros. Built on that are a nice
#' destructuring-bind for parallel assignment of variables, a pipline
#' macro similar to the "->" macro of Clojure, and other utilities.
#'
#' Of note are:
#'
#' \itemize{
#'
#' \item{\code{\link{chain}}, a function for increasing code clarity and
#' concision by carrying the output of one function into the input of the next,
#' akin to UNIX shell pipelines, or the \code{->} macro of Clojure.}
#'
#' \item{\code{\link{bind}}, an R-ish implementation of
#' "destructuring bind" or parallel assignment.}
#'
#' \item{\code{\link{index}}, which provides some "missing"
#' indexing operations on \code{data.frame}s;}
#'
#' \item{\code{\link{qq}} or quasiquote, a more complete version of
#' \code{\link{bquote}};}
#'
#' \item{\code{\link{dots}} which exposes dot lists (\code{\link{...}}) as
#' first class objects with a useful API.}
#'
#' \item{\code{\link{macro}} which helps write syntactic transforming
#' functions and makes them fast;}
#'
#' \item{\code{\link{fun}}, which is the shortest possible way to define a
#' throwaway function.}
#'
#' }
#'
#' @name vadr
#' @docType package
#' @import compiler
#' @author Peter Meilstrup
NULL
