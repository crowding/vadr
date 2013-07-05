#' Unpack a list and assign to multiple variables.
#'
#' This is a "destructuring bind" for R. It can be used to unpack
#' structured lists into different variables, or achieve the effect of
#' multiple return values from a function.
#'
#' Element to variable matching should match R's argument binding
#' rules, with the modification that arguments to the right of the
#' ... will be matched positionally to elements at the end of the
#' unpacked sequence. Calls to bind() can be nested to unpack nested
#' structures.
#'
#' You may leave an argument blank as in \code{bind[, skipKey=,
#' ...=rest] <- seq} to skip an element. (Here the first element of
#' \code{seq} and the one tagged "skipKey" are both skipped and the
#' rest are gathered in the output variable \code{rest}.)
#'
#' Note that the assigned-to variable is on the \emph{right} side of
#' each \code{=} in the argument list. This is admittedly awkward but
#' is the best way to remain consistent with R's argument-binding
#' semantics.
#'
#' @usage bind[key=varName, ...] <- list(key=value, ...)
#' @name bind
#' @format N/A
#' @examples
#' #match by position
#' bind[x, y] <- c("foo", "bar")
#'
#' #match by name
#' bind[a=x, b=y] <- c(b="bar", a="foo")
#'
#' # one often wants to unpack the first and/or last, and rest of a list.
#' bind[first, ...=rest, last] <- letters
#'
#' record <- list("Marilyn", "Monroe", dob=list("June", 1, 1926),
#'                profession="film star", "born Norma Jean Baker",
#'                donotuse="garbage", "1947 California Artichoke Queen",
#'                list("August", 5, 1962))
#' bind[first, last,
#'      dob=bind[month, day, year],
#'      donotuse=, ...=notes, death] <- record
#'
#' @note This will incidentally create a local variable named "bind"
#' in your environment. On the other hand if you have an object
#' already named "bind" and not of class "bind" this method won't be
#' found, so it's merely annoying and not destructive. It's not
#' clear how to avoid this and still use an assignment operator to do
#' the binding. (I could write a simple function, but I strongly
#' prefer there to be a \code{<-} anywhere that there is a
#' modification to the environment.)
#'
#' Nonlocal assignments (\code{<<-}) are not supported and will behave
#' as local assignments.
#'
#' @param ... a list of assignments, key on left, target variable on
#' right. That is, \code{bind[a=x] <- c(a=1)} creates a variable named
#' \code{x}, not \code{a}. It is somewhat counterintuitive but this is
#' the only way that matches R's argument binding syntax.
#' @param *envir* The environment to bind in (defaults to the caller).
#' @aliases bind bind<- [<-.bind <-.bind
#' @method "[<-" bind
#' @return a "bind" object, since it is invoked via a subset on "bind".
#' @author Peter Meilstrup
#' @export
#' @S3method "[<-" bind
`[<-.bind` <- function(`*temp*`, ..., `*envir*`=parent.frame()) {
  #why square brackets?
  #1. I want there to be a <- everywhere there is a change to the workspace.
  #2. we can't simply have
  #bind(x=a, y=b) <- c(x=1,y=2) because R mangles
  #`<-`(bind(x=a,y=b), c(1,2) into
  #`bind<-`(`*tmp*`, c(x=1, y=2), y=b)
  #which erases the fact that you wanted to match to the key "x".
  #using `{<-` would allow more flexible syntax but won't work on account
  #of more involved mangling.

  ##for instance the next three lines could be written
  ## bind[...=eIn, eOut] <- eval(substitute(alist(...)))
  eOut <- eval(substitute(alist(...)))
  eIn <- eOut[[length(eOut)]]
  eOut[length(eOut)] <- NULL

  vIn <- eval(eIn, `*envir*`)
  nOut <- if(is.null(names(eOut))) rep("", length(eOut)) else names(eOut)

  vOut <- bind_match(nOut, vIn)

  for (i in seq(len=length(nOut))) {
    to <- eOut[[i]]
    if (!missing(to)) {
        expr <- quote(a <- quote(b))
        expr[[2]] <- to
        expr[[3]][[2]] <- vOut[[i]]
        eval(expr, `*envir*`)
    }
  }

  #a side effect is that R creates a variable named "bind" in local
  #workspace.
  `*temp*`
}

bind_match <- function(nOut, vIn) {
  ##Match according to name, and compute the values to assign to the outputs.

  ##You know, this might be a whole lot easier if I didn't support
  ##partial matching.

  ##First, match all names.
  i_in_out <- pmatch(nOut, names(vIn))

  if (any(is.na(i_in_out) & !(nOut %in% c("", "...")))) {
    stop(sprintf("no matches found for %s",
                 paste("\"",
                       nOut[is.na(i_in_out) & !(nOut %in% c("", "..."))],
                       "\"", sep="", collapse=", ")))
  }
  #From the front, assign inputs to outputs until you hit "..."
  i_out_unmatched <- which(is.na(i_in_out) & nOut %in% c("", "..."))
  i_in_unmatched <- na.omit(`[<-`(seq(length(vIn)), i_in_out, NA))
  for (i in seq(len=length(i_in_unmatched))) {
    if (i > length(i_out_unmatched)) {
      stop("Not enough items to bind")
    }
    if (nOut[i_out_unmatched[i]] == "...") {
      break
    }
    i_in_out[i_out_unmatched[i]] <- i_in_unmatched[i]
  }

  #same from the back
  i_out_unmatched <- rev(which(is.na(i_in_out) & nOut %in% c("", "...")))
  i_in_unmatched <- rev(na.omit(`[<-`(seq(length(vIn)), i_in_out, NA)))
  for (i in seq(len=length(i_in_unmatched))) {
    if (i > length(i_out_unmatched)) {
      stop("Not enough items to bind")
    }
    if (nOut[i_out_unmatched[i]] == "...") {
      break
    }
    i_in_out[i_out_unmatched[i]] <- i_in_unmatched[i]
  }

  #data.frame objects choke on selecing columns with NAs, so...
  vOut <- vIn[rep(1, length(nOut))]
  positional <- !is.na(i_in_out)
  if (any(positional)) {
    vOut[positional] <- as.list(vIn[i_in_out[positional]])
  }

  #then put the rest into dots.
  if (!is.null(i) && nOut[i_out_unmatched[i]] == "...") {
    i_out_unmatched <- which(is.na(i_in_out) & nOut %in% c("", "..."))
    i_in_unmatched <- na.omit(`[<-`(seq(length(vIn)), i_in_out, NA))
    if (length(i_out_unmatched) > i) {
      stop("This should not happen")
    }
    vOut[[i_out_unmatched]] <- vIn[i_in_unmatched]
  } else {
    if (any(!is.na(`[<-`(seq(length(vIn)), i_in_out, NA)))) {
      stop("More items supplied than names to bind")
    }
  }

  vOut
}

#' @export
bind <- "bind"
class(bind) <- "bind"

#' @S3method print bind
print.bind <- function(...)
  invisible(cat("Use bind[a=x, b=y] <- c(a=1,b=2) to do parallel assignment.\n\n"))

##' @export
`[.bind` <- function(tmp) {
  stop("bind[...] must be used as the target of an assignment.");
}
