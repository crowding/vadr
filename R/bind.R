##' Unpack a list and assign to multiple variables.
##'
##' This is a "destructuring bind" for R. It can be used to unpack
##' lists into different values, or achieve the effect of multiple
##' return values from a function.
##'
##' @examples
##' # For a simple example,
##' bind(a=x, b=y) <- c(a="foo", b="bar")
##' x # -> "foo"
##' y # -> "bar"
##'
##' # Note that the assigned-to variable is on the _right_ side of each
##' # equals in the arglist. This is admittedly awkward but is the best
##' # way to remain consistent with R's argument-binding semantics.
##'
##' # Element to variable matching happens according to R's argument
##' # binding rules, can be nested, and complicated assignment targets
##' # can be used. The following shows off a complicated unpacking:
##'
##' bind[a=x, .rest=bind[aa=xx, bb=yy], b=y, cc=bind[a=zz[2], b=zz[1]]] <-
##'   list(a=1, b="two", aa="eleven", bb=22, cc=c(a=1, b=2))
##' x
##' y
##' xx
##' yy <- 22
##' zz <- c(2,1)
##'
##' # x <- 1; y <- "two", xx <- "eleve
##'
##' @param ...
##' @param .envir The environment to bind in (defaults to the caller).
##' @aliases bind<- [<-.bind <-.bind <<-.bind, [<<-.bind
##' @return The list that was originally unpacked.
##' @author Peter Meilstrup
##' @export
bind <- "use bind[a=x, b=y] <- c(a=1,b=2) to do parallel assignment."
class(bind) <- "bind"

##' @export
`[.bind` <- function(tmp) {
  stop("bind[...] must be used as the target of an assignment.");
}

#' @export
`[<-.bind` <- function(`*temp*`, ..., .envir=parent.frame()) {
  ##for instance this could be written
  ## bind[...=eIn, eOut] <- eval(substitute(alist(...)))
  eOut <- eval(substitute(alist(...)))
  eIn <- eOut[[length(eOut)]]
  eOut[length(eOut)] <- NULL

  vIn <- eval(eIn, .envir)
  nOut <- if(is.null(names(eOut))) rep("", length(eOut)) else names(eOut)

  vOut <- bind_match(nOut, vIn)

  for (i in seq(len=length(nOut))) {
    to <- eOut[[i]]
    if (!missing(to)) {
      eval(substitute(target <- value,
                      list(target=to, value=vOut[[i]])),
           .envir)
    }
  }

  #a side effect is that this creates a variable named "bind" in local
  #workspace.
  `*temp*`
}

bind_match <- function(nOut, vIn) {
  ##Match according to name, and compute the values to assign to the outputs.

  ##First, match all names.
  i_in_out <- pmatch(nOut, names(vIn))

  if (any(is.na(i_in_out) & !(nOut %in% c("", "...")))) {
    stop(sprintf("no matches found for %s",
                 paste("\"",
                       nOut[any(is.na(i_in_out) & !(nOut %in% c("", "...")))],
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

  vOut <- as.list(vIn[i_in_out])

  #then put the rest into dots.
  if (!is.null(i) && nOut[i_out_unmatched[i]] == "...") {
    i_out_unmatched <- which(is.na(i_in_out) & nOut %in% c("", "..."))
    i_in_unmatched <- na.omit(`[<-`(seq(length(vIn)), i_in_out, NA))
    if (length(i_out_unmatched) > i) {
      stop("")
    }
    vOut[[i_out_unmatched]] <- vIn[i_in_unmatched]
  } else {
    if (any(!is.na(`[<-`(seq(length(vIn)), i_in_out, NA)))) {
      stop("More items supplied than names to bind")
    }
  }

  vOut
}
