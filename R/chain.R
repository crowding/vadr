## safer "data-like" version of pipe
chain <- function(...,dwim=TRUE) {
   arg.list <- as.list(match.call())
   arg.list[1] <- NULL
   if(dwim) arg.list[-1] <- pipe.dwim(arg.list[-1])
   sym <- gensym(envir=parent.frame())
   arg.list <- lapply(arg.list, substitute.nq, list(.=sym))
   arg.list <- lapply(arg.list, fn(substitute(a <- b, list(a=sym,b=.))))
   pipe.call <- do.call(call, c(list("{", enquote(substitute(. <- NULL, list(.=sym)))),
                                lapply(arg.list, enquote),
                                list(enquote(substitute(remove.returning(.), list(.=sym))))))
   eval.parent(pipe.call)
}

pipe <- chain

## a version that creates a function to be called later. useful in ddply etc.
mkchain <- function(..., dwim=TRUE) {
  arg.list <- as.list(match.call())
  arg.list[1] <- NULL
  if(dwim) arg.list <- pipe.dwim(arg.list)
  arg.list <- lapply(arg.list, substitute.nq, list(.=quote(..DATA)))
  arg.list <- lapply(arg.list, fn(substitute(a<-b, list(a=quote(..DATA),b=.))))
  pipe.call <- do.call(call, c(list("{"), lapply(arg.list, enquote), quote(quote(..DATA))))
  pipe.fn <- substitute(function(..DATA) ., list(.=pipe.call))
  out <- eval.parent(pipe.fn)
  attr(out, "source") <- NULL
  out
}

mkpipe <- mkchain

##this slightly ugly hack is required by the current way that pipe works.
remove.returning <- function(sym) {
  q <- substitute(sym)
  val <- force(sym)
  rm(list=as.character(q), envir=parent.frame())
  val
}

## a "macro-like" version of pipe. Note that in principle I could do
## the substitution once and then remember it, leading to a speed
## increase? Modifying the calling function wherever it is called from?
## replace.parent.call.with()? I think I can guarantee 
pipe.macro <- function(..., dwim=TRUE) {
  arg.list <- match.call()
  if(dwim) arg.list[-1:-2] <- pipe.dwim(arg.list[-1:-2])
  eval.parent(Reduce(function(x, y) substitute.nq(y, list(.=x)), arg.list[-1]))
}

## helper function that "guesses" the correct form of the arguments to
## pipe if they do not contain dots.
pipe.dwim <- function(arg.list) {
  lapply(arg.list,
         function(expr) if("." %in% all.names(expr)) {
           expr
         } else {
           switch(mode(expr),
                  name=call(as.character(expr),quote(.)), 
                  call=as.call(c(expr[[1]], quote(.), as.list(expr[-1]))),
                  stop("don't know how to pipe into a '", mode(expr), "'"))
         })
}
