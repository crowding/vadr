##' @export
bind <- function(..., .envir=parent.frame()) {
  print(match.args())
  print(match.call())
  #nothing
}

##' @export
`bind<-` <- function(..., .envir=parent.frame()) {
  print(match.args())
  print(match.call())
  #nothing
}
