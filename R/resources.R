##Evaluates each argument in turn.  Each argument, if its result has
##an attribute "release" which is a function, or is of a class that
##defines a method "close" or "release", will be called upon
##conclusion of this function.  Returns the value of the last
##argument, or propagates any exceptions which happened.  All
##functions are evaluated in a local frame. If arguments are named,
##the results will be assigned to variables in the local frame.
with_resources <- function(...) {
  # there is a problem with using ..., in that we have to split it apart in order to
  names <- names(eval(substitute(alist(...))))
  stop("not written")
}

##similar to the above except instead of assigning to local variable,
##each argument that evals to a function handle is then called with
##the result; taking all the previous results as arguments.  Use this
##to pass R CMD CHECK.
with_resources_call <- function() {
  stop("not written")
}

using <- function(...) {
  stop("use with[arg=resource, ...](code ...)")
}

`[.using` <- function(with, ...) function() {
  expressions <- dots_expressions(...)
  x <- dots(...)
  body <- using_body(...)
}

closable <- function()

#a macro-writing macro, maybe? First to gen function template, second
#to make fun?
using_body <- macro(function(...){

})

using_body <- macro({

})

## with | o = options(data=data)
##      | f = file(f)
##      | ( test
##        | test
##        | test
##        )

## `[.macro` <- function(...) {
##   args <- arg_quote(...)
##   macro(function(...) {
##     .(macro)(.(function(..(arg_quote(...)))))
##   })
## }
